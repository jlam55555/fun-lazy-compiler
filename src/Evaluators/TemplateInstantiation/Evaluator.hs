module Evaluators.TemplateInstantiation.Evaluator
  ( run
  , runShowResult
  , runGetResult
  , runGetNumResult
  , runGetBoolResult
  , runShowResultSimple
  , getResult
  , compile
  , eval
  , showResults
  , showDataNode
  ) where

import           Data.AssocList

import           Alloc
import           CorePrelude
import           Language
import           Parser
import           Utils

import           Evaluators.TemplateInstantiation.Node
import           Evaluators.TemplateInstantiation.PrintUtils
import           Evaluators.TemplateInstantiation.State
import           Evaluators.TemplateInstantiation.Statistics

-- Simply run program
run :: String -> [TiState]
run = eval . compile . parse

-- Driver for the graph reduction implementation;
-- runs program and shows result
runShowResult :: String -> String
runShowResult = showResults . run

-- Get data from program result
getResult :: [TiState] -> Node
getResult states = hLookup h s
  where -- Non-exhaustive pattern should not fail because `eval` should always
        -- return a singleton stack (the single element containing the result)
        ([s], _, h, _, _) = last states

-- Get result of running program
runGetResult :: String -> Node
runGetResult = getResult . run

-- Get numeric result; useful for tests
runGetNumResult :: String -> Int
runGetNumResult = toNum . runGetResult
 where
  toNum (NNum n) = n
  toNum _        = error "runGetNumResult: not a numeric result"

-- Get boolean result; useful for tests
runGetBoolResult :: String -> Bool
runGetBoolResult = toNum . runGetResult
 where
  toNum b | b == trueNode  = True
          | b == falseNode = False
          | otherwise      = error "runGetBoolResult: not a boolean result"

-- Print simple form of answer
runShowResultSimple :: String -> String
runShowResultSimple = showDataNode . runGetResult

-- Translate the program into a form suitable for execution
compile :: CoreProgram -> TiState
compile program =
  (initialStack, initialTiDump, initialHeap, globalEnv, tiStatInitial)
 where
  -- `program` is reversed so that later definitions override earlier ones
  scDefs = (reverse program) ++ preludeDefs ++ extraPreludeDefs
  (initialHeap, globalEnv) = buildInitialHeap scDefs
  initialStack = [addressOfMain]
  addressOfMain =
    lookupDef (error "compile: main is not defined") "main" globalEnv

-- Initial heap comprises an address for each supercombinator
buildInitialHeap :: CoreProgram -> (TiHeap, TiEnv)
buildInitialHeap scDefs = (h', scAddrs ++ primAddrs)
 where
  (h , scAddrs  ) = mapAccuml allocateSc hInitial scDefs
  (h', primAddrs) = mapAccuml allocatePrim h primitives

-- Add a builtin supercombinator to the heap
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc h (name, args, body) = (h', (name, a))
  where (h', a) = hAlloc h $ NSupercomb name args body

-- Add primitives to the heap
primitives :: AssocList Name Primitive
primitives =
  [ ("negate"  , Neg)
  , ("+"       , Add)
  , ("-"       , Sub)
  , ("*"       , Mul)
  , ("/"       , Div)
  , ("if"      , If)
  , (">"       , Greater)
  , (">="      , GreaterEq)
  , ("<"       , Less)
  , ("<="      , LessEq)
  , ("=="      , Eq)
  , ("~="      , NotEq)
  , ("casePair", CasePair)
  , ("caseList", CaseList)
  , ("abort"   , Abort)
  ]

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim h (name, prim) = (h', (name, a))
  where (h', a) = hAlloc h $ NPrim name prim

-- Execute the program by performing repeated state transitions
-- until a final state is reached. The result is the list of all
-- states passed through.
-- Exercise 2.9: This formulation is more useful than the suggested
-- one because it will still show the final state if there is an
-- error thrown in `tiFinal` (empty stack error)
eval :: TiState -> [TiState]
eval state = state : restStates
 where
  restStates | tiFinal state = []
             | otherwise     = eval nextState
  nextState = (doAdmin . step) state

-- Test if a state is final
-- Exercise 2.16: not final if the dump is empty
tiFinal :: TiState -> Bool
tiFinal ([soleAddr], [], h, _, _) = isDataNode $ hLookup h soleAddr
tiFinal ([]        , _ , _, _, _) = error "tiFinal: empty stack"
tiFinal _ = -- stack contains more than one element or the dump is not empty
  False

-- Check if a Node is a data object rather than a redex
-- (supercombinator or application)
isDataNode :: Node -> Bool
isDataNode (NNum _   ) = True
isDataNode (NData _ _) = True
isDataNode _           = False

-- Update statistics on TiState on an evaluation step
doAdmin :: TiState -> TiState
doAdmin state = applyToStats updateFn state
 where
  updateFn =
    tiStatIncSteps
      . tiStatUpdateReductions (hLookup h $ head s)
      . tiStatUpdateMaxStackDepth (length s)
  (s, _, h, _, _) = state

-- Maps a state to its successor following the stated evaluation rules
step :: TiState -> TiState
step state = dispatch $ hLookup h $ head s
 where
  (s, _, h, _, _) = state
  dispatch (NNum n                 ) = numStep state n
  dispatch (NAp a1 a2              ) = apStep state a1 a2
  dispatch (NSupercomb sc args body) = scStep state sc args body
  dispatch (NInd a                 ) = indStep state a
  dispatch (NPrim _   prim         ) = primStep state prim
  dispatch (NData tag args         ) = dataStep state tag args

-- Number may only appear on the stack spine if the current
-- stack is final; in Mark 4, this will further evaluate if the
-- dump is not empty
-- Exercise 2.16: Change this to implement Equation 2.7
numStep :: TiState -> Int -> TiState
numStep ([_], s : d, h, e, stats) _ = (s, d, h, e, stats)
numStep _ _ = error "numStep: number applied as function"

-- Exercise 2.21: Similar to above
dataStep :: TiState -> Int -> [Addr] -> TiState
dataStep ([_], s : d, h, e, stats) _ _ = (s, d, h, e, stats)
dataStep _ _ _ = error "dataStep: structured data applied as function"

-- Step when an application node is reached: add the function to the spine
-- Exercise 2.16: updated to implement Equation 2.8
apStep :: TiState -> Addr -> Addr -> TiState
apStep (s, d, h, e, stats) a1 a2 = nextState $ hLookup h a2
 where
  nextState (NInd a2') = (s, d, h', e, stats)
   where
    h'    = hUpdate h a $ NAp a1 a2'
    a : _ = s
  nextState _ = (a1 : s, d, h, e, stats)

-- Step when a supercombinator node is reached: unwind the stack
-- and instantiate the sc with the environment (globals + args)
-- Exercise 2.6. If there are not enough stack elements, throw
-- a suitable error message.
scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (s, d, h, e, stats) _ argNames body = (s', d, h', e, stats)
 where
  -- stack update: remove sc and args from stack, replace node
  s'@(rootAddr : _) = checkArgCount $ drop argsToDrop s
   where
    checkArgCount s''@(_ : _) = s''
    checkArgCount _ = error "scStep: not enough arguments for application"
  argsToDrop  = length argNames
  h'          = instantiateAndUpdate body rootAddr h e'
  -- Exercise 2.8: order matters here; if it were reversed, the
  -- outside environment would override the new bindings
  e'          = argBindings ++ e
  argBindings = zip argNames $ getArgs h s

-- Indirection node: equation 2.4; the indirection node gets
-- replaced with the address on the stack.
-- Exercise 2.13: Implement indirection nodes.
indStep :: TiState -> Addr -> TiState
indStep (s, d, h, e, stats) a = (a : tail s, d, h, e, stats)

-- Primitives, introduced in Mark 4
-- Exercise 2.16: unary negation operator
-- Exercise 2.17: binary arithmetic operators (+, -, *, /)
primStep :: TiState -> Primitive -> TiState
primStep state Neg                = primNeg state
primStep state Add                = primArith state (+)
primStep state Sub                = primArith state (-)
primStep state Mul                = primArith state (*)
primStep state Div                = primArith state div
primStep state Greater            = primComp state (>)
primStep state GreaterEq          = primComp state (>=)
primStep state Less               = primComp state (<)
primStep state LessEq             = primComp state (<=)
primStep state Eq                 = primComp state (==)
primStep state NotEq              = primComp state (/=)
primStep state (Constr tag arity) = primConstr state tag arity
primStep state If                 = primIf state
primStep state CasePair           = primCasePair state
primStep state CaseList           = primCaseList state
primStep state Abort              = primAbort state

-- Exercise 2.16: evaluation of `negate` (the only unary primitive)
primNeg :: TiState -> TiState
primNeg (s, d, h, e, stats) = state'
 where
  -- Non-exhaustive pattern match; if there are not enough arguments, it would
  -- be caught by the `getArg` matching below
  state' | -- Equation 2.5: Argument is already evaluated
           isDataNode arg = (s', d, h', e, stats)
         | -- Equation 2.9: Create new stack, push old one onto the dump
           otherwise      = (s'', d', h, e, stats)
  -- Stack, heap, and n if argument is already evaluated
  s'@[rootAddr] = tail s
  h'            = hUpdate h rootAddr $ NNum $ -n
  NNum n        = arg
  -- Stack and dump if argument is not evaluated
  s''           = argAddrs
  d'            = s' : d
  -- Get singular argument
  argAddrs      = getArgs h s
  arg           = hLookup h $ getArg argAddrs
   where
    -- Rule 2.5: there should only be a single argument
    getArg [argAddr] = argAddr
    getArg _         = error "primNeg: wrong number of arguments to `negate`"

-- Exercise 2.17: evaluation of binary primitive operators
primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state fn = primBinary state fn'
 where
  fn' (NNum n1) (NNum n2) = NNum $ fn n1 n2
  fn' _ _ =
    error "primComp: relational operators applied to non-integer values"

primComp :: TiState -> (Int -> Int -> Bool) -> TiState
primComp state fn = primBinary state fn'
 where
  fn' (NNum n1) (NNum n2) = boolToNode $ fn n1 n2
  fn' _ _ =
    error "primComp: relational operators applied to non-integer values"

-- Exercise 2.21: Generalize `primArith` (and also make it easy to define
-- `primComp`. I don't like the term "dyadic function" -- "binary" seems
-- to be the more commonplace term.
primBinary :: TiState -> (Node -> Node -> Node) -> TiState
primBinary (s, d, h, e, stats) fn = state' where
  state' | isDataNode arg1 && isDataNode arg2 = (s', d, h', e, stats)
         | otherwise                          = (s''', d', h, e, stats)
  -- Stack, heap, and ns if arguments are already evaluated
  [_, _, rootAddr]      = s
  s'                    = [rootAddr]
  h'                    = hUpdate h rootAddr $ fn arg1 arg2
  -- Stack and dump if either argument is not evaluated;
  -- Note: both arguments are re-evaluated, even if one is already
  -- evaluated; this simplifies the implementation a little bit
  s''                   = [a1]
  s'''                  = [a2]
  d'                    = s'' : s' : d
  -- Get both arguments
  argAddrs@[a1  , a2  ] = getArgs h s
  [         arg1, arg2] = hLookup h <$> getTwoArgs argAddrs
   where
    getTwoArgs args@[_, _] = args
    getTwoArgs _ =
      error "primArith: wrong number of arguments to binary primitive"

-- Exercise 2.21: implement application of constructor primitive.
-- We don't need to force of the arguments here, which makes this simpler
-- than the other `prim*` functions.
primConstr :: TiState -> Int -> Int -> TiState
primConstr (s, d, h, e, stats) tag arity = state'
 where
  state'
    | length argAddrs == arity
    = (s', d, h', e, stats)
    | otherwise
    = error
      $  "primConstr: wrong number of arguments to constructor (expected "
      ++ show arity
      ++ ", got "
      ++ show (length argAddrs)
      ++ ")"
  s'@[rootAddr] = drop arity s
  h'            = hUpdate h rootAddr $ NData tag argAddrs
  argAddrs      = getArgs h s

-- Exercise 2.21: implement application of `if` primitive. We need to
-- force the first argument (the conditional). The implementation of
-- this function is fairly similar to `primNeg` and `primArith`.
-- Note that unlike `primNeg`, `primArith`, and `primConstr`, the result
-- may be a function, so the stack does not need to have exactly
-- (arity + 1) elements.
primIf :: TiState -> TiState
primIf (s, d, h, e, stats) = state' where
  state' | s' == []        = error "primIf: not enough arguments to if"
         | isDataNode cond = (s', d, h', e, stats)
         | otherwise       = (s'', d', h, e, stats)
  -- Stack, heap, and n if conditional is already evaluated
  s'           = drop 3 s
  rootAddr : _ = s'
  h'           = hUpdate h rootAddr $ NInd $ branch
  branch | cond == trueNode  = thenAddr
         | cond == falseNode = elseAddr
         | otherwise         = error "primIf: conditional is not a boolean"
  -- Stack and dump if argument is not evaluated
  s''  = [condAddr]
  d'   = tail s : d
  -- Get arguments
  condAddr : thenAddr : elseAddr : _ = getArgs h s
  cond = hLookup h condAddr

-- Exercise 2.22: implement `casePair` primitive. This is fairly similar
-- to the evaluation of primIf
primCasePair :: TiState -> TiState
primCasePair (s, d, h, e, stats) = state' where
  state' | s' == [] = error "primCasePair: not enough arguments to casePair"
         | isDataNode pair = (s', d, h'', e, stats)
         | otherwise = (s'', d', h, e, stats)
  -- Stack, heap, and n if conditional is already evaluated
  s'                     = drop 2 s
  rootAddr : _           = s'
  (h', ap)               = hAlloc h $ NAp handlerAddr pair1Addr
  h''                    = hUpdate h' rootAddr $ NAp ap pair2Addr
  -- Stack and dump if argument is not evaluated
  s''                    = [pairAddr]
  d'                     = tail s : d
  -- Get arguments
  (pair1Addr, pair2Addr) = getPairAddrs pair
  getPairAddrs (NData tagPair' [pair1Addr', pair2Addr'])
    | tagPair == tagPair' = (pair1Addr', pair2Addr')
    | otherwise = error "primCasePair: argument is not a pair (incorrect tag)"
  getPairAddrs _ = error "primCasePair: argument is not a pair"
  pairAddr : handlerAddr : _ = getArgs h s
  pair                       = hLookup h pairAddr

-- Exercise 2.24: implement `caseList` primitive. This is like a combination
-- of `caseIf` and `casePair`.
primCaseList :: TiState -> TiState
primCaseList (s, d, h, e, stats) = state' where
  state' | s' == [] = error "primCaseList: not enough arguments to caseList"
         | isDataNode lst = (s', d, h', e, stats)
         | otherwise = (s'', d', h, e, stats)
  -- Stack, heap, and n if conditional is already evaluated
  s'           = drop 3 s
  rootAddr : _ = s'
  h'           = updateHeap lst
   where
    updateHeap (NData tag _)
      | tag == tagNil  = updateHeapNil
      | tag == tagCons = updateHeapCons lst
      | otherwise      = error "primCaseList: argument is not a list"
    updateHeap _ = error "primCaseList: argument is not a list"
    updateHeapNil = hUpdate h rootAddr $ NInd f1Addr
    updateHeapCons (NData _ [cons1Addr, cons2Addr]) = h'''
     where
      (h'', ap) = hAlloc h $ NAp f2Addr cons1Addr
      h'''      = hUpdate h'' rootAddr $ NAp ap cons2Addr
    updateHeapCons _ = error "primCaseList: Cons: wrong number of arguments"
  -- Stack and dump if argument is not evaluated
  s''                           = [lstAddr]
  d'                            = tail s : d
  -- Get arguments
  lstAddr : f1Addr : f2Addr : _ = getArgs h s
  lst                           = hLookup h lstAddr

-- Exercise 2.24: Implement primitive `abort`.
primAbort :: TiState -> TiState
primAbort = error "primAbort: abort: fatal error"

-- Looks up all the arguments (names) for NAp nodes on the spine
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs _ []      = error "getArgs: empty stack"
getArgs h (_ : s) = getArg <$> s
 where
  getArg a = arg
    where -- Non-exhaustive pattern should never fail
          NAp _ arg = hLookup h a

-- Instantiate a supercombinator. Takes a sc, heap, and
-- environment (globals + args bindings).
-- (Instantiation is essentially substitution of the environment to
-- evaluate an expression, such as the body of a supercombinator or `let`.)
type TiInst = TiHeap -> TiEnv -> (TiHeap, Addr)

instantiate :: CoreExpr -> TiInst
instantiate (ENum n   ) h _ = hAlloc h $ NNum n
instantiate (EAp e1 e2) h e = hAlloc h'' $ NAp a1 a2
 where
  (h' , a1) = instantiate e1 h e
  (h'', a2) = instantiate e2 h' e
instantiate (EVar v) h e =
  (h, lookupDef (error $ "instantiate: unbound var " ++ show v) v e)
instantiate (EConstr tag arity) h e = instantiateConstr tag arity h e
instantiate (ELet isrec defs body) h e = instantiateLet isrec defs body h e
instantiate (ECase _ _) _ _ = error "instantiate: can't instantiate case exprs"
instantiate (ELam _ _) _ _ = error "instantiate: can't instantiate lambda fns"

instantiateConstr :: Int -> Int -> TiInst
instantiateConstr tag arity h _ = hAlloc h $ NPrim "Constr" $ Constr tag arity

-- Exercise 2.10, 2.11: Implement let(rec)
instantiateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiInst
instantiateLet isRec bindings body h e = instantiate body h' e'
 where
  -- Augmented environment to evaluate body (and bindings in letrec)
  e'             = zip (bindersOf bindings) rhsAddrs ++ e
  (h', rhsAddrs) = mapAccuml instantiateBinding h $ rhssOf bindings
  instantiateBinding h'' body' = instantiate body' h'' e''
   where
    e'' | -- letrec: use augmented environment for bindings
          isRec == recursive = e'
        | -- let: use previous environment for bindings
          otherwise          = e

-- Exercise 2.12: The given program cannot exist in a strongly-typed language
-- since the binding `f = f x` is not well-typed (similar to how the Y-combinator
-- cannot be well-typed).

-- Exercise 2.14: Implement `instantiateAndUpdate`. Same as instantiate, except that
-- the root of the instantiation doesn't allocate a new node. The second argument
-- is the address of the root node to update.
type TiUpdInst = Addr -> TiHeap -> TiEnv -> TiHeap

instantiateAndUpdate :: CoreExpr -> TiUpdInst
instantiateAndUpdate (ENum n   ) updAddr h _ = hUpdate h updAddr $ NNum n
instantiateAndUpdate (EAp e1 e2) updAddr h e = hUpdate h'' updAddr $ NAp a1 a2
 where
  (h' , a1) = instantiate e1 h e
  (h'', a2) = instantiate e2 h' e
-- Exercise 2.14: For `EVar`, we need to have an indirection because the
-- variable's address may be updated at some point. This is the only place
-- where we need to create an `NInd`.
instantiateAndUpdate (EVar v) updAddr h e = hUpdate h updAddr $ NInd a
  where a = lookupDef (error $ "instantiate: unbound var " ++ show v) v e
instantiateAndUpdate (EConstr tag arity) updAddr h e =
  instantiateAndUpdateConstr tag arity updAddr h e
instantiateAndUpdate (ELet isRec bindings body) updAddr h e =
  instantiateAndUpdateLet isRec bindings body updAddr h e
instantiateAndUpdate (ECase _ _) _ _ _ =
  error "instantiateAndUpdate: can't instantiate case"
instantiateAndUpdate (ELam _ _) _ _ _ =
  error "instantiateAndUpdate: can't instantiate lambda fns"

instantiateAndUpdateConstr :: Int -> Int -> TiUpdInst
instantiateAndUpdateConstr tag arity updAddr h _ =
  hUpdate h updAddr $ NPrim "Constr" $ Constr tag arity

-- Exercise 2.14: Note: we need to be careful to recursively call
-- `instantiateAndUpdate` on the body expression but `instantiate` on the
-- bindings expressions to avoid extra indirections
instantiateAndUpdateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiUpdInst
instantiateAndUpdateLet isRec bindings body updAddr h e = instantiateAndUpdate
  body
  updAddr
  h'
  e'
 where
  -- Augmented environment to evaluate body (and bindings in letrec)
  e'             = zip (bindersOf bindings) rhsAddrs ++ e
  (h', rhsAddrs) = mapAccuml instantiateBinding h $ rhssOf bindings
  instantiateBinding h'' body' = instantiate body' h'' e''
   where
    e'' | -- letrec: use augmented environment for bindings
          isRec == recursive = e'
        | -- let: use previous environment for bindings
          otherwise          = e
