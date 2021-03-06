{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Evaluators.GMachine.PrintUtils
  ( showTrace
  , showOutput
  ) where

import           Evaluators.GMachine.State

import           Alloc
import           Iseq
import           Language

instance Show Instruction where
  show Unwind           = "unwind"
  show (Pushglobal n)   = "pushglobal " ++ n
  show (Pushint    n)   = "pushint " ++ show n
  show (Push       n)   = "pusharg " ++ show n
  show Mkap             = "mkap"
  show (Update n)       = "update " ++ show n
  show (Pop    n)       = "pop " ++ show n
  show (Alloc  n)       = "alloc " ++ show n
  show (Slide  n)       = "slide " ++ show n
  show Eval             = "eval"
  show Add              = "add"
  show Sub              = "sub"
  show Mul              = "mul"
  show Div              = "div"
  show Neg              = "neg"
  show Eq               = "eq"
  show Ne               = "ne"
  show Lt               = "lt"
  show Le               = "le"
  show Gt               = "gt"
  show Ge               = "ge"
  -- Cond replaced in Mark 6
  -- show (Cond t f)     = iDisplay $ iConcat
  --   [ iStr "cond t"
  --   , iIndent $ shortShowInstructions 3 t
  --   , iNewline
  --   , iStr "     f"
  --   , iIndent $ shortShowInstructions 3 f
  --   ]
  show (Pack t n      ) = "pack " ++ show t ++ " " ++ show n
  show (Casejump rules) = iDisplay $ iConcat
    [ iStr "casejump ["
    , iIndent $ iInterleave iNewline $ showRule <$> rules
    , iStr "]"
    ]
   where
    showRule (t, c) = iConcat [iNum t, iStr " -> ", shortShowInstructions 3 c]
  show (Split n) = "split " ++ show n
  show Print     = "print"

-- Show output in "structured form" (Exercise 3.36); also requires
-- changes in the print opcode
showOutput :: [GmState] -> String
showOutput trace = printTree $ gmOutput state where state = last trace

showTrace :: [GmState] -> String
showTrace states = iDisplay $ iConcat
  [ iStr "Supercombinator definitions"
  , iNewline
  , iInterleave iNewline $ showSC state <$> gmEnv state
  , iNewline
  , iNewline
  , iStr "State transitions"
  , iNewline
  , iNewline
  , iLayn $ showState <$> states
  , iNewline
  , iNewline
  , showStats $ last states
  ]
  where state : _ = states

showSC :: GmState -> (Name, Addr) -> Iseq
showSC state (f, a) = iConcat
  [ iStr "Code for "
  , iStr f
  , iNewline
  , showInstructions code
  , iNewline
  , iNewline
  ]
  where NGlobal _ code = hLookup (gmHeap state) a

showInstructions :: GmCode -> Iseq
showInstructions is = iConcat
  [ iStr "  Code:{"
  , iIndent $ iInterleave iNewline $ iStr . show <$> is
  , iStr "}"
  , iNewline
  ]

showState :: GmState -> Iseq
showState state = iConcat
  [ showOutput'
  , iNewline
  , showStack state
  , iNewline
  , showDump state
  , iNewline
  , showInstructions $ gmCode state
  , iNewline
  ]
 where
  showOutput' =
    iConcat [iStr "Output: \"\n", iStr $ showOutput [state], iStr "\""]

showStack :: GmState -> Iseq
showStack state = iConcat
  [ iStr " Stack:["
  , iIndent
  $   iInterleave iNewline
  $   showStackItem state
  <$> (reverse $ gmStack state)
  , iStr "]"
  ]

showStackItem :: GmState -> Addr -> Iseq
showStackItem state a =
  iConcat [showAddr a, iStr ": ", showNode state a $ hLookup (gmHeap state) a]

showAddr :: Addr -> Iseq
showAddr = iStr . show

showNode :: GmState -> Addr -> Node -> Iseq
showNode _     _ (NNum n     ) = iNum n
showNode state a (NGlobal _ _) = iConcat [iStr "Global ", iStr f]
  where f = head [ f' | (f', a') <- gmEnv state, a == a' ] -- reverse lookup
showNode _ _ (NAp a1 a2) =
  iConcat [iStr "Ap ", showAddr a1, iStr " ", showAddr a2]
showNode _ _ (NInd a      ) = iConcat [iStr "Ind ", showAddr a]
showNode _ _ (NConstr t as) = iConcat
  [ iStr "Constr "
  , iNum t
  , iStr " ["
  , iInterleave (iStr ", ") $ showAddr <$> as
  , iStr "]"
  ]

showDump :: GmState -> Iseq
showDump state = iConcat
  [ iStr "  Dump:["
  , iIndent $ iInterleave iNewline $ showDumpItem <$> reverse (gmDump state)
  , iStr "]"
  ]

showDumpItem :: GmDumpItem -> Iseq
showDumpItem (is, s) = iConcat
  [iStr "<", shortShowInstructions 3 is, iStr ",", shortShowStack s, iStr ">"]

shortShowInstructions :: Int -> GmCode -> Iseq
shortShowInstructions n is = iConcat
  [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
 where
  codes = iStr . show <$> take n is
  dotcodes | length is > n = codes ++ [iStr "..."]
           | otherwise     = codes

shortShowStack :: GmStack -> Iseq
shortShowStack s =
  iConcat [iStr "[", iInterleave (iStr ", ") $ showAddr <$> s, iStr "]"]

showStats :: GmState -> Iseq
showStats state =
  iConcat [iStr "Steps taken = ", iNum $ statGetSteps $ gmStats state]

-- Inorder to tree
printToTree :: [PrintPreorderNode] -> Maybe PrintTreeNode
printToTree []    = Nothing
printToTree inord = getResult $ printToTree' [(1, PTNStruct (-1) [])] inord
 where
  getResult (Just [(0, PTNStruct (-1) [n])]) = Just $ reverseTree n
  getResult Nothing                          = Nothing
  getResult _                                = error "printToTree: invalid tree"
  reverseTree n@(PTNNum _) = n
  reverseTree (PTNStruct tag children) =
    PTNStruct tag $ reverse $ reverseTree <$> children

-- Helper function for inorder -> tree
printToTree'
  :: [(Int, PrintTreeNode)]
  -> [PrintPreorderNode]
  -> Maybe [(Int, PrintTreeNode)]
-- Done
printToTree' final@[(0, _)] [] = Just final
-- 0 elements left, pop the stack
printToTree' ((0, n) : (arity, PTNStruct tag children) : ppns) inord =
  printToTree' ((arity, PTNStruct tag (n : children)) : ppns) inord
printToTree' ((arity, PTNStruct tag children) : ppns) (n : ns) = printToTree''
  n
 where
  printToTree'' (PPNNum n') =
    printToTree' ((arity - 1, PTNStruct tag (PTNNum n' : children)) : ppns) ns
  printToTree'' (PPNStruct tag' arity') = printToTree'
    ((arity', PTNStruct tag' []) : (arity - 1, PTNStruct tag children) : ppns)
    ns
printToTree' _ _ = Nothing

-- Print tree
printTree :: [PrintPreorderNode] -> String
printTree = toString . printToTree
 where
  toString Nothing     = ""
  toString (Just tree) = iDisplay $ printTree' tree

printTree' :: PrintTreeNode -> Iseq
printTree' (PTNNum n              ) = iNum n
printTree' (PTNStruct tag children) = printNode tag $ length children
 where
  printNode 1 0 = iStr "False"
  printNode 2 0 = iStr "True"
  printNode 3 0 = iStr "Nil"
  printNode 4 2 = regularStruct $ iStr "Cons"
  printNode n _ = regularStruct $ iNum n
  regularStruct tag' = iConcat
    [ tag'
    , iStr "{"
    , iIndent $ iInterleave (iStr ",\n") $ printTree' <$> children
    , iStr "}"
    ]
