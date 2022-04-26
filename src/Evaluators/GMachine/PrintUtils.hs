module Evaluators.GMachine.PrintUtils
  ( showTrace
  , showOutput
  ) where

import           Evaluators.GMachine.State

import           Alloc
import           Iseq
import           Language

showOutput :: [GmState] -> String
showOutput trace = iDisplay $ showNode state a node
 where
  state = last trace
  a : _ = gmStack state
  node  = hLookup (gmHeap state) a

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
showState state =
  iConcat [showStack state, iNewline, showInstructions $ gmCode state, iNewline]

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
showNode _ _ (NInd a) = iConcat [iStr "Ind ", showAddr a]

showStats :: GmState -> Iseq
showStats state =
  iConcat [iStr "Steps taken = ", iNum $ statGetSteps $ gmStats state]
