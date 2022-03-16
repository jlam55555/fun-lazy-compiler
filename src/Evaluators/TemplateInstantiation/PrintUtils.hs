module Evaluators.TemplateInstantiation.PrintUtils
  ( showResults
  ) where

import           Alloc
import           Iseq
import           Utils

import           Evaluators.TemplateInstantiation.Node
import           Evaluators.TemplateInstantiation.State
import           Evaluators.TemplateInstantiation.Statistics

-- Format the result of `eval` for printing.
-- TODO: move this to a different module
showResults :: [TiState] -> String
showResults states =
  iDisplay $ iConcat [iLayn $ showState <$> states, showStats $ last states]

-- Exercise 2.5. Modify `showState` to print out heap.
showState :: TiState -> Iseq
showState (s, _, h, _, _) =
  iConcat [showStack h s, iNewline, showHeap h, iNewline]

showStack :: TiHeap -> TiStack -> Iseq
showStack h s = iConcat
  [ iStr "stack ["
  , iIndent $ iInterleave iNewline $ showStackItem <$> s
  , iStr " ]"
  ]
 where
  showStackItem a =
    iConcat [showFWAddr a, iStr ": ", showStkNode h $ hLookup h a]

-- Exercise 2.5. Modify `showState` to print out heap.
showHeap :: TiHeap -> Iseq
showHeap h = iConcat
  [ iStr "heap  ["
  , iIndent $ iInterleave iNewline $ showItem <$> hAddresses h
  , iStr " ]"
  ]
  where showItem a = iConcat [showFWAddr a, iStr ": ", showNode $ hLookup h a]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode h (NAp fnAddr argAddr) = iConcat
  [ iStr "NAp "
  , showFWAddr fnAddr
  , iStr " "
  , showFWAddr argAddr
  , iStr " ("
  , showNode $ hLookup h argAddr
  , iStr ")"
  ]
showStkNode _ node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) =
  iConcat [iStr "NAp ", showAddr a1, iStr " ", showAddr a2]
showNode (NSupercomb name _ _) = iStr $ "NSupercomb " ++ name
showNode (NNum n             ) = iStr "NNum " `iAppend` iNum n
showNode (NInd a             ) = iStr "NInd " `iAppend` showAddr a
showNode (NPrim name _       ) = iStr $ "NPrim " ++ name
showNode (NData t    as      ) = iConcat
  [ iStr "NData "
  , iNum t
  , iStr " ["
  , iIndent $ iInterleave iNewline $ showAddr <$> as
  , iStr " ]"
  ]

showAddr :: Addr -> Iseq
showAddr a = iStr $ show a

-- Show address in field of width 4
showFWAddr :: Addr -> Iseq
showFWAddr a = iStr $ space (4 - length str) ++ str where str = show a

showStats :: TiState -> Iseq
showStats (_, _, h, _, stats) = iConcat
  [ iNewline
  , iStr "stats ["
  , iIndent $ iInterleave iNewline $ printStat <$> statLabelFns
  , iStr " ]"
  , iNewline
  ]
 where
  printStat (label, stat) = iConcat [iStr $ label ++ ": ", iNum stat]
  statLabelFns =
    [ ("Total number of steps     ", tiStatGetSteps $ stats)
    , ("Primitive reductions      ", tiStatGetPrimitiveReductions $ stats)
    , ("Supercombinator reductions", tiStatGetSupercombinatorReductions $ stats)
    , ("Maximum stack depth       ", tiStatGetMaxStackDepth $ stats)
    , ("Heap allocs               ", hStatsGetAllocs $ h)
    , ("Heap updates              ", hStatsGetUpdates $ h)
    , ("Heap frees                ", hStatsGetFrees $ h)
    ]
