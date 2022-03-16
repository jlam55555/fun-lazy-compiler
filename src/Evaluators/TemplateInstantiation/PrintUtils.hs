module Evaluators.TemplateInstantiation.PrintUtils
  ( showTrace
  , showOutputNode
  ) where

import           Alloc
import           Iseq
import           Utils

import           Evaluators.TemplateInstantiation.Node
import           Evaluators.TemplateInstantiation.State
import           Evaluators.TemplateInstantiation.Statistics

-- Show the program result simply. Special handling for special
-- structured data (requires heap), and very primitive representation
-- for non-special structured data.
showOutputNode :: TiHeap -> Node -> String
showOutputNode h node = iDisplay $ showOutputNode' h node

showOutputNode' :: TiHeap -> Node -> Iseq
showOutputNode' _ (NNum n) = iNum n
showOutputNode' h node@(NData tag _) | node == trueNode  = iStr "True"
                                     | node == falseNode = iStr "False"
                                     | tag == tagNil     = showNil node
                                     | tag == tagCons    = showCons node
                                     | otherwise         = showData node
 where
  showNil (NData _ []) = iStr "[]"
  showNil _            = error "showOutputNode: node tagged as nil not nil"
  showCons (NData _ [a1, a2]) = iConcat
    [ iStr "("
    , showOutputNode' h $ hLookup h a1
    , iStr " : "
    , showOutputNode' h $ hLookup h a2
    , iStr ")"
    ]
  showCons _ = error "showOutputNode: node tagged as cons not a pair"
  showData (NData _ argAddrs) = iConcat
    [ iStr "<"
    , iNum tag
    , iStr ">( "
    , iIndent
    $   iInterleave iNewline
    $   showOutputNode' h
    .   hLookup h
    <$> argAddrs
    , iStr " )"
    ]
  -- Should not occur; internal error
  showData _ = error "showOutputNode: not a structured data node"
showOutputNode' h (NInd a   ) = showOutputNode' h $ hLookup h a
showOutputNode' h (NAp a1 a2) = iConcat
  [ iStr "("
  , showOutputNode' h $ hLookup h a1
  , iStr " "
  , showOutputNode' h $ hLookup h a2
  , iStr ")"
  ]
showOutputNode' _ (NSupercomb name _ _) = iStr name
showOutputNode' _ (NPrim name _       ) = iStr name

-- Format the result of `eval` (a trace of states) for printing.
showTrace :: [TiState] -> String
showTrace states =
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
