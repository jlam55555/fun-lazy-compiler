module IseqTests
  ( tests
  ) where

import           Iseq
import           Test.HUnit

tests :: Test
tests = test
  [ -- iNil
    "iNil" ~: "" ~=? iDisplay iNil
  , -- iNum
    "iNum(523)" ~: "523" ~=? iDisplay (iNum 523)
  , -- iFWNum
    "iFWNum(5, 523)" ~: "  523" ~=? iDisplay (iFWNum 5 523)
  , "iFWNum(1, 523)" ~: "523" ~=? iDisplay (iFWNum 1 523)
  , -- iLayn
    "iLayn(5,\"hello\")" ~: "   1) 5\n   2) hello\n" ~=? iDisplay
    (iLayn [iNum 5, iStr "hello"])
  , -- iStr
    "iStr(\"hello!\")" ~: "hello!" ~=? iDisplay (iStr "hello!")
  , "iStr(\"hi\\nworld\\nbye\")"
  ~:  iDisplay (iStr "hi\nworld\nbye")
  ~=? "hi\nworld\nbye"
  , -- iAppend
    "iAppend(\"hello \",iAppend(5,\" world\"))" ~: "hello 5 world" ~=? iDisplay
    (iAppend (iStr "hello ") $ iAppend (iNum 5) (iStr " world"))
  , -- iNewline
    "iNewline" ~: "\n" ~=? iDisplay iNewline
  , -- iIndent
    "-- iIndent(\"hi\nworld\")" ~: "-- hi\n   world" ~=? iDisplay
    (iConcat [iStr "-- ", iIndent (iStr "hi\nworld")])
  , "complex iIndent"
  ~:  "i1 @i1\n   @i1:i2 @i2\n          @i2 more text\n   @i1\n@i0"
  ~=? iDisplay
        (iConcat
          [ iStr "i1 "
          , iIndent $ iConcat
            [ iStr "@i1\n@i1:i2 "
            , iIndent $ iStr "@i2\n@i2"
            , iStr " more text"
            , iNewline
            , iStr "@i1"
            ]
          , iNewline
          , iStr "@i0"
          ]
        )
  , -- iConcat
    "iConcat []" ~: "" ~=? iDisplay (iConcat [])
  , "iConcat [\"hello \", 5, \" world\"]"
  ~:  iDisplay (iConcat [iStr "hello ", iNum 5, iStr " world"])
  ~=? "hello 5 world"
  , -- iInterleave
    "iInterleave \",\" []" ~: "" ~=? iDisplay (iInterleave (iStr ",") [])
  , "iInterleave \"\\n\" [\"hello\",\"world\"]" ~: "hello\nworld" ~=? iDisplay
    (iInterleave (iStr ",") [iStr "hello", iStr "world"])
  ]
