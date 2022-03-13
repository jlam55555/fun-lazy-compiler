-- In haskell-interactive-mode, use M-x haskell-session-change-target
-- to switch to the flc-test target
module Main
  ( main
  ) where

import           Test.HUnit

import           AllocTests
import           Evaluators.TemplateInstantiation.EvaluatorTests
import           IseqTests
import           LanguageTests
import           LexerTests
import           NameSupplyTests
import           Parser.CoreTests
import           Parser.SubparserTests
import           ParserTests
import           PrettyPrintTests
import           UtilsTests

main :: IO Counts
main = runTestTT $ test
  [ "Alloc" ~: AllocTests.tests
  , "Evaluators.TemplateInstantiation.EvaluatorTests"
    ~: Evaluators.TemplateInstantiation.EvaluatorTests.tests
  , "Iseq" ~: IseqTests.tests
  , "Language" ~: LanguageTests.tests
  , "Lexer" ~: LexerTests.tests
  , "NameSupply" ~: NameSupplyTests.tests
  , "Parser.Subparser" ~: Parser.SubparserTests.tests
  , "Parser.Core" ~: Parser.CoreTests.tests
  , "Parser" ~: ParserTests.tests
  , "PrettyPrint" ~: PrettyPrintTests.tests
  , "UtilsPrint" ~: UtilsTests.tests
  ]
