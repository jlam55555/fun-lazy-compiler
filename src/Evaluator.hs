{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Evaluator
  ( Evaluator(..)
  , Gm.GmState
  , Ti.TiState
  ) where

import qualified Evaluators.GMachine           as Gm
import qualified Evaluators.TemplateInstantiation
                                               as Ti

import           Language

-- Typeclass allows for generic evaluator. The evaluator is parameterized
-- on the state type `s`.
class Evaluator s where
  eval :: s -> [s]
  compile :: CoreProgram -> s
  showTrace :: [s] -> String
  showOutput :: [s] -> String
  showCompiled :: s -> String

instance Evaluator Gm.GmState where
  eval         = Gm.eval
  compile      = Gm.compile
  showTrace    = Gm.showTrace
  showOutput   = Gm.showOutput
  showCompiled = Gm.exportAsm

instance Evaluator Ti.TiState where
  eval         = Ti.eval
  compile      = Ti.compile
  showTrace    = Ti.showTrace
  showOutput   = Ti.showOutput
  showCompiled = error "showCompiled: TI has no (useful) compiled IR"
