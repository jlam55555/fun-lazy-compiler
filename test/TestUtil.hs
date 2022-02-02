-- Utility functions for HUnit tests
module TestUtil
  ( assertError
  ) where

import           Control.Exception
import           Test.HUnit

-- Assertion fails if code does not exit with an exception
-- Based on: https://stackoverflow.com/a/46330966
assertError :: String -> IO a -> Assertion
assertError desc a = do
  errored <- catch (a >> pure False)
                   ((const $ pure True) :: SomeException -> IO Bool)
  if errored then pure () else assertFailure $ "Did not fail: " ++ desc
