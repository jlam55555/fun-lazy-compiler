module Exercises.PrettyPrintInfix
  ( exercise1_8
  , infix_exs
  ) where

import           Iseq
import           Language
import           PrettyPrint

-- should give:  x + y < p * length xs
exercise1_8 :: IO ()
exercise1_8 = putStrLn $ iDisplay $ pprExpr $ EAp
  (EAp (EVar "<") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
  (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))

-- Other examples
infix_exs :: IO ()
infix_exs = putStrLn $ iDisplay $ pprProgram $ map
  (\expr -> ("test", [], expr))
  [
    -- should give:  x + y < p * length xs
    EAp (EAp (EVar "<") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
        (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))

    -- should give:  ( x + y ) * p * length xs
  , EAp (EAp (EVar "*") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
        (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))

    -- should give:  ( x * y ) * p * length xs
  , EAp (EAp (EVar "*") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y")))
        (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))

    -- should give:  f ( x + y ) ( p * length xs )
  , EAp (EAp (EVar "f") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
        (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))

    -- should give:  h ( g ( f x ) )
  , EAp (EVar "h")                  (EAp (EVar "g") (EAp (EVar "f") (EVar "x")))

  -- should give:  h g f x
  , EAp (EAp (EAp (EVar "h") (EVar "g")) (EVar "f")) (EVar "x")

  -- should give:  h g ( f x )
  , EAp (EAp (EVar "h") (EVar "g")) (EAp (EVar "f") (EVar "x"))

  -- should give:  ( 1 - 2 ) - 3
  , EAp (EAp (EVar "-") (EAp (EAp (EVar "-") (ENum 1)) (ENum 2))) (ENum 3)

  -- should give:  1 - ( 2 - 3 )
  , EAp (EAp (EVar "-") (ENum 1))   (EAp (EAp (EVar "-") (ENum 2)) (ENum 3))

  -- should give:  ( 1 + 2 ) + 3
  -- (note: + is right-associative)
  , EAp (EAp (EVar "+") (ENum 1))   (EAp (EAp (EVar "+") (ENum 2)) (ENum 3))

  -- should give:  1 + 2 + 3
  , EAp (EAp (EVar "+") (EAp (EAp (EVar "+") (ENum 1)) (ENum 2))) (ENum 3)

  -- should give:  ( ( * ) + ( + ) ( + ) ) ( + ) (which is nonsensical)
  , EAp (EAp (EAp (EVar "+") (EVar "*")) (EAp (EVar "+") (EVar "+"))) (EVar "+")
  ]
