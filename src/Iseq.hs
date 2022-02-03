module Iseq
  ( Iseq
  , iNil
  , iNum
  , iFWNum
  , iLayn
  , iStr
  , iAppend
  , iNewline
  , iIndent
  , iDisplay
  , iConcat
  , iInterleave
  ) where

import           Data.List.Split
import           Utils

-- Iseq has two advantages over strings:
-- - Linear time append characteristic (basically defer all concatenation
--   operations until printing)
-- - Has operations for controlling newlines and indentation
data Iseq = INil
  | IStr String
  | IAppend Iseq Iseq
  | IIndent Iseq
  | INewline

-- Empty iseq
iNil :: Iseq
iNil = INil

-- Integer to iseq
iNum :: Int -> Iseq
iNum = IStr . show

-- Fixed width integer to iseq
iFWNum :: Int -> Int -> Iseq
iFWNum width n = iStr $ space (width - length digits) ++ digits
  where digits = show n

-- Lays out a list
iLayn :: [Iseq] -> Iseq
iLayn seqs =
  iConcat
    $   (\(n, iseq) -> iConcat [iFWNum 4 n, iStr ") ", iIndent iseq, iNewline])
    <$> zip [1 ..] seqs

-- Turn a string into an iseq.
-- Exercise 1.7. Replace newlines with INewline
iStr :: String -> Iseq
iStr s = iInterleave iNewline $ IStr <$> splitOn "\n" s

-- Append two iseqs
-- Exercise 1.5. Simplify if operand is INil
iAppend :: Iseq -> Iseq -> Iseq
iAppend INil seq2 = seq2
iAppend seq1 INil = seq1
iAppend seq1 seq2 = IAppend seq1 seq2

-- New line with indentation
iNewline :: Iseq
iNewline = INewline

-- Indent an iseq
-- (Currently ignoring indentation)
iIndent :: Iseq -> Iseq
iIndent iseq = IIndent iseq

-- Turn an iseq into a string
iDisplay :: Iseq -> String
iDisplay iseq = flatten 0 [(iseq, 0)]

-- Helper for iDisplay: concatenate in roughly linear time
-- (Reduces right-associatively.)
-- First argument is the current indentation level.
-- Each element in the worklist contains its indentation level.
-- Exercise 1.6. Write rules for INil, IStr, IAppend with
-- correct indenting.
flatten :: Int -> [(Iseq, Int)] -> String
flatten _   []                   = ""
flatten col ((INil  , _) : seqs) = flatten col seqs
flatten col ((IStr s, _) : seqs) = s ++ flatten (col + length s) seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) =
  flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((IIndent iseq, _) : seqs) = flatten col ((iseq, col) : seqs)
flatten _ ((INewline, indent) : seqs) =
  '\n' : space indent ++ flatten indent seqs

-- Concatenate a list of iseqs
-- Exercise 1.2. Write in terms of iAppend and iNil
iConcat :: [Iseq] -> Iseq
iConcat iseqList = foldl iAppend iNil iseqList

-- Interleave an iseq between every element in a
-- list of iseqs
iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _     []             = iNil
iInterleave delim (iseq : iseqs) = foldl concat_term iseq iseqs
  where concat_term acc term = iConcat [acc, delim, term]
