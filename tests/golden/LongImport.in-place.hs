module LongImport where

import Prelude
  ( Bool(False, True), Either(Left, Right), Foldable(elem, foldl, foldr, length)
  , Maybe(Just, Nothing), Num((*), (+), (-), abs, negate, signum)
  , Ord(compare, max, min), Ordering(EQ, GT, LT), Semigroup((<>)), Show(show)
  , ($), (&&), (++), (.), (||), Int, String, all, and, any, break, concat
  , concatMap, const, curry, drop, dropWhile, even, flip, fst, head, id, init
  , last, lines, map, not, notElem, or, otherwise, replicate, reverse, scanl
  , scanr, seq, snd, span, tail, take, uncurry, undefined, unlines, unwords
  , unzip, words, zip
  )

f :: String
f =
  show symbols


use :: a -> ()
use _ =
  ()


symbols :: ()
symbols =
  let
    uInt :: Int
    uInt = undefined

    uInts :: [Int]
    uInts = undefined

    uIntss :: [[Int]]
    uIntss = undefined

    uPairs :: [(Int, Int)]
    uPairs = undefined

    uBools :: [Bool]
    uBools = undefined

    uString :: String
    uString = undefined

    uStrings :: [String]
    uStrings = undefined
  in
    use (id uInt)
      `seq` use (const uInt uInt)
      `seq` use (flip id uInt)
      `seq` use ((.) id id)
      `seq` use (($) id uInt)
      `seq` use (curry fst)
      `seq` use (uncurry (+))
      `seq` use ((+) uInt uInt)
      `seq` use ((-) uInt uInt)
      `seq` use ((*) uInt uInt)
      `seq` use (negate uInt)
      `seq` use (abs uInt)
      `seq` use (signum uInt)
      `seq` use (compare uInt uInt)
      `seq` use (min uInt uInt)
      `seq` use (max uInt uInt)
      `seq` use (not undefined :: Bool)
      `seq` use ((&&) undefined undefined :: Bool)
      `seq` use ((||) undefined undefined :: Bool)
      `seq` use False
      `seq` use True
      `seq` use LT
      `seq` use GT
      `seq` use EQ
      `seq` use (Just uInt)
      `seq` use Nothing
      `seq` use (Left uInt)
      `seq` use (Right uInt)
      `seq` use (fst undefined :: (Int, Int))
      `seq` use (snd undefined :: (Int, Int))
      `seq` use (head uInts)
      `seq` use (tail uInts)
      `seq` use (init uInts)
      `seq` use (last uInts)
      `seq` use (reverse uInts)
      `seq` use (take uInt uInts)
      `seq` use (drop uInt uInts)
      `seq` use (replicate uInt uInt)
      `seq` use (concat uIntss)
      `seq` use (map id uInts)
      `seq` use (foldl (+) uInt uInts)
      `seq` use (foldr (+) uInt uInts)
      `seq` use (uInts ++ uInts)
      `seq` use (zip uInts uInts)
      `seq` use (unzip uPairs)
      `seq` use (length uInts)
      `seq` use (elem uInt uInts)
      `seq` use (notElem uInt uInts)
      `seq` use (and uBools)
      `seq` use (or uBools)
      `seq` use (any even uInts)
      `seq` use (all even uInts)
      `seq` use (concatMap (:[]) uInts)
      `seq` use (scanl (+) uInt uInts)
      `seq` use (scanr (+) uInt uInts)
      `seq` use (dropWhile even uInts)
      `seq` use (span even uInts)
      `seq` use (break even uInts)
      `seq` use (lines uString)
      `seq` use (unlines uStrings)
      `seq` use (words uString)
      `seq` use (unwords uStrings)
      `seq` use (show uInt)
      `seq` use (show uInts <> show uInt)
      `seq` use otherwise
      `seq` use seq
      `seq` use undefined
