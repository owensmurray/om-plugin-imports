module Basic where

import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)
import Prelude (Foldable(foldl'), Num((+)), Char, Int, Maybe, id)

foo :: [Int] -> Int
foo = foldl' (+) 0


bar :: [Maybe Int] -> [Int]
bar = mapMaybe id


baz :: [Maybe Int] -> [Int]
baz = catMaybes


qux :: [[Char]] -> [Char]
qux = intercalate ","
