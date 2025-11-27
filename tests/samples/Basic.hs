module Basic where

import Data.List (foldl')
import Data.Maybe (mapMaybe)

foo :: [Int] -> Int
foo = foldl' (+) 0

bar :: [Maybe Int] -> [Int]
bar = mapMaybe id
