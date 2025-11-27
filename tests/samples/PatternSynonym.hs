{-# LANGUAGE PatternSynonyms #-}
module PatternSynonym where

import PatternSynonymDef (pattern MyPattern)

test :: Int -> Bool
test MyPattern = True
test _ = False
