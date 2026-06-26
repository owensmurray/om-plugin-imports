{-# LANGUAGE PatternSynonyms #-}
module PatternSynonym where

import PatternSynonymDef (pattern MyPattern)
import Prelude (Bool(False, True), Int)

test :: Int -> Bool
test MyPattern = True
test _ = False
