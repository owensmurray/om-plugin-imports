{-# LANGUAGE PatternSynonyms #-}
module EnumPatDef (
  Color(Red, Green),
) where

import Prelude (Int)

data Color = ColorCon Int

pattern Red :: Color
pattern Red = ColorCon 1

pattern Green :: Color
pattern Green = ColorCon 2
