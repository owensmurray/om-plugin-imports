module EnumPatImport where

import EnumPatDef (Color(Red, Green))

f :: Color -> Bool
f Red = True
f Green = False
f _ = False
