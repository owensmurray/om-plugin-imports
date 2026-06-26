module EnumPatImport where

import EnumPatDef (Color(Green, Red))
import Prelude (Bool(False, True))

f :: Color -> Bool
f Red = True
f Green = False
f _ = False
