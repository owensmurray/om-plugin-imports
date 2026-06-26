module OrphanInstanceImport where

import OrphanInstanceDef ()
import Prelude (Show(show), String)
import WidgetDef (Widget)

foo :: Widget -> String
foo w =
  show w
