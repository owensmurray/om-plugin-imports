module OrphanInstanceImport where

import OrphanInstanceDef ()
import WidgetDef (Widget(Widget))
import Prelude (Show(show), String)

foo :: Widget -> String
foo w =
  show w
