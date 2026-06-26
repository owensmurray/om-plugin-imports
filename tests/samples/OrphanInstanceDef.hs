{-# OPTIONS_GHC -Wno-orphans #-}
module OrphanInstanceDef () where

import Prelude (Show(show))
import WidgetDef (Widget)

instance Show Widget where
  show _ =
    "Widget"
