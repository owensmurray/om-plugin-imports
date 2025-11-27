module AmbiguousUser where

import qualified AmbiguousA as M
import qualified AmbiguousB as M

val :: Int
val = M.foo + M.bar
