module SpecUtils where

import Scheme.Error()

isError :: Either error value -> Bool
isError (Right _) = False
isError (Left  _) = True
