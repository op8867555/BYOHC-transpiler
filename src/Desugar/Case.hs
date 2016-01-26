module Desugar.Case (desugarCase) where

import Desugar.Case.AltCompletion (desugarAltCompletion)
import Control.Monad ((<=<))

desugarCase = desugarAltCompletion
