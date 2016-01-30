module Desugar.Case (desugarCase) where

import Desugar.Case.AltCompletion (desugarAltCompletion)
import Desugar.Case.ExpandPattern (desugarExpandPattern)
import Control.Monad ((>=>))

desugarCase = desugarExpandPattern
          >=> desugarAltCompletion
