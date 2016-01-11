module Desugar
    ( module Desugar.Monad
    , desugar
    )
where

import Desugar.Monad
import Desugar.If (desugarIf)
import Desugar.Where (desugarWhere)
import Control.Monad

desugar = desugarIf >=> desugarWhere
