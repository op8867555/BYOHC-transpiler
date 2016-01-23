module Desugar
    ( module Desugar.Monad
    , desugar
    )
where

import Desugar.Monad
import Desugar.If (desugarIf)
import Desugar.Where (desugarWhere)
import Desugar.Rename (rename)
import Control.Monad

desugar = desugarIf
      >=> desugarWhere
      >=> rename
