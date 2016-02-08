module Desugar
    ( module Desugar.Monad
    , desugar
    )
where

import Desugar.Monad
import Desugar.If (desugarIf)
import Desugar.Lit (desugarLit)
import Desugar.Where (desugarWhere)
import Desugar.Rename (rename)
import Desugar.GADT (desugarGADT)
import Desugar.Case (desugarCase)
import Control.Monad

desugar = desugarIf
      >=> desugarLit
      >=> desugarWhere
      >=> rename
      >=> desugarGADT
      >=> desugarCase
