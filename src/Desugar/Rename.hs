module Desugar.Rename (rename) where

import Language.Haskell.Exts.Annotated

import Desugar.Monad
import Desugar.Rename.CollectGlobal (desugarCollectGlobal)
import Desugar.Rename.Rename        (desugarRename)

rename :: Module SrcSpanInfo -> DesugarM (Module SrcSpanInfo)
rename m =     desugarCollectGlobal m
           >>= desugarRename
