module Desugar.Monad where

import Control.Monad.State
type DesugarM a = State DesugarState a

data DesugarState =
    DesugarState { fresh :: Int }

emptyDesugarState :: DesugarState
emptyDesugarState =
    DesugarState { fresh = 0 }

runDesugarM :: DesugarM a -> DesugarState -> a
runDesugarM = evalState

transTupleWith :: Monad m => (a -> m a, b -> m b) -> (a, b) -> m (a, b)
transTupleWith (fa, fb) (a, b) = do
    a' <- fa a
    b' <- fb b
    return (a', b')


