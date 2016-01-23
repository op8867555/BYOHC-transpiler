{-# LANGUAGE NamedFieldPuns #-}
module Desugar.Monad where

import Control.Monad.State
import Language.Haskell.Exts.Annotated

import qualified Data.Map as M
import Data.Map (Map)

type DesugarM a = State DesugarState a

data DesugarState =
    DesugarState { freshCount :: Int
                 , globals :: Map (Name ()) (SrcSpanInfo, String)
                 , moduleName :: String
                 } deriving Show

emptyDesugarState :: DesugarState
emptyDesugarState =
    DesugarState { freshCount = 0
                 , globals = M.empty
                 , moduleName = ""
                 }

runDesugarM :: DesugarM a -> DesugarState -> a
runDesugarM = evalState

extractName :: Name l -> String
extractName (Ident _ s) = s
extractName (Symbol _ s) = s

freshVar :: Name l -> DesugarM String
freshVar name = do
    let s = extractName name
    st@DesugarState { freshCount } <- get
    put $ st { freshCount = succ freshCount }
    return $ s ++ "_" ++ show freshCount

getModuleName :: DesugarM String
getModuleName = liftM moduleName get

setModuleName :: String -> DesugarM ()
setModuleName s =
    modify $ \st@DesugarState{ moduleName } ->
        st { moduleName = s }

moduleDefined :: Name SrcSpanInfo -> DesugarM String
moduleDefined name = do
    let s = extractName name
    moduleName <- getModuleName
    return $ concat [moduleName, ".", s]

addGlobal :: Name SrcSpanInfo -> String -> DesugarM ()
addGlobal name qname =
    modify $ \st@DesugarState{ globals } ->
        st{ globals = M.insert (void name) (ann name, qname) globals }

lookupGlobal :: Name l -> DesugarM (Maybe (SrcSpanInfo, String))
lookupGlobal name = do
    DesugarState{ globals } <- get
    return $ void name `M.lookup` globals

transTupleWith :: Monad m => (a -> m a, b -> m b) -> (a, b) -> m (a, b)
transTupleWith (fa, fb) (a, b) = do
    a' <- fa a
    b' <- fb b
    return (a', b')


