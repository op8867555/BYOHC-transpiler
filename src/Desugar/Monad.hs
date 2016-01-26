{-# LANGUAGE NamedFieldPuns #-}
module Desugar.Monad where

import Control.Monad.State
import Language.Haskell.Exts.Annotated

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

type DesugarM a = State DesugarState a

type ConData = ( String  -- constructor name
               , Int     -- constructor args count
               )

data DesugarState =
    DesugarState { cons :: Map String String -- ConName to Type
                 , dataTypes :: Map String [ConData] -- Type to ConData
                 , freshCount :: Int
                 , globals :: Map (Name ()) (SrcSpanInfo, String)
                 , moduleName :: String
                 } deriving Show

emptyDesugarState :: DesugarState
emptyDesugarState =
    DesugarState { cons = M.fromList [ (prelude "True", prelude "Bool")
                                     , (prelude "False", prelude "Bool")
                                     ]
                 , dataTypes =
                     M.fromList
                         [(prelude "Bool", [ (prelude "True", 0)
                                           , (prelude "False", 0)])]
                 , freshCount = 0
                 , globals = M.empty
                 , moduleName = ""
                 }
  where prelude = ("Prelude." ++)

runDesugarM :: DesugarM a -> DesugarState -> a
runDesugarM = evalState

addCons :: String -> [ConData] -> DesugarM ()
addCons typeName conDatas =
    modify $ \st@DesugarState{cons, dataTypes} ->
        st { cons = M.union cons $
                        M.fromList [ (conName, typeName) | (conName, _) <- conDatas ]
           , dataTypes = M.insert typeName conDatas dataTypes
           }

lookupCons :: String -> DesugarM [ConData]
lookupCons conName = do
    DesugarState{cons, dataTypes} <- get
    return $ fromMaybe (error $ "lookupCons:" ++ show conName) $ do
        typeName <- M.lookup conName cons
        M.lookup typeName dataTypes

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


