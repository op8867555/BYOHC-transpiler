{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
module Trans
    ( transpile
    , transModule
    , build
    , parseFile
    , evalTranspiler
    , Expr(..)
    , TName
    ) where
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Fixity (preludeFixities)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Foldable (concat, foldrM)
import Data.Map (Map)
import Data.Aeson (ToJSON, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map

parseMode filename = ParseMode
  { parseFilename = filename
  , baseLanguage = Haskell2010
  , extensions = map EnableExtension
    [ OverlappingInstances
    , UndecidableInstances
    , IncoherentInstances
    , InstanceSigs
    , DoRec
    , RecursiveDo
    , ParallelListComp
    , MultiParamTypeClasses
    , FunctionalDependencies
    , RankNTypes
    , PolymorphicComponents
    , ExistentialQuantification
    , ScopedTypeVariables
    , ImplicitParams --
    , FlexibleContexts
    , FlexibleInstances
    , EmptyDataDecls
    , CPP
    , KindSignatures --
    , BangPatterns
    , TypeSynonymInstances
    , TemplateHaskell --
    , ForeignFunctionInterface --
    , Arrows
    , Generics
    , ImplicitPrelude --
    , NamedFieldPuns
    , PatternGuards
    , GeneralizedNewtypeDeriving
    , HereDocuments -- Hugs98 的功能, 看想不想做
    , MagicHash --
    , BinaryLiterals
    , TypeFamilies
    , StandaloneDeriving
    , UnicodeSyntax
    , LiberalTypeSynonyms
    , TypeOperators
    , ParallelArrays
    , RecordWildCards
    , DisambiguateRecordFields
    , OverloadedStrings
    , GADTs
    , RelaxedPolyRec
    , ExtendedDefaultRules
    , UnboxedTuples
    , DeriveDataTypeable
    , ConstrainedClassMethods
    , PackageImports --
    , LambdaCase
    , NewQualifiedOperators
    , PostfixOperators
    , QuasiQuotes --
    , TransformListComp
    , ViewPatterns
    , TupleSections
    , DoAndIfThenElse
    , RebindableSyntax
    , ExplicitForAll
    , DeriveFunctor
    , DeriveGeneric
    , DeriveTraversable
    , DeriveFoldable
    , NondecreasingIndentation
    , ExplicitNamespaces
    , DataKinds --
    , PolyKinds --
    , MultiWayIf
    , DefaultSignatures
    , ConstraintKinds
    ]
  , ignoreLanguagePragmas = True
  , ignoreLinePragmas = False
  , fixities = Just preludeFixities
  }


data Expr a = TVar TName
            | TAbs TName (Expr a)
            | TApp (Expr a) (Expr a)
            | TPrim String String -- Primitive Type and Value
            | TChar Char
            | TInt Integer
            deriving (Show)


instance ToJSON a => ToJSON (Expr a) where
    toJSON (TAbs name expr) = toJSON [toJSON "lam", toJSON name, toJSON expr]
    toJSON (TApp e1 e2) = toJSON [toJSON "app", toJSON e1, toJSON e2]
    toJSON (TVar name) = toJSON [toJSON "var", toJSON name]
    toJSON (TPrim typ raw) = toJSON [toJSON "prim", toJSON typ, toJSON raw]
    toJSON (TChar c) = toJSON [toJSON "prim", toJSON "str", toJSON c]
    toJSON (TInt i) = toJSON [toJSON "prim", toJSON "int", toJSON i]

data TransState =
        TransState { cons :: Map TName Int
                   , freshCount :: Int
                   } deriving (Show)

emptyTransState :: TransState
emptyTransState =
    TransState { cons = Map.empty
               , freshCount = 0
               }

type Transpiler a = StateT TransState IO a

type TName = String

freshVar :: String -> Transpiler String
freshVar str = do
    state@TransState {freshCount=freshCount} <- get
    put $ state { freshCount = freshCount + 1}
    return $ str ++ show freshCount

addCons :: TName -> Int -> Transpiler ()
addCons dataName count =
    modify $ \st@TransState{cons} ->
        st{cons=Map.insert dataName count cons}

lambda :: [TName] -> Expr a -> Expr a
lambda vars body = foldr TAbs body vars

apply :: Expr a -> [Expr a] -> Expr a
apply f vs = foldl TApp f vs

transModule :: Module -> Transpiler [(TName, Expr a)]
transModule (Module srcloc moduleName pragma warningText exportSpec importDecls decls) =
    transDecls decls


gadtCase :: Exp -> [Alt] -> Transpiler (Expr a)
gadtCase exp alts = do
        exp' <- transExp exp
        alts' <- mapM transAlt alts
        return $ apply exp' alts'
    where transAlt (Alt srcLoc pat rhs binds) =
              case pat of
                   PApp qname pats ->
                       lambda <$> mapM transPat pats <*> transRhs rhs
                   PList [] -> transRhs rhs
          transPApp (PList []) = return ("cons", [])

intCase :: Exp -> [Alt] -> Transpiler (Expr a)
intCase exp alts = do
        exp' <- transExp exp
        alts' <- transAlts alts
        return $ apply (lambda ["##case"] alts') [exp']
    where transAlts [] = return $ TVar "()"
          transAlts (Alt srcLoc pat rhs binds:alts) = do
              pred <- transPat pat
              rhs' <- transRhs rhs
              alts' <- transAlts alts
              return $ apply (TVar "if") [ pred , rhs', alts' ]
          transPat (PLit sign lit) = do
              lit' <- transExp (Lit lit)
              return $ apply (TVar "==") [TVar "##case", lit']
          transPat PWildCard = return $ TVar "True"


transExp :: Exp -> Transpiler (Expr a)
transExp (Lambda src pats exp) = do
    exp' <- transExp exp
    names <- mapM transPat pats
    return $ lambda names exp'
transExp (Var qname) = do
    name <- transQName qname
    return $ TVar name
transExp (App e1 e2) =
    TApp <$> transExp e1 <*> transExp e2
transExp (Case exp alts) =
        let Alt srcLoc pat rhs binds = head alts
        in case pat of
                PLit {} -> intCase exp alts
                _ -> gadtCase exp alts
transExp (Lit literal) =
    return $ case literal of
                  Char c -> TChar c
                  Int i -> TInt i
                  String s -> foldr (\c cs -> apply (TVar "cons") [c, cs])
                                    (TVar "nil")
                                    (map TChar s)
                  _ -> error $ show literal
transExp (List xs) = do
    xs' <- mapM transExp xs
    return $ foldr (\x xs -> apply (TVar "cons") [x, xs])
                   (TVar "nil")
                   xs'
transExp (Paren exp) = transExp exp
transExp (Con qname) = TVar <$> transQName qname
transExp (Let (BDecls decls) exp) = do
    bindings <- transDecls decls
    exp' <- transExp exp
    return $ makeLet bindings exp'
transExp _rest = error $ show _rest

makeLet :: [(TName, Expr a)] -> Expr a -> Expr a
makeLet bindings exp =
    TApp (TApp (TVar "Y")
               (lambda ["##gen", "##tuple"] (apply (lambda names (apply (TVar "##tuple") exprs)) (map gen names))))
         (lambda names exp)
    where
        (names, exprs) = unzip bindings
        gen x = TApp (TVar "##gen") (lambda names (TVar x))


transRhs :: Rhs -> Transpiler (Expr a)
transRhs (UnGuardedRhs exp) = transExp exp
transRhs _rest = error $ show _rest

transPat :: Pat -> Transpiler TName
transPat (PVar name) = transName name
transPat PWildCard = return "_"
transPat _rest = error $ show _rest

transName :: Name -> Transpiler TName
transName (Ident str) = return str
transName (Symbol str) = return str

transQName :: QName -> Transpiler TName
transQName (UnQual name) = transName name
transQName (Special Cons) = return "cons"
transQName _rest = error $ show _rest

transDecls :: [Decl] -> Transpiler [(TName, Expr a)]
transDecls decls = fmap concat . sequence $ map transDecl decls

transDecl :: Decl -> Transpiler [(TName, Expr a)]
transDecl (PatBind srcloc pat rhs binds) = do
    pat' <- transPat pat
    rhs' <- transRhs rhs
    return [(pat', rhs')]
transDecl (GDataDecl srcLoc dataOrNew context name tyVarBinds kind gadtDecls derivings) =
    do  dataName <- transName name
        consNames <- mapM consName gadtDecls
        let go (GadtDecl srcLoc name nameTyPairs ty) = do
                let varsCount = countVars ty
                let vars = map show $ take varsCount [0..]
                name' <- transName name
                addCons name' varsCount
                return (name', lambda (vars ++ consNames)
                                      (apply (TVar name') (map TVar vars)))
        mapM go gadtDecls
    where
        consName (GadtDecl srcLoc name nameTyPairs ty) = transName name
transDecl _rest = error $ show _rest

countVars :: Type -> Int
countVars = count 0 where
    count !n (TyFun t1 t2) =  count (n+1) t2
    count !n _ = n

parseFile :: String -> String -> Transpiler Module
parseFile filename input = return ast
    where
        parsed = parseWithMode (parseMode filename) input :: ParseResult Module
        ast = case parsed of
                   ParseOk a -> a
                   ParseFailed srcLoc err -> error $ show srcLoc ++ ": " ++ err

build :: [(TName, Expr a)] -> Transpiler (Expr a)
build bindings = return $
    makeLet bindings $ TApp (TVar "runIO") (TVar "main")

transpile :: String -> String -> Transpiler (Expr a)
transpile filename input =
        parseFile filename input
    >>= transModule
    >>= build

evalTranspiler :: Transpiler a -> IO a
evalTranspiler t = evalStateT t emptyTransState
