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
import Language.Haskell.Exts.Annotated hiding (parseFile)
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
  , ignoreFunctionArity = False
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

transModule :: Module l -> Transpiler [(TName, Expr a)]
transModule (Module srcloc head pragma importDecls decls) =
    transDecls decls


gadtCase :: Exp l -> [Alt l] -> Transpiler (Expr a)
gadtCase exp alts = do
        exp' <- transExp exp
        alts' <- mapM transAlt alts
        return $ apply exp' alts'
    where transAlt (Alt srcLoc pat rhs binds) =
              case pat of
                   PApp l qname pats ->
                       lambda <$> mapM transPat pats <*> transRhs rhs
                   PList l [] -> transRhs rhs
          transPApp (PList l []) = return ("cons", [])

intCase :: Exp l -> [Alt l] -> Transpiler (Expr a)
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
          transPat (PLit l sign lit) = do
              lit' <- transExp (Lit l lit)
              return $ apply (TVar "==") [TVar "##case", lit']
          transPat (PWildCard l) = return $ TVar "True"


transExp :: Exp l -> Transpiler (Expr a)
transExp (Lambda l pats exp) = do
    exp' <- transExp exp
    names <- mapM transPat pats
    return $ lambda names exp'
transExp (Var l qname) = do
    name <- transQName qname
    return $ TVar name
transExp (App l e1 e2) =
    TApp <$> transExp e1 <*> transExp e2
transExp (Case l exp alts) =
        let Alt srcLoc pat rhs binds = head alts
        in case pat of
                PLit {} -> intCase exp alts
                _ -> gadtCase exp alts
transExp (Lit l literal) =
    return $ case literal of
                  Char l c s -> TChar c
                  Int l i s-> TInt i
                  String l s s' -> foldr (\c cs -> apply (TVar "cons") [c, cs])
                                    (TVar "nil")
                                    (map TChar s)
transExp (List l xs) = do
    xs' <- mapM transExp xs
    return $ foldr (\x xs -> apply (TVar "cons") [x, xs])
                   (TVar "nil")
                   xs'
transExp (Paren l exp) = transExp exp
transExp (Con l qname) = TVar <$> transQName qname
transExp (Let l (BDecls l' decls) exp) = do
    bindings <- transDecls decls
    exp' <- transExp exp
    return $ makeLet bindings exp'
transExp (InfixApp l lexp qop rexp) = do
    qop' <- transQOp qop
    lexp' <- transExp lexp
    rexp' <- transExp rexp
    return $ apply (TVar qop') [lexp', rexp']

makeLet :: [(TName, Expr a)] -> Expr a -> Expr a
makeLet bindings exp =
    TApp (TApp (TVar "Y")
               (lambda ["##gen", "##tuple"] (apply (lambda names (apply (TVar "##tuple") exprs)) (map gen names))))
         (lambda names exp)
    where
        (names, exprs) = unzip bindings
        gen x = TApp (TVar "##gen") (lambda names (TVar x))


transRhs :: Rhs l -> Transpiler (Expr a)
transRhs (UnGuardedRhs l exp) = transExp exp

transPat :: Pat l -> Transpiler TName
transPat (PVar l name) = transName name
transPat (PWildCard l) = return "_"

transName :: Name l -> Transpiler TName
transName (Ident l str) = return str
transName (Symbol l str) = return str

transDeclHead :: DeclHead l -> Transpiler TName
transDeclHead (DHead l name) = transName name

transQName :: QName l -> Transpiler TName
transQName (UnQual l name) = transName name
transQName (Special l (Cons l')) = return "cons"

transQOp :: QOp l -> Transpiler TName
transQOp (QVarOp l qname) = transQName qname
transQOp (QConOp l qname) = transQName qname

transDecls :: [Decl l] -> Transpiler [(TName, Expr a)]
transDecls decls = fmap concat . sequence $ map transDecl decls

transDecl :: Decl l -> Transpiler [(TName, Expr a)]
transDecl (PatBind srcloc pat rhs binds) = do
    pat' <- transPat pat
    rhs' <- transRhs rhs
    return [(pat', rhs')]
transDecl (GDataDecl srcLoc dataOrNew context head kind gadtDecls derivings) =
    do  dataName <- transDeclHead head
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

transDecl (ForImp srcLoc callConv safety str name ty) =
    do
        name' <- transName name
        let Just ffiStr = str
        return [(name', TPrim "ffi" ffiStr)]

transDecl (InfixDecl srcLoc assoc priority ops) = return []

countVars :: Type l -> Int
countVars = count 0 where
    count !n (TyFun l t1 t2) =  count (n+1) t2
    count !n _ = n

parseFile :: String -> String -> Transpiler (Module SrcSpanInfo)
parseFile filename input = return ast
    where
        parsed = parseWithMode (parseMode filename) input :: ParseResult (Module SrcSpanInfo)
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
