{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Lib where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts as HSE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char

emptySrcLoc :: HSE.SrcLoc
emptySrcLoc = HSE.SrcLoc "" 0 0


mkT :: String -> HSE.Type
mkT = HSE.TyVar . HSE.Ident


mkN :: String -> HSE.Name
mkN = HSE.name


annType :: HSE.Type
annType = mkT "l"

customArgs :: [(HSE.Name, HSE.Type)]
customArgs = []


run :: Q Exp
run = do
      let entryType = ''Language.Haskell.Exts.Annotated.Module
      types <- S.toList <$> collectAllNonTrivialComponent entryType
      infos <- mapM reify types
      let decls = desugarFn ++ concatMap genFun infos
      stringE . HSE.prettyPrint $
          hModule emptySrcLoc "Desugar.Pass" imports decls


imports :: [HSE.ImportDecl]
imports = map importD [ "Desugar.Monad"
                      , "Language.Haskell.Exts.Annotated" ]
    where importD x = HSE.ImportDecl emptySrcLoc
                                     (HSE.ModuleName x)
                                     False
                                     False
                                     False
                                     Nothing
                                     Nothing
                                     Nothing


desugarFn :: [HSE.Decl]
desugarFn =
    [ HSE.TypeSig emptySrcLoc [mkN "desugarPass"] (foldr1 HSE.TyFun $ map snd customArgs ++ [t, t'])
    , HSE.patBind emptySrcLoc (HSE.pvar $ mkN "desugarPass") (HSE.var $ mkN "transModule") ]
        where t = HSE.TyApp (mkT "Module") annType
              t' = HSE.TyApp (mkT "DesugarM") t


hModule :: HSE.SrcLoc -> String -> [HSE.ImportDecl] -> [HSE.Decl] -> HSE.Module
hModule l n = HSE.Module l
                         (HSE.ModuleName n)
                         []
                         Nothing
                         Nothing


genFun :: Info -> [HSE.Decl]
genFun = \case
        TyConI (DataD [] (nameBase -> name) slots cons _) ->
            [ HSE.TypeSig emptySrcLoc [mkN ("trans" ++ name)] (foldr1 HSE.TyFun $ map snd customArgs ++ [t, t'])
            , HSE.FunBind $ map (genMatch ("trans" ++ name)) cons
            ] where t = case slots of
                            [] -> mkT name
                            [_] -> HSE.TyApp (mkT name) annType
                    t' = HSE.TyApp (mkT "DesugarM") t


rename :: [String] -> [String]
rename xs = rename' xs M.empty where
    rename' (x:xs) m
        | Just xCount <- x `M.lookup` m =
            (x ++ show xCount) : rename' xs (M.update (Just . (+1)) x m)
        | otherwise =
            x : rename' xs (M.insert x 1 m)
    rename' _ _ = []


extract :: Con -> (Name, [Type])
extract (NormalC conName types) =
    (conName, map snd types)
extract (RecC conName types) =
    (conName, map (\(_,_,c) -> c) types)


genMatch :: String -> Con -> HSE.Match
genMatch funName (extract -> (conName, types)) =
        match emptySrcLoc (mkN funName) pat body
    where
        match l n p e =
            HSE.Match l n p Nothing (HSE.UnGuardedRhs e) Nothing
        body | [HSE.Qualifier exp] <- stmts = exp
             | otherwise = HSE.doE stmts
        stmts = concatMap genStmt bindings ++
                         [ HSE.qualStmt $
                             HSE.app (hVar "return")
                                     (HSE.appFun (HSE.var conName') $ map (HSE.var . fst) bindings) ]

        bindings :: [(HSE.Name, Maybe HSE.Exp)]
        bindings = zipWith mkBind (map HSE.var argNames) types

        genStmt :: (HSE.Name, Maybe HSE.Exp) -> [HSE.Stmt]
        genStmt (n, Just e) = [HSE.genStmt emptySrcLoc (HSE.pvar n) e]
        genStmt (_, Nothing) = []

        pat = map (HSE.pvar . fst) customArgs ++
                 [HSE.pApp conName' (map HSE.pvar argNames)]

        conName' = mkN . nameBase $ conName
        argNames = map (mkN . lowerHead) . rename $ map genName types
        hVar = HSE.var . mkN
        mkPrime (HSE.Ident name) = HSE.Ident $ name ++ "'"

        mkBind :: HSE.Exp -> Type -> (HSE.Name, Maybe HSE.Exp)
        mkBind arg@(HSE.Var (HSE.UnQual name)) typ =
            case refactor $ HSE.app (apply typ) arg of
              exp | is "return" (apply typ) -> (name, Nothing)
              exp -> (mkPrime name, Just exp)

        apply :: Type -> HSE.Exp
        apply = \case
            VarT n -> hVar "return"
            ConT (nameBase -> name)
                | name `elem` trivialTypes -> hVar "return"
                | otherwise ->
                    HSE.appFun (HSE.var . mkN . ("trans" ++) $ name)
                               (map (HSE.var . fst) customArgs)
            AppT ListT x ->
                HSE.appFun (hVar "mapM") [apply x]
            AppT (ConT (nameBase -> name)) x
                | name == "Maybe" ->
                    HSE.appFun (hVar "mapM") [apply x]
            AppT (AppT (TupleT 2) a) b ->
                HSE.app (hVar "transTupleWith") (HSE.tuple [apply a, apply b])
            AppT f (VarT _) -> apply f
            _p -> error $ "apply: " ++  show _p


is :: String -> HSE.Exp -> Bool
is n (HSE.Var (HSE.UnQual (HSE.Ident n'))) | n == n' = True
is n _ = False


refactor :: HSE.Exp -> HSE.Exp
refactor (HSE.App g@(HSE.App f x) y)
    | HSE.Var (HSE.UnQual (HSE.Ident "fmap")) <- f
    , is "id" x = refactor y
    | otherwise = HSE.app (refactor g) (refactor y)
refactor (HSE.App f x)
    | is "id" f = refactor x
    | otherwise = HSE.app (refactor f) (refactor x)
refactor (HSE.InfixApp a f b)
    | is "id" a && is "id" b = HSE.function "id"
    | is "id" a = HSE.app (HSE.function "second") (refactor b)
    | is "id" b = HSE.app (HSE.function "first")  (refactor a)
    | otherwise = HSE.InfixApp (refactor a) f (refactor b)
refactor v@HSE.Var{} = v
refactor (HSE.Tuple _ [a, b]) = HSE.tuple [refactor a, refactor b]
refactor other = error $ show other


trivialTypes :: [String]
trivialTypes = ["String", "Maybe", "Int", "Rational", "Char", "Integer", "Bool"]


genName :: Type -> String
genName = lowerHead . concat . inner where
    inner = \case
          VarT n -> [upperHead . nameBase $ n]
          ConT n -> [upperHead . nameBase $ n]
          AppT (AppT (TupleT 2) x) y -> concat [ ["Tuple2"]
                                               , inner x
                                               , ["_"]
                                               , inner y
                                               ]
          AppT ListT x -> inner x ++ ["s"]
          AppT f x -> inner f ++ inner x
          _p -> error $ "genName: " ++ show _p


lowerHead :: String -> String
lowerHead (c:cs) = toLower c : cs
lowerHead [] = []


upperHead :: String -> String
upperHead (c:cs) = toUpper c : cs
upperHead [] = []


-- copied from https://github.com/CindyLinz/Haskell.js/blob/master/trans/desugar-template-src/DeriveTemplate.hs

collectAllNonTrivialComponent :: Name -> Q (S.Set Name)
collectAllNonTrivialComponent root = go S.empty (S.singleton root) where
    go res pending =
        case S.minView pending of
            Nothing -> return res
            Just (name, others)
                | S.member name res ->
                    go res others
                | otherwise -> do
                    info <- reify name
                    go (S.insert name res)
                       (nonTrivialComponentInInfo info `S.union` others)


nonTrivialComponentInType :: Type -> S.Set Name
nonTrivialComponentInType = \case
    ConT (name @ (Name (OccName nameStr) _))
        | nameStr `elem` trivialTypes -> S.empty
        | isLower (head nameStr) -> S.empty
        | otherwise -> S.singleton name
    AppT f x ->
        nonTrivialComponentInType f `S.union` nonTrivialComponentInType x
    ListT -> S.empty
    TupleT _ -> S.empty
    VarT _ -> S.empty
    others -> error $ "nonTrivialComponentInType " ++ show others ++ " not implemented"


nonTrivialComponentInInfo :: Info -> S.Set Name
nonTrivialComponentInInfo = \case
    TyConI (DataD [] _ bndrs cons _) -> mconcat $ map nonTrivialComponentInCon cons
    TyConI (TySynD _ bndrs ty) -> nonTrivialComponentInType ty
    TyConI (NewtypeD [] _ bndrs con _) -> nonTrivialComponentInCon con
    others -> error $ "nonTrivialComponentInInfo " ++ show others ++ " not implemented"


nonTrivialComponentInCon :: Con -> S.Set Name
nonTrivialComponentInCon = \case
    NormalC _ slots -> mconcat (map (nonTrivialComponentInType . snd) slots)
    RecC _ slots -> mconcat (map (\(_, _, ty) -> nonTrivialComponentInType ty) slots)
    others -> error $ "nonTrivialComponentInCon " ++ show others ++ " not implemented"
