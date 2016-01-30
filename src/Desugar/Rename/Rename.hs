{-
這個 pass 將所以 Name 跟 QName 都重新命名

1. top-level name 改為 full qualified
2. local-defined name 加上唯一 suffix

-------------------------------------

尚未實作

* `Maybe (Binds l)`:
        where-bindings
* `FunBind l`:
        function binds
* `Stmt`:
        pattern guard, do-notation
* `QualStmt`:
        list comprehension

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Desugar.Rename.Rename where
import Desugar.Monad
import Language.Haskell.Exts.Annotated

import Control.Monad (void)
import Control.Applicative ((<|>))
import Prelude hiding (lookup)
import qualified  Data.Map.Strict as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Char (isLower)

type LocalEnv = Map (Name ()) String

desugarRename :: Module l -> DesugarM (Module l)
desugarRename = transModule initLocals
    -- TODO: 在實作模組之前，將 Prelude 的名字直接寫在
    where initLocals =
            M.fromList $ syms ++ idents
          idents = map ide [ "putStrLn"
                           , "putChar"
                           , "const"
                           , "fst"
                           , "snd"
                           , "div"
                           , "mod"
                           , "head"
                           , "tail"
                           , "True"
                           , "False"]
          syms = map sym [ "+"
                         , "-"
                         , "*"
                         , "=="
                         ]
          ide a = (Ident () a, "Prelude." ++ a)
          sym a = (Symbol () a, "Prelude." ++ a)
lookup :: Name l -> LocalEnv -> DesugarM String
lookup name@(extractName -> n) locals = do
    tryGlobal <- lookupGlobal name
    let err = error $ "Symbol not found: " ++ show n
    return . fromMaybe err $
            void name `M.lookup` locals
        <|> snd <$> tryGlobal

modifyName :: (String -> String) -> Name l -> Name l
modifyName f (Ident l s) =  Ident l (f s)
modifyName f (Symbol l s) = Symbol l (f s)

addLocal :: Name l -> LocalEnv -> DesugarM (Name l, LocalEnv)
addLocal name locals = do
    freshNameStr <- freshVar name
    let name' = modifyName (const freshNameStr) name
        newBindings = M.fromList [(void name, freshNameStr)
                                 ,(void name', freshNameStr)]
        locals' = M.union newBindings locals
    return (name', locals')

mergeLocals :: [LocalEnv] -> LocalEnv
mergeLocals = M.unions

withPat :: LocalEnv -> Pat l -> DesugarM (Pat l, LocalEnv)
withPat locals (PVar l name) = do
    (name', locals') <- addLocal name locals
    return (PVar l name', locals')
withPat locals (PParen l pat) =
    withPat locals pat
withPat locals (PApp l qname pats) = do
    (pats', locals') <- withPats locals pats
    return (PApp l qname pats', locals')
withPat locals p@PLit{} = return (p, locals)
withPat locals p@PWildCard{} = return (p, locals)
withPat locals p = error $ show $ void p

withPats :: LocalEnv -> [Pat l] -> DesugarM ([Pat l], LocalEnv)
withPats l ps = do
    (ps', ls) <- unzip <$> mapM (withPat l) ps
    return (ps', M.union (M.unions ls) l)


transMaybeBinds :: LocalEnv
                -> Maybe (Binds l)
                -> DesugarM (Maybe (Binds l), LocalEnv)
transMaybeBinds locals maybeBindsL = do
       (maybeBindsL', maybeLocals') <-
           unpairMaybe <$> mapM (transBinds locals) maybeBindsL
       let locals' = fromMaybe locals maybeLocals'
       return (maybeBindsL' , locals')
    where
        unpairMaybe (Just (a, b)) = (Just a, Just b)
        unpairMaybe Nothing = (Nothing, Nothing)


transActivation ::
                LocalEnv -> Activation l -> DesugarM (Activation l)
transActivation locals (ActiveFrom l int)
  = return (ActiveFrom l int)
transActivation locals (ActiveUntil l int)
  = return (ActiveUntil l int)

-- desugar:
transAlt :: LocalEnv -> Alt l -> DesugarM (Alt l)
transAlt locals (Alt l patL rhsL maybeBindsL)
  = do (patL', locals') <- withPat locals patL
       patL'' <- transPat locals' patL'
       (maybeBindsL', locals'') <- transMaybeBinds locals' maybeBindsL
       rhsL' <- transRhs locals'' rhsL
       return (Alt l patL'' rhsL' maybeBindsL')

transAnnotation ::
                LocalEnv -> Annotation l -> DesugarM (Annotation l)
transAnnotation locals (Ann l nameL expL)
  = do nameL' <- transName locals nameL
       expL' <- transExp locals expL
       return (Ann l nameL' expL')
transAnnotation locals (TypeAnn l nameL expL)
  = do nameL' <- transName locals nameL
       expL' <- transExp locals expL
       return (TypeAnn l nameL' expL')
transAnnotation locals (ModuleAnn l expL)
  = do expL' <- transExp locals expL
       return (ModuleAnn l expL')

transAssoc :: LocalEnv -> Assoc l -> DesugarM (Assoc l)
transAssoc locals (AssocNone l) = return (AssocNone l)
transAssoc locals (AssocLeft l) = return (AssocLeft l)
transAssoc locals (AssocRight l) = return (AssocRight l)

transAsst :: LocalEnv -> Asst l -> DesugarM (Asst l)
transAsst locals (ClassA l qNameL typeLs)
  = do qNameL' <- transQName locals qNameL
       typeLs' <- mapM (transType locals) typeLs
       return (ClassA l qNameL' typeLs')
transAsst locals (AppA l nameL typeLs)
  = do nameL' <- transName locals nameL
       typeLs' <- mapM (transType locals) typeLs
       return (AppA l nameL' typeLs')
transAsst locals (InfixA l typeL qNameL typeL1)
  = do typeL' <- transType locals typeL
       qNameL' <- transQName locals qNameL
       typeL1' <- transType locals typeL1
       return (InfixA l typeL' qNameL' typeL1')
transAsst locals (IParam l iPNameL typeL)
  = do iPNameL' <- transIPName locals iPNameL
       typeL' <- transType locals typeL
       return (IParam l iPNameL' typeL')
transAsst locals (EqualP l typeL typeL1)
  = do typeL' <- transType locals typeL
       typeL1' <- transType locals typeL1
       return (EqualP l typeL' typeL1')
transAsst locals (ParenA l asstL)
  = do asstL' <- transAsst locals asstL
       return (ParenA l asstL')
transAsst locals (WildCardA l maybeNameL)
  = do maybeNameL' <- mapM (transName locals) maybeNameL
       return (WildCardA l maybeNameL')

transBangType :: LocalEnv -> BangType l -> DesugarM (BangType l)
transBangType locals (BangedTy l) = return (BangedTy l)
transBangType locals (UnpackedTy l) = return (UnpackedTy l)

-- desugar:
-- transBinds 需要將 (Binds l) 所產生出的名字傳回去
transBinds :: LocalEnv -> Binds l -> DesugarM (Binds l, LocalEnv)
transBinds locals (BDecls l declLs)
  = do
      (declLs', locals') <- unzip <$> mapM (transDecl' locals) declLs
      return (BDecls l declLs', mergeLocals locals')
  where
      transDecl' locals (PatBind l patL rhsL maybeBindsL) = do
          (patL', locals') <- withPat locals patL
          decl <- transDecl locals' (PatBind l patL' rhsL maybeBindsL)
          return (decl, locals')
      transDecl' locals pat = error "transBinds: unsupported bindings"

transBinds locals (IPBinds l iPBindLs)
  = do iPBindLs' <- mapM (transIPBind locals) iPBindLs
       return (locals, IPBinds l iPBindLs')
       error "IPBinds is not implemented yet"

transBooleanFormula ::
                    LocalEnv -> BooleanFormula l -> DesugarM (BooleanFormula l)
transBooleanFormula locals (VarFormula l nameL)
  = do nameL' <- transName locals nameL
       return (VarFormula l nameL')
transBooleanFormula locals (AndFormula l booleanFormulaLs)
  = do booleanFormulaLs' <- mapM (transBooleanFormula locals)
                              booleanFormulaLs
       return (AndFormula l booleanFormulaLs')
transBooleanFormula locals (OrFormula l booleanFormulaLs)
  = do booleanFormulaLs' <- mapM (transBooleanFormula locals)
                              booleanFormulaLs
       return (OrFormula l booleanFormulaLs')
transBooleanFormula locals (ParenFormula l booleanFormulaL)
  = do booleanFormulaL' <- transBooleanFormula locals booleanFormulaL
       return (ParenFormula l booleanFormulaL')

transBoxed :: LocalEnv -> Boxed -> DesugarM Boxed
transBoxed locals Boxed = return Boxed
transBoxed locals Unboxed = return Unboxed

transBracket :: LocalEnv -> Bracket l -> DesugarM (Bracket l)
transBracket locals (ExpBracket l expL)
  = do expL' <- transExp locals expL
       return (ExpBracket l expL')
transBracket locals (PatBracket l patL)
  = do patL' <- transPat locals patL
       return (PatBracket l patL')
transBracket locals (TypeBracket l typeL)
  = do typeL' <- transType locals typeL
       return (TypeBracket l typeL')
transBracket locals (DeclBracket l declLs)
  = do declLs' <- mapM (transDecl locals) declLs
       return (DeclBracket l declLs')

transCName :: LocalEnv -> CName l -> DesugarM (CName l)
transCName locals (VarName l nameL)
  = do nameL' <- transName locals nameL
       return (VarName l nameL')
transCName locals (ConName l nameL)
  = do nameL' <- transName locals nameL
       return (ConName l nameL')

transCallConv :: LocalEnv -> CallConv l -> DesugarM (CallConv l)
transCallConv locals (StdCall l) = return (StdCall l)
transCallConv locals (CCall l) = return (CCall l)
transCallConv locals (CPlusPlus l) = return (CPlusPlus l)
transCallConv locals (DotNet l) = return (DotNet l)
transCallConv locals (Jvm l) = return (Jvm l)
transCallConv locals (Js l) = return (Js l)
transCallConv locals (JavaScript l) = return (JavaScript l)
transCallConv locals (CApi l) = return (CApi l)

transClassDecl :: LocalEnv -> ClassDecl l -> DesugarM (ClassDecl l)
transClassDecl locals (ClsDecl l declL)
  = do declL' <- transDecl locals declL
       return (ClsDecl l declL')
transClassDecl locals
  (ClsDataFam l maybeContextL declHeadL maybeKindL)
  = do maybeContextL' <- mapM (transContext locals) maybeContextL
       declHeadL' <- transDeclHead locals declHeadL
       maybeKindL' <- mapM (transKind locals) maybeKindL
       return (ClsDataFam l maybeContextL' declHeadL' maybeKindL')
transClassDecl locals (ClsTyFam l declHeadL maybeKindL)
  = do declHeadL' <- transDeclHead locals declHeadL
       maybeKindL' <- mapM (transKind locals) maybeKindL
       return (ClsTyFam l declHeadL' maybeKindL')
transClassDecl locals (ClsTyDef l typeL typeL1)
  = do typeL' <- transType locals typeL
       typeL1' <- transType locals typeL1
       return (ClsTyDef l typeL' typeL1')
transClassDecl locals (ClsDefSig l nameL typeL)
  = do nameL' <- transName locals nameL
       typeL' <- transType locals typeL
       return (ClsDefSig l nameL' typeL')

transConDecl :: LocalEnv -> ConDecl l -> DesugarM (ConDecl l)
transConDecl locals (ConDecl l nameL typeLs)
  = do nameL' <- transName locals nameL
       typeLs' <- mapM (transType locals) typeLs
       return (ConDecl l nameL' typeLs')
transConDecl locals (InfixConDecl l typeL nameL typeL1)
  = do typeL' <- transType locals typeL
       nameL' <- transName locals nameL
       typeL1' <- transType locals typeL1
       return (InfixConDecl l typeL' nameL' typeL1')
transConDecl locals (RecDecl l nameL fieldDeclLs)
  = do nameL' <- transName locals nameL
       fieldDeclLs' <- mapM (transFieldDecl locals) fieldDeclLs
       return (RecDecl l nameL' fieldDeclLs')

transContext :: LocalEnv -> Context l -> DesugarM (Context l)
transContext locals (CxSingle l asstL)
  = do asstL' <- transAsst locals asstL
       return (CxSingle l asstL')
transContext locals (CxTuple l asstLs)
  = do asstLs' <- mapM (transAsst locals) asstLs
       return (CxTuple l asstLs')
transContext locals (CxEmpty l) = return (CxEmpty l)

transDataOrNew :: LocalEnv -> DataOrNew l -> DesugarM (DataOrNew l)
transDataOrNew locals (DataType l) = return (DataType l)
transDataOrNew locals (NewType l) = return (NewType l)

transDecl :: LocalEnv -> Decl l -> DesugarM (Decl l)
transDecl locals (TypeDecl l declHeadL typeL)
  = do declHeadL' <- transDeclHead locals declHeadL
       typeL' <- transType locals typeL
       return (TypeDecl l declHeadL' typeL')
transDecl locals (TypeFamDecl l declHeadL maybeKindL)
  = do declHeadL' <- transDeclHead locals declHeadL
       maybeKindL' <- mapM (transKind locals) maybeKindL
       return (TypeFamDecl l declHeadL' maybeKindL')
transDecl locals
  (ClosedTypeFamDecl l declHeadL maybeKindL typeEqnLs)
  = do declHeadL' <- transDeclHead locals declHeadL
       maybeKindL' <- mapM (transKind locals) maybeKindL
       typeEqnLs' <- mapM (transTypeEqn locals) typeEqnLs
       return (ClosedTypeFamDecl l declHeadL' maybeKindL' typeEqnLs')
transDecl locals
  (DataDecl l dataOrNewL maybeContextL declHeadL qualConDeclLs
     maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew locals dataOrNewL
       maybeContextL' <- mapM (transContext locals) maybeContextL
       declHeadL' <- transDeclHead locals declHeadL
       qualConDeclLs' <- mapM (transQualConDecl locals) qualConDeclLs
       maybeDerivingL' <- mapM (transDeriving locals) maybeDerivingL
       return
         (DataDecl l dataOrNewL' maybeContextL' declHeadL' qualConDeclLs'
            maybeDerivingL')
transDecl locals
  (GDataDecl l dataOrNewL maybeContextL declHeadL maybeKindL
     gadtDeclLs maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew locals dataOrNewL
       maybeContextL' <- mapM (transContext locals) maybeContextL
       declHeadL' <- transDeclHead locals declHeadL
       maybeKindL' <- mapM (transKind locals) maybeKindL
       gadtDeclLs' <- mapM (transGadtDecl locals) gadtDeclLs
       maybeDerivingL' <- mapM (transDeriving locals) maybeDerivingL
       return
         (GDataDecl l dataOrNewL' maybeContextL' declHeadL' maybeKindL'
            gadtDeclLs'
            maybeDerivingL')
transDecl locals (DataFamDecl l maybeContextL declHeadL maybeKindL)
  = do maybeContextL' <- mapM (transContext locals) maybeContextL
       declHeadL' <- transDeclHead locals declHeadL
       maybeKindL' <- mapM (transKind locals) maybeKindL
       return (DataFamDecl l maybeContextL' declHeadL' maybeKindL')
transDecl locals (TypeInsDecl l typeL typeL1)
  = do typeL' <- transType locals typeL
       typeL1' <- transType locals typeL1
       return (TypeInsDecl l typeL' typeL1')
transDecl locals
  (DataInsDecl l dataOrNewL typeL qualConDeclLs maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew locals dataOrNewL
       typeL' <- transType locals typeL
       qualConDeclLs' <- mapM (transQualConDecl locals) qualConDeclLs
       maybeDerivingL' <- mapM (transDeriving locals) maybeDerivingL
       return
         (DataInsDecl l dataOrNewL' typeL' qualConDeclLs' maybeDerivingL')
transDecl locals
  (GDataInsDecl l dataOrNewL typeL maybeKindL gadtDeclLs
     maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew locals dataOrNewL
       typeL' <- transType locals typeL
       maybeKindL' <- mapM (transKind locals) maybeKindL
       gadtDeclLs' <- mapM (transGadtDecl locals) gadtDeclLs
       maybeDerivingL' <- mapM (transDeriving locals) maybeDerivingL
       return
         (GDataInsDecl l dataOrNewL' typeL' maybeKindL' gadtDeclLs'
            maybeDerivingL')
transDecl locals
  (ClassDecl l maybeContextL declHeadL funDepLs maybeClassDeclLs)
  = do maybeContextL' <- mapM (transContext locals) maybeContextL
       declHeadL' <- transDeclHead locals declHeadL
       funDepLs' <- mapM (transFunDep locals) funDepLs
       maybeClassDeclLs' <- mapM (mapM (transClassDecl locals))
                              maybeClassDeclLs
       return
         (ClassDecl l maybeContextL' declHeadL' funDepLs' maybeClassDeclLs')
transDecl locals
  (InstDecl l maybeOverlapL instRuleL maybeInstDeclLs)
  = do maybeOverlapL' <- mapM (transOverlap locals) maybeOverlapL
       instRuleL' <- transInstRule locals instRuleL
       maybeInstDeclLs' <- mapM (mapM (transInstDecl locals))
                             maybeInstDeclLs
       return (InstDecl l maybeOverlapL' instRuleL' maybeInstDeclLs')
transDecl locals (DerivDecl l maybeOverlapL instRuleL)
  = do maybeOverlapL' <- mapM (transOverlap locals) maybeOverlapL
       instRuleL' <- transInstRule locals instRuleL
       return (DerivDecl l maybeOverlapL' instRuleL')
transDecl locals (InfixDecl l assocL maybeInt opLs)
  = do assocL' <- transAssoc locals assocL
       maybeInt' <- mapM return maybeInt
       opLs' <- mapM (transOp locals) opLs
       return (InfixDecl l assocL' maybeInt' opLs')
transDecl locals (DefaultDecl l typeLs)
  = do typeLs' <- mapM (transType locals) typeLs
       return (DefaultDecl l typeLs')
transDecl locals (SpliceDecl l expL)
  = do expL' <- transExp locals expL
       return (SpliceDecl l expL')
transDecl locals (TypeSig l nameLs typeL)
  = do nameLs' <- mapM (transName locals) nameLs
       typeL' <- transType locals typeL
       return (TypeSig l nameLs' typeL')
transDecl locals
  (PatSynSig l nameL maybeTyVarBindLs maybeContextL maybeContextL1
     typeL)
  = do nameL' <- transName locals nameL
       maybeTyVarBindLs' <- mapM (mapM (transTyVarBind locals))
                              maybeTyVarBindLs
       maybeContextL' <- mapM (transContext locals) maybeContextL
       maybeContextL1' <- mapM (transContext locals) maybeContextL1
       typeL' <- transType locals typeL
       return
         (PatSynSig l nameL' maybeTyVarBindLs' maybeContextL'
            maybeContextL1'
            typeL')
transDecl locals (FunBind l matchLs)
  = do matchLs' <- mapM (transMatch locals) matchLs
       return (FunBind l matchLs')
-- desugar:
transDecl locals (PatBind l patL rhsL maybeBindsL)
  = do patL' <- transPat locals patL
       (maybeBindsL', locals'') <- transMaybeBinds locals maybeBindsL
       rhsL' <- transRhs locals'' rhsL
       return (PatBind l patL' rhsL' maybeBindsL')
transDecl locals (PatSyn l patL patL1 patternSynDirectionL)
  = do patL' <- transPat locals patL
       patL1' <- transPat locals patL1
       patternSynDirectionL' <- transPatternSynDirection locals
                                  patternSynDirectionL
       return (PatSyn l patL' patL1' patternSynDirectionL')
transDecl locals
  (ForImp l callConvL maybeSafetyL maybeString nameL typeL)
  = do callConvL' <- transCallConv locals callConvL
       maybeSafetyL' <- mapM (transSafety locals) maybeSafetyL
       maybeString' <- mapM return maybeString
       nameL' <- transName locals nameL
       typeL' <- transType locals typeL
       return
         (ForImp l callConvL' maybeSafetyL' maybeString' nameL' typeL')
transDecl locals (ForExp l callConvL maybeString nameL typeL)
  = do callConvL' <- transCallConv locals callConvL
       maybeString' <- mapM return maybeString
       nameL' <- transName locals nameL
       typeL' <- transType locals typeL
       return (ForExp l callConvL' maybeString' nameL' typeL')
transDecl locals (RulePragmaDecl l ruleLs)
  = do ruleLs' <- mapM (transRule locals) ruleLs
       return (RulePragmaDecl l ruleLs')
transDecl locals (DeprPragmaDecl l tuple2NameLs_Strings)
  = do tuple2NameLs_Strings' <- mapM
                                  (transTupleWith (mapM (transName locals), return))
                                  tuple2NameLs_Strings
       return (DeprPragmaDecl l tuple2NameLs_Strings')
transDecl locals (WarnPragmaDecl l tuple2NameLs_Strings)
  = do tuple2NameLs_Strings' <- mapM
                                  (transTupleWith (mapM (transName locals), return))
                                  tuple2NameLs_Strings
       return (WarnPragmaDecl l tuple2NameLs_Strings')
transDecl locals (InlineSig l bool maybeActivationL qNameL)
  = do maybeActivationL' <- mapM (transActivation locals)
                              maybeActivationL
       qNameL' <- transQName locals qNameL
       return (InlineSig l bool maybeActivationL' qNameL')
transDecl locals (InlineConlikeSig l maybeActivationL qNameL)
  = do maybeActivationL' <- mapM (transActivation locals)
                              maybeActivationL
       qNameL' <- transQName locals qNameL
       return (InlineConlikeSig l maybeActivationL' qNameL')
transDecl locals (SpecSig l maybeActivationL qNameL typeLs)
  = do maybeActivationL' <- mapM (transActivation locals)
                              maybeActivationL
       qNameL' <- transQName locals qNameL
       typeLs' <- mapM (transType locals) typeLs
       return (SpecSig l maybeActivationL' qNameL' typeLs')
transDecl locals
  (SpecInlineSig l bool maybeActivationL qNameL typeLs)
  = do maybeActivationL' <- mapM (transActivation locals)
                              maybeActivationL
       qNameL' <- transQName locals qNameL
       typeLs' <- mapM (transType locals) typeLs
       return (SpecInlineSig l bool maybeActivationL' qNameL' typeLs')
transDecl locals (InstSig l instRuleL)
  = do instRuleL' <- transInstRule locals instRuleL
       return (InstSig l instRuleL')
transDecl locals (AnnPragma l annotationL)
  = do annotationL' <- transAnnotation locals annotationL
       return (AnnPragma l annotationL')
transDecl locals (MinimalPragma l maybeBooleanFormulaL)
  = do maybeBooleanFormulaL' <- mapM (transBooleanFormula locals)
                                  maybeBooleanFormulaL
       return (MinimalPragma l maybeBooleanFormulaL')
transDecl locals (RoleAnnotDecl l qNameL roleLs)
  = do qNameL' <- transQName locals qNameL
       roleLs' <- mapM (transRole locals) roleLs
       return (RoleAnnotDecl l qNameL' roleLs')

transDeclHead :: LocalEnv -> DeclHead l -> DesugarM (DeclHead l)
transDeclHead locals (DHead l nameL)
  = do nameL' <- transName locals nameL
       return (DHead l nameL')
transDeclHead locals (DHInfix l tyVarBindL nameL)
  = do tyVarBindL' <- transTyVarBind locals tyVarBindL
       nameL' <- transName locals nameL
       return (DHInfix l tyVarBindL' nameL')
transDeclHead locals (DHParen l declHeadL)
  = do declHeadL' <- transDeclHead locals declHeadL
       return (DHParen l declHeadL')
transDeclHead locals (DHApp l declHeadL tyVarBindL)
  = do declHeadL' <- transDeclHead locals declHeadL
       tyVarBindL' <- transTyVarBind locals tyVarBindL
       return (DHApp l declHeadL' tyVarBindL')

transDeriving :: LocalEnv -> Deriving l -> DesugarM (Deriving l)
transDeriving locals (Deriving l instRuleLs)
  = do instRuleLs' <- mapM (transInstRule locals) instRuleLs
       return (Deriving l instRuleLs')

transExp :: LocalEnv -> Exp l -> DesugarM (Exp l)
transExp locals (Var l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (Var l qNameL')
transExp locals (IPVar l iPNameL)
  = do iPNameL' <- transIPName locals iPNameL
       return (IPVar l iPNameL')
transExp locals (Con l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (Con l qNameL')
transExp locals (Lit l literalL)
  = do literalL' <- transLiteral locals literalL
       return (Lit l literalL')
transExp locals (InfixApp l expL qOpL expL1)
  = do expL' <- transExp locals expL
       qOpL' <- transQOp locals qOpL
       expL1' <- transExp locals expL1
       return (InfixApp l expL' qOpL' expL1')
transExp locals (App l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (App l expL' expL1')
transExp locals (NegApp l expL)
  = do expL' <- transExp locals expL
       return (NegApp l expL')
-- desugar:
transExp locals (Lambda l patLs expL)
  = do (patLs', locals') <- withPats locals patLs
       patLs'' <- mapM (transPat locals') patLs'
       expL' <- transExp locals' expL
       return (Lambda l patLs'' expL')
-- desugar:
transExp locals (Let l bindsL expL)
  = do (bindsL', locals') <- transBinds locals bindsL
       expL' <- transExp locals' expL
       return (Let l bindsL' expL')
transExp locals (If l expL expL1 expL2)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       expL2' <- transExp locals expL2
       return (If l expL' expL1' expL2')
transExp locals (MultiIf l guardedRhsLs)
  = do guardedRhsLs' <- mapM (transGuardedRhs locals) guardedRhsLs
       return (MultiIf l guardedRhsLs')
transExp locals (Case l expL altLs)
  = do expL' <- transExp locals expL
       altLs' <- mapM (transAlt locals) altLs
       return (Case l expL' altLs')
transExp locals (Do l stmtLs)
  = do stmtLs' <- mapM (transStmt locals) stmtLs
       return (Do l stmtLs')
transExp locals (MDo l stmtLs)
  = do stmtLs' <- mapM (transStmt locals) stmtLs
       return (MDo l stmtLs')
transExp locals (Tuple l boxed expLs)
  = do boxed' <- transBoxed locals boxed
       expLs' <- mapM (transExp locals) expLs
       return (Tuple l boxed' expLs')
transExp locals (TupleSection l boxed maybeExpLs)
  = do boxed' <- transBoxed locals boxed
       maybeExpLs' <- mapM (mapM (transExp locals)) maybeExpLs
       return (TupleSection l boxed' maybeExpLs')
transExp locals (List l expLs)
  = do expLs' <- mapM (transExp locals) expLs
       return (List l expLs')
transExp locals (ParArray l expLs)
  = do expLs' <- mapM (transExp locals) expLs
       return (ParArray l expLs')
transExp locals (Paren l expL)
  = do expL' <- transExp locals expL
       return (Paren l expL')
transExp locals (LeftSection l expL qOpL)
  = do expL' <- transExp locals expL
       qOpL' <- transQOp locals qOpL
       return (LeftSection l expL' qOpL')
transExp locals (RightSection l qOpL expL)
  = do qOpL' <- transQOp locals qOpL
       expL' <- transExp locals expL
       return (RightSection l qOpL' expL')
transExp locals (RecConstr l qNameL fieldUpdateLs)
  = do qNameL' <- transQName locals qNameL
       fieldUpdateLs' <- mapM (transFieldUpdate locals) fieldUpdateLs
       return (RecConstr l qNameL' fieldUpdateLs')
transExp locals (RecUpdate l expL fieldUpdateLs)
  = do expL' <- transExp locals expL
       fieldUpdateLs' <- mapM (transFieldUpdate locals) fieldUpdateLs
       return (RecUpdate l expL' fieldUpdateLs')
transExp locals (EnumFrom l expL)
  = do expL' <- transExp locals expL
       return (EnumFrom l expL')
transExp locals (EnumFromTo l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (EnumFromTo l expL' expL1')
transExp locals (EnumFromThen l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (EnumFromThen l expL' expL1')
transExp locals (EnumFromThenTo l expL expL1 expL2)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       expL2' <- transExp locals expL2
       return (EnumFromThenTo l expL' expL1' expL2')
transExp locals (ParArrayFromTo l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (ParArrayFromTo l expL' expL1')
transExp locals (ParArrayFromThenTo l expL expL1 expL2)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       expL2' <- transExp locals expL2
       return (ParArrayFromThenTo l expL' expL1' expL2')
transExp locals (ListComp l expL qualStmtLs)
  = do expL' <- transExp locals expL
       qualStmtLs' <- mapM (transQualStmt locals) qualStmtLs
       return (ListComp l expL' qualStmtLs')
transExp locals (ParComp l expL qualStmtLss)
  = do expL' <- transExp locals expL
       qualStmtLss' <- mapM (mapM (transQualStmt locals)) qualStmtLss
       return (ParComp l expL' qualStmtLss')
transExp locals (ParArrayComp l expL qualStmtLss)
  = do expL' <- transExp locals expL
       qualStmtLss' <- mapM (mapM (transQualStmt locals)) qualStmtLss
       return (ParArrayComp l expL' qualStmtLss')
transExp locals (ExpTypeSig l expL typeL)
  = do expL' <- transExp locals expL
       typeL' <- transType locals typeL
       return (ExpTypeSig l expL' typeL')
transExp locals (VarQuote l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (VarQuote l qNameL')
transExp locals (TypQuote l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (TypQuote l qNameL')
transExp locals (BracketExp l bracketL)
  = do bracketL' <- transBracket locals bracketL
       return (BracketExp l bracketL')
transExp locals (SpliceExp l spliceL)
  = do spliceL' <- transSplice locals spliceL
       return (SpliceExp l spliceL')
transExp locals (QuasiQuote l string string1)
  = return (QuasiQuote l string string1)
transExp locals (XTag l xNameL xAttrLs maybeExpL expLs)
  = do xNameL' <- transXName locals xNameL
       xAttrLs' <- mapM (transXAttr locals) xAttrLs
       maybeExpL' <- mapM (transExp locals) maybeExpL
       expLs' <- mapM (transExp locals) expLs
       return (XTag l xNameL' xAttrLs' maybeExpL' expLs')
transExp locals (XETag l xNameL xAttrLs maybeExpL)
  = do xNameL' <- transXName locals xNameL
       xAttrLs' <- mapM (transXAttr locals) xAttrLs
       maybeExpL' <- mapM (transExp locals) maybeExpL
       return (XETag l xNameL' xAttrLs' maybeExpL')
transExp locals (XPcdata l string) = return (XPcdata l string)
transExp locals (XExpTag l expL)
  = do expL' <- transExp locals expL
       return (XExpTag l expL')
transExp locals (XChildTag l expLs)
  = do expLs' <- mapM (transExp locals) expLs
       return (XChildTag l expLs')
transExp locals (CorePragma l string expL)
  = do expL' <- transExp locals expL
       return (CorePragma l string expL')
transExp locals (SCCPragma l string expL)
  = do expL' <- transExp locals expL
       return (SCCPragma l string expL')
transExp locals
  (GenPragma l string tuple2Int_Int tuple2Int_Int1 expL)
  = do tuple2Int_Int' <- transTupleWith (return, return)
                           tuple2Int_Int
       tuple2Int_Int1' <- transTupleWith (return, return) tuple2Int_Int1
       expL' <- transExp locals expL
       return (GenPragma l string tuple2Int_Int' tuple2Int_Int1' expL')
transExp locals (Proc l patL expL)
  = do patL' <- transPat locals patL
       expL' <- transExp locals expL
       return (Proc l patL' expL')
transExp locals (LeftArrApp l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (LeftArrApp l expL' expL1')
transExp locals (RightArrApp l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (RightArrApp l expL' expL1')
transExp locals (LeftArrHighApp l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (LeftArrHighApp l expL' expL1')
transExp locals (RightArrHighApp l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (RightArrHighApp l expL' expL1')
transExp locals (LCase l altLs)
  = do altLs' <- mapM (transAlt locals) altLs
       return (LCase l altLs')
transExp locals (ExprHole l) = return (ExprHole l)

transExportSpec ::
                LocalEnv -> ExportSpec l -> DesugarM (ExportSpec l)
transExportSpec locals (EVar l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (EVar l qNameL')
transExportSpec locals (EAbs l namespaceL qNameL)
  = do namespaceL' <- transNamespace locals namespaceL
       qNameL' <- transQName locals qNameL
       return (EAbs l namespaceL' qNameL')
transExportSpec locals (EThingAll l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (EThingAll l qNameL')
transExportSpec locals (EThingWith l qNameL cNameLs)
  = do qNameL' <- transQName locals qNameL
       cNameLs' <- mapM (transCName locals) cNameLs
       return (EThingWith l qNameL' cNameLs')
transExportSpec locals (EModuleContents l moduleNameL)
  = do moduleNameL' <- transModuleName locals moduleNameL
       return (EModuleContents l moduleNameL')

transExportSpecList ::
                    LocalEnv -> ExportSpecList l -> DesugarM (ExportSpecList l)
transExportSpecList locals (ExportSpecList l exportSpecLs)
  = do exportSpecLs' <- mapM (transExportSpec locals) exportSpecLs
       return (ExportSpecList l exportSpecLs')

transFieldDecl :: LocalEnv -> FieldDecl l -> DesugarM (FieldDecl l)
transFieldDecl locals (FieldDecl l nameLs typeL)
  = do nameLs' <- mapM (transName locals) nameLs
       typeL' <- transType locals typeL
       return (FieldDecl l nameLs' typeL')

transFieldUpdate ::
                 LocalEnv -> FieldUpdate l -> DesugarM (FieldUpdate l)
transFieldUpdate locals (FieldUpdate l qNameL expL)
  = do qNameL' <- transQName locals qNameL
       expL' <- transExp locals expL
       return (FieldUpdate l qNameL' expL')
transFieldUpdate locals (FieldPun l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (FieldPun l qNameL')
transFieldUpdate locals (FieldWildcard l)
  = return (FieldWildcard l)

transFunDep :: LocalEnv -> FunDep l -> DesugarM (FunDep l)
transFunDep locals (FunDep l nameLs nameLs1)
  = do nameLs' <- mapM (transName locals) nameLs
       nameLs1' <- mapM (transName locals) nameLs1
       return (FunDep l nameLs' nameLs1')

transGadtDecl :: LocalEnv -> GadtDecl l -> DesugarM (GadtDecl l)
transGadtDecl locals (GadtDecl l nameL maybeFieldDeclLs typeL)
  = do nameL' <- transName locals nameL
       maybeFieldDeclLs' <- mapM (mapM (transFieldDecl locals))
                              maybeFieldDeclLs
       typeL' <- transType locals typeL
       return (GadtDecl l nameL' maybeFieldDeclLs' typeL')

transGuardedRhs ::
                LocalEnv -> GuardedRhs l -> DesugarM (GuardedRhs l)
transGuardedRhs locals (GuardedRhs l stmtLs expL)
  = do stmtLs' <- mapM (transStmt locals) stmtLs
       expL' <- transExp locals expL
       return (GuardedRhs l stmtLs' expL')

transIPBind :: LocalEnv -> IPBind l -> DesugarM (IPBind l)
transIPBind locals (IPBind l iPNameL expL)
  = do iPNameL' <- transIPName locals iPNameL
       expL' <- transExp locals expL
       return (IPBind l iPNameL' expL')

transIPName :: LocalEnv -> IPName l -> DesugarM (IPName l)
transIPName locals (IPDup l string) = return (IPDup l string)
transIPName locals (IPLin l string) = return (IPLin l string)

transImportDecl ::
                LocalEnv -> ImportDecl l -> DesugarM (ImportDecl l)
transImportDecl locals
  (ImportDecl l moduleNameL bool bool1 bool2 maybeString
     maybeModuleNameL maybeImportSpecListL)
  = do moduleNameL' <- transModuleName locals moduleNameL
       maybeString' <- mapM return maybeString
       maybeModuleNameL' <- mapM (transModuleName locals) maybeModuleNameL
       maybeImportSpecListL' <- mapM (transImportSpecList locals)
                                  maybeImportSpecListL
       return
         (ImportDecl l moduleNameL' bool bool1 bool2 maybeString'
            maybeModuleNameL'
            maybeImportSpecListL')

transImportSpec ::
                LocalEnv -> ImportSpec l -> DesugarM (ImportSpec l)
transImportSpec locals (IVar l nameL)
  = do nameL' <- transName locals nameL
       return (IVar l nameL')
transImportSpec locals (IAbs l namespaceL nameL)
  = do namespaceL' <- transNamespace locals namespaceL
       nameL' <- transName locals nameL
       return (IAbs l namespaceL' nameL')
transImportSpec locals (IThingAll l nameL)
  = do nameL' <- transName locals nameL
       return (IThingAll l nameL')
transImportSpec locals (IThingWith l nameL cNameLs)
  = do nameL' <- transName locals nameL
       cNameLs' <- mapM (transCName locals) cNameLs
       return (IThingWith l nameL' cNameLs')

transImportSpecList ::
                    LocalEnv -> ImportSpecList l -> DesugarM (ImportSpecList l)
transImportSpecList locals (ImportSpecList l bool importSpecLs)
  = do importSpecLs' <- mapM (transImportSpec locals) importSpecLs
       return (ImportSpecList l bool importSpecLs')

transInstDecl :: LocalEnv -> InstDecl l -> DesugarM (InstDecl l)
transInstDecl locals (InsDecl l declL)
  = do declL' <- transDecl locals declL
       return (InsDecl l declL')
transInstDecl locals (InsType l typeL typeL1)
  = do typeL' <- transType locals typeL
       typeL1' <- transType locals typeL1
       return (InsType l typeL' typeL1')
transInstDecl locals
  (InsData l dataOrNewL typeL qualConDeclLs maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew locals dataOrNewL
       typeL' <- transType locals typeL
       qualConDeclLs' <- mapM (transQualConDecl locals) qualConDeclLs
       maybeDerivingL' <- mapM (transDeriving locals) maybeDerivingL
       return
         (InsData l dataOrNewL' typeL' qualConDeclLs' maybeDerivingL')
transInstDecl locals
  (InsGData l dataOrNewL typeL maybeKindL gadtDeclLs maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew locals dataOrNewL
       typeL' <- transType locals typeL
       maybeKindL' <- mapM (transKind locals) maybeKindL
       gadtDeclLs' <- mapM (transGadtDecl locals) gadtDeclLs
       maybeDerivingL' <- mapM (transDeriving locals) maybeDerivingL
       return
         (InsGData l dataOrNewL' typeL' maybeKindL' gadtDeclLs'
            maybeDerivingL')

transInstHead :: LocalEnv -> InstHead l -> DesugarM (InstHead l)
transInstHead locals (IHCon l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (IHCon l qNameL')
transInstHead locals (IHInfix l typeL qNameL)
  = do typeL' <- transType locals typeL
       qNameL' <- transQName locals qNameL
       return (IHInfix l typeL' qNameL')
transInstHead locals (IHParen l instHeadL)
  = do instHeadL' <- transInstHead locals instHeadL
       return (IHParen l instHeadL')
transInstHead locals (IHApp l instHeadL typeL)
  = do instHeadL' <- transInstHead locals instHeadL
       typeL' <- transType locals typeL
       return (IHApp l instHeadL' typeL')

transInstRule :: LocalEnv -> InstRule l -> DesugarM (InstRule l)
transInstRule locals
  (IRule l maybeTyVarBindLs maybeContextL instHeadL)
  = do maybeTyVarBindLs' <- mapM (mapM (transTyVarBind locals))
                              maybeTyVarBindLs
       maybeContextL' <- mapM (transContext locals) maybeContextL
       instHeadL' <- transInstHead locals instHeadL
       return (IRule l maybeTyVarBindLs' maybeContextL' instHeadL')
transInstRule locals (IParen l instRuleL)
  = do instRuleL' <- transInstRule locals instRuleL
       return (IParen l instRuleL')

transKind :: LocalEnv -> Kind l -> DesugarM (Kind l)
transKind locals (KindStar l) = return (KindStar l)
transKind locals (KindFn l kindL kindL1)
  = do kindL' <- transKind locals kindL
       kindL1' <- transKind locals kindL1
       return (KindFn l kindL' kindL1')
transKind locals (KindParen l kindL)
  = do kindL' <- transKind locals kindL
       return (KindParen l kindL')
transKind locals (KindVar l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (KindVar l qNameL')
transKind locals (KindApp l kindL kindL1)
  = do kindL' <- transKind locals kindL
       kindL1' <- transKind locals kindL1
       return (KindApp l kindL' kindL1')
transKind locals (KindTuple l kindLs)
  = do kindLs' <- mapM (transKind locals) kindLs
       return (KindTuple l kindLs')
transKind locals (KindList l kindL)
  = do kindL' <- transKind locals kindL
       return (KindList l kindL')

transLiteral :: LocalEnv -> Literal l -> DesugarM (Literal l)
transLiteral locals (Char l char string)
  = return (Char l char string)
transLiteral locals (String l string string1)
  = return (String l string string1)
transLiteral locals (Int l integer string)
  = return (Int l integer string)
transLiteral locals (Frac l rational string)
  = return (Frac l rational string)
transLiteral locals (PrimInt l integer string)
  = return (PrimInt l integer string)
transLiteral locals (PrimWord l integer string)
  = return (PrimWord l integer string)
transLiteral locals (PrimFloat l rational string)
  = return (PrimFloat l rational string)
transLiteral locals (PrimDouble l rational string)
  = return (PrimDouble l rational string)
transLiteral locals (PrimChar l char string)
  = return (PrimChar l char string)
transLiteral locals (PrimString l string string1)
  = return (PrimString l string string1)

transMatch :: LocalEnv -> Match l -> DesugarM (Match l)
transMatch locals _
    = error "transMatch: unsupported match"

transModule :: LocalEnv -> Module l -> DesugarM (Module l)
transModule locals
  (Module l maybeModuleHeadL modulePragmaLs importDeclLs declLs)
  = do maybeModuleHeadL' <- mapM (transModuleHead locals)
                              maybeModuleHeadL
       modulePragmaLs' <- mapM (transModulePragma locals) modulePragmaLs
       importDeclLs' <- mapM (transImportDecl locals) importDeclLs
       declLs' <- mapM (transDecl locals) declLs
       return
         (Module l maybeModuleHeadL' modulePragmaLs' importDeclLs' declLs')
transModule locals
  (XmlPage l moduleNameL modulePragmaLs xNameL xAttrLs maybeExpL
     expLs)
  = do moduleNameL' <- transModuleName locals moduleNameL
       modulePragmaLs' <- mapM (transModulePragma locals) modulePragmaLs
       xNameL' <- transXName locals xNameL
       xAttrLs' <- mapM (transXAttr locals) xAttrLs
       maybeExpL' <- mapM (transExp locals) maybeExpL
       expLs' <- mapM (transExp locals) expLs
       return
         (XmlPage l moduleNameL' modulePragmaLs' xNameL' xAttrLs' maybeExpL'
            expLs')
transModule locals
  (XmlHybrid l maybeModuleHeadL modulePragmaLs importDeclLs declLs
     xNameL xAttrLs maybeExpL expLs)
  = do maybeModuleHeadL' <- mapM (transModuleHead locals)
                              maybeModuleHeadL
       modulePragmaLs' <- mapM (transModulePragma locals) modulePragmaLs
       importDeclLs' <- mapM (transImportDecl locals) importDeclLs
       declLs' <- mapM (transDecl locals) declLs
       xNameL' <- transXName locals xNameL
       xAttrLs' <- mapM (transXAttr locals) xAttrLs
       maybeExpL' <- mapM (transExp locals) maybeExpL
       expLs' <- mapM (transExp locals) expLs
       return
         (XmlHybrid l maybeModuleHeadL' modulePragmaLs' importDeclLs'
            declLs'
            xNameL'
            xAttrLs'
            maybeExpL'
            expLs')

transModuleHead ::
                LocalEnv -> ModuleHead l -> DesugarM (ModuleHead l)
transModuleHead locals
  (ModuleHead l moduleNameL maybeWarningTextL maybeExportSpecListL)
  = do moduleNameL' <- transModuleName locals moduleNameL
       maybeWarningTextL' <- mapM (transWarningText locals)
                               maybeWarningTextL
       maybeExportSpecListL' <- mapM (transExportSpecList locals)
                                  maybeExportSpecListL
       return
         (ModuleHead l moduleNameL' maybeWarningTextL'
            maybeExportSpecListL')

transModuleName ::
                LocalEnv -> ModuleName l -> DesugarM (ModuleName l)
transModuleName locals (ModuleName l string)
  = return (ModuleName l string)

transModulePragma ::
                  LocalEnv -> ModulePragma l -> DesugarM (ModulePragma l)
transModulePragma locals (LanguagePragma l nameLs)
  = do nameLs' <- mapM (transName locals) nameLs
       return (LanguagePragma l nameLs')
transModulePragma locals (OptionsPragma l maybeTool string)
  = do maybeTool' <- mapM (transTool locals) maybeTool
       return (OptionsPragma l maybeTool' string)
transModulePragma locals (AnnModulePragma l annotationL)
  = do annotationL' <- transAnnotation locals annotationL
       return (AnnModulePragma l annotationL')

-- desugar: 將 top-level name 改成 full-qualified
transName :: LocalEnv -> Name l -> DesugarM (Name l)
transName locals name = do
    qualified <- lookup name locals
    return (modifyName (const qualified) name)

transNamespace :: LocalEnv -> Namespace l -> DesugarM (Namespace l)
transNamespace locals (NoNamespace l) = return (NoNamespace l)
transNamespace locals (TypeNamespace l) = return (TypeNamespace l)
transNamespace locals (PatternNamespace l)
  = return (PatternNamespace l)

transOp :: LocalEnv -> Op l -> DesugarM (Op l)
transOp locals (VarOp l nameL)
  = do nameL' <- transName locals nameL
       return (VarOp l nameL')
transOp locals (ConOp l nameL)
  = do nameL' <- transName locals nameL
       return (ConOp l nameL')

transOverlap :: LocalEnv -> Overlap l -> DesugarM (Overlap l)
transOverlap locals (NoOverlap l) = return (NoOverlap l)
transOverlap locals (Overlap l) = return (Overlap l)
transOverlap locals (Incoherent l) = return (Incoherent l)

transPXAttr :: LocalEnv -> PXAttr l -> DesugarM (PXAttr l)
transPXAttr locals (PXAttr l xNameL patL)
  = do xNameL' <- transXName locals xNameL
       patL' <- transPat locals patL
       return (PXAttr l xNameL' patL')

transPat :: LocalEnv -> Pat l -> DesugarM (Pat l)
transPat locals (PVar l nameL)
  = do nameL' <- transName locals nameL
       return (PVar l nameL')
transPat locals (PLit l signL literalL)
  = do signL' <- transSign locals signL
       literalL' <- transLiteral locals literalL
       return (PLit l signL' literalL')
transPat locals (PNPlusK l nameL integer)
  = do nameL' <- transName locals nameL
       return (PNPlusK l nameL' integer)
transPat locals (PInfixApp l patL qNameL patL1)
  = do patL' <- transPat locals patL
       qNameL' <- transQName locals qNameL
       patL1' <- transPat locals patL1
       return (PInfixApp l patL' qNameL' patL1')
transPat locals (PApp l qNameL patLs)
  = do qNameL' <- transQName locals qNameL
       patLs' <- mapM (transPat locals) patLs
       return (PApp l qNameL' patLs')
transPat locals (PTuple l boxed patLs)
  = do boxed' <- transBoxed locals boxed
       patLs' <- mapM (transPat locals) patLs
       return (PTuple l boxed' patLs')
transPat locals (PList l patLs)
  = do patLs' <- mapM (transPat locals) patLs
       return (PList l patLs')
transPat locals (PParen l patL)
  = do patL' <- transPat locals patL
       return (PParen l patL')
transPat locals (PRec l qNameL patFieldLs)
  = do qNameL' <- transQName locals qNameL
       patFieldLs' <- mapM (transPatField locals) patFieldLs
       return (PRec l qNameL' patFieldLs')
transPat locals (PAsPat l nameL patL)
  = do nameL' <- transName locals nameL
       patL' <- transPat locals patL
       return (PAsPat l nameL' patL')
transPat locals (PWildCard l) = return (PWildCard l)
transPat locals (PIrrPat l patL)
  = do patL' <- transPat locals patL
       return (PIrrPat l patL')
transPat locals (PatTypeSig l patL typeL)
  = do patL' <- transPat locals patL
       typeL' <- transType locals typeL
       return (PatTypeSig l patL' typeL')
transPat locals (PViewPat l expL patL)
  = do expL' <- transExp locals expL
       patL' <- transPat locals patL
       return (PViewPat l expL' patL')
transPat locals (PRPat l rPatLs)
  = do rPatLs' <- mapM (transRPat locals) rPatLs
       return (PRPat l rPatLs')
transPat locals (PXTag l xNameL pXAttrLs maybePatL patLs)
  = do xNameL' <- transXName locals xNameL
       pXAttrLs' <- mapM (transPXAttr locals) pXAttrLs
       maybePatL' <- mapM (transPat locals) maybePatL
       patLs' <- mapM (transPat locals) patLs
       return (PXTag l xNameL' pXAttrLs' maybePatL' patLs')
transPat locals (PXETag l xNameL pXAttrLs maybePatL)
  = do xNameL' <- transXName locals xNameL
       pXAttrLs' <- mapM (transPXAttr locals) pXAttrLs
       maybePatL' <- mapM (transPat locals) maybePatL
       return (PXETag l xNameL' pXAttrLs' maybePatL')
transPat locals (PXPcdata l string) = return (PXPcdata l string)
transPat locals (PXPatTag l patL)
  = do patL' <- transPat locals patL
       return (PXPatTag l patL')
transPat locals (PXRPats l rPatLs)
  = do rPatLs' <- mapM (transRPat locals) rPatLs
       return (PXRPats l rPatLs')
transPat locals (PQuasiQuote l string string1)
  = return (PQuasiQuote l string string1)
transPat locals (PBangPat l patL)
  = do patL' <- transPat locals patL
       return (PBangPat l patL')

transPatField :: LocalEnv -> PatField l -> DesugarM (PatField l)
transPatField locals (PFieldPat l qNameL patL)
  = do qNameL' <- transQName locals qNameL
       patL' <- transPat locals patL
       return (PFieldPat l qNameL' patL')
transPatField locals (PFieldPun l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (PFieldPun l qNameL')
transPatField locals (PFieldWildcard l) = return (PFieldWildcard l)

transPatternSynDirection ::
                         LocalEnv ->
                           PatternSynDirection l -> DesugarM (PatternSynDirection l)
transPatternSynDirection locals Unidirectional
  = return Unidirectional
transPatternSynDirection locals ImplicitBidirectional
  = return ImplicitBidirectional
transPatternSynDirection locals (ExplicitBidirectional l declLs)
  = do declLs' <- mapM (transDecl locals) declLs
       return (ExplicitBidirectional l declLs')

transPromoted :: LocalEnv -> Promoted l -> DesugarM (Promoted l)
transPromoted locals (PromotedInteger l integer string)
  = return (PromotedInteger l integer string)
transPromoted locals (PromotedString l string string1)
  = return (PromotedString l string string1)
transPromoted locals (PromotedCon l bool qNameL)
  = do qNameL' <- transQName locals qNameL
       return (PromotedCon l bool qNameL')
transPromoted locals (PromotedList l bool typeLs)
  = do typeLs' <- mapM (transType locals) typeLs
       return (PromotedList l bool typeLs')
transPromoted locals (PromotedTuple l typeLs)
  = do typeLs' <- mapM (transType locals) typeLs
       return (PromotedTuple l typeLs')
transPromoted locals (PromotedUnit l) = return (PromotedUnit l)

-- desugar:
transQName :: LocalEnv -> QName l -> DesugarM (QName l)
transQName locals (Qual l (ModuleName _ modName) nameL)
  = do nameL' <- transName locals $
                    modifyName ((modName ++ ".") ++) nameL
       return . UnQual l $ nameL'
transQName locals (UnQual l nameL)
  = do nameL' <- transName locals nameL
       return (UnQual l nameL')
transQName locals (Special l specialConL)
  = do specialConL' <- transSpecialCon locals specialConL
       return (Special l specialConL')

transQOp :: LocalEnv -> QOp l -> DesugarM (QOp l)
transQOp locals (QVarOp l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (QVarOp l qNameL')
transQOp locals (QConOp l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (QConOp l qNameL')

transQualConDecl ::
                 LocalEnv -> QualConDecl l -> DesugarM (QualConDecl l)
transQualConDecl locals
  (QualConDecl l maybeTyVarBindLs maybeContextL conDeclL)
  = do maybeTyVarBindLs' <- mapM (mapM (transTyVarBind locals))
                              maybeTyVarBindLs
       maybeContextL' <- mapM (transContext locals) maybeContextL
       conDeclL' <- transConDecl locals conDeclL
       return (QualConDecl l maybeTyVarBindLs' maybeContextL' conDeclL')

transQualStmt :: LocalEnv -> QualStmt l -> DesugarM (QualStmt l)
transQualStmt locals (QualStmt l stmtL)
  = do stmtL' <- transStmt locals stmtL
       return (QualStmt l stmtL')
transQualStmt locals (ThenTrans l expL)
  = do expL' <- transExp locals expL
       return (ThenTrans l expL')
transQualStmt locals (ThenBy l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (ThenBy l expL' expL1')
transQualStmt locals (GroupBy l expL)
  = do expL' <- transExp locals expL
       return (GroupBy l expL')
transQualStmt locals (GroupUsing l expL)
  = do expL' <- transExp locals expL
       return (GroupUsing l expL')
transQualStmt locals (GroupByUsing l expL expL1)
  = do expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return (GroupByUsing l expL' expL1')

transRPat :: LocalEnv -> RPat l -> DesugarM (RPat l)
transRPat locals (RPOp l rPatL rPatOpL)
  = do rPatL' <- transRPat locals rPatL
       rPatOpL' <- transRPatOp locals rPatOpL
       return (RPOp l rPatL' rPatOpL')
transRPat locals (RPEither l rPatL rPatL1)
  = do rPatL' <- transRPat locals rPatL
       rPatL1' <- transRPat locals rPatL1
       return (RPEither l rPatL' rPatL1')
transRPat locals (RPSeq l rPatLs)
  = do rPatLs' <- mapM (transRPat locals) rPatLs
       return (RPSeq l rPatLs')
transRPat locals (RPGuard l patL stmtLs)
  = do patL' <- transPat locals patL
       stmtLs' <- mapM (transStmt locals) stmtLs
       return (RPGuard l patL' stmtLs')
transRPat locals (RPCAs l nameL rPatL)
  = do nameL' <- transName locals nameL
       rPatL' <- transRPat locals rPatL
       return (RPCAs l nameL' rPatL')
transRPat locals (RPAs l nameL rPatL)
  = do nameL' <- transName locals nameL
       rPatL' <- transRPat locals rPatL
       return (RPAs l nameL' rPatL')
transRPat locals (RPParen l rPatL)
  = do rPatL' <- transRPat locals rPatL
       return (RPParen l rPatL')
transRPat locals (RPPat l patL)
  = do patL' <- transPat locals patL
       return (RPPat l patL')

transRPatOp :: LocalEnv -> RPatOp l -> DesugarM (RPatOp l)
transRPatOp locals (RPStar l) = return (RPStar l)
transRPatOp locals (RPStarG l) = return (RPStarG l)
transRPatOp locals (RPPlus l) = return (RPPlus l)
transRPatOp locals (RPPlusG l) = return (RPPlusG l)
transRPatOp locals (RPOpt l) = return (RPOpt l)
transRPatOp locals (RPOptG l) = return (RPOptG l)

transRhs :: LocalEnv -> Rhs l -> DesugarM (Rhs l)
transRhs locals (UnGuardedRhs l expL)
  = do expL' <- transExp locals expL
       return (UnGuardedRhs l expL')
transRhs locals (GuardedRhss l guardedRhsLs)
  = do guardedRhsLs' <- mapM (transGuardedRhs locals) guardedRhsLs
       return (GuardedRhss l guardedRhsLs')

transRole :: LocalEnv -> Role l -> DesugarM (Role l)
transRole locals (Nominal l) = return (Nominal l)
transRole locals (Representational l) = return (Representational l)
transRole locals (Phantom l) = return (Phantom l)
transRole locals (RoleWildcard l) = return (RoleWildcard l)

transRule :: LocalEnv -> Rule l -> DesugarM (Rule l)
transRule locals
  (Rule l string maybeActivationL maybeRuleVarLs expL expL1)
  = do maybeActivationL' <- mapM (transActivation locals)
                              maybeActivationL
       maybeRuleVarLs' <- mapM (mapM (transRuleVar locals)) maybeRuleVarLs
       expL' <- transExp locals expL
       expL1' <- transExp locals expL1
       return
         (Rule l string maybeActivationL' maybeRuleVarLs' expL' expL1')

transRuleVar :: LocalEnv -> RuleVar l -> DesugarM (RuleVar l)
transRuleVar locals (RuleVar l nameL)
  = do nameL' <- transName locals nameL
       return (RuleVar l nameL')
transRuleVar locals (TypedRuleVar l nameL typeL)
  = do nameL' <- transName locals nameL
       typeL' <- transType locals typeL
       return (TypedRuleVar l nameL' typeL')

transSafety :: LocalEnv -> Safety l -> DesugarM (Safety l)
transSafety locals (PlayRisky l) = return (PlayRisky l)
transSafety locals (PlaySafe l bool) = return (PlaySafe l bool)
transSafety locals (PlayInterruptible l)
  = return (PlayInterruptible l)

transSign :: LocalEnv -> Sign l -> DesugarM (Sign l)
transSign locals (Signless l) = return (Signless l)
transSign locals (Negative l) = return (Negative l)

transSpecialCon ::
                LocalEnv -> SpecialCon l -> DesugarM (SpecialCon l)
transSpecialCon locals (UnitCon l) = return (UnitCon l)
transSpecialCon locals (ListCon l) = return (ListCon l)
transSpecialCon locals (FunCon l) = return (FunCon l)
transSpecialCon locals (TupleCon l boxed int)
  = do boxed' <- transBoxed locals boxed
       return (TupleCon l boxed' int)
transSpecialCon locals (Cons l) = return (Cons l)
transSpecialCon locals (UnboxedSingleCon l)
  = return (UnboxedSingleCon l)

transSplice :: LocalEnv -> Splice l -> DesugarM (Splice l)
transSplice locals (IdSplice l string) = return (IdSplice l string)
transSplice locals (ParenSplice l expL)
  = do expL' <- transExp locals expL
       return (ParenSplice l expL')

-- TODO:
transStmt :: LocalEnv -> Stmt l -> DesugarM (Stmt l)
transStmt locals (Generator l patL expL)
  = do patL' <- transPat locals patL
       expL' <- transExp locals expL
       return (Generator l patL' expL')
transStmt locals (Qualifier l expL)
  = do expL' <- transExp locals expL
       return (Qualifier l expL')
transStmt locals (LetStmt l bindsL)
  = do (bindsL', _) <- transBinds locals bindsL
       return (LetStmt l bindsL')
transStmt locals (RecStmt l stmtLs)
  = do stmtLs' <- mapM (transStmt locals) stmtLs
       return (RecStmt l stmtLs')

transTool :: LocalEnv -> Tool -> DesugarM Tool
transTool locals GHC = return GHC
transTool locals HUGS = return HUGS
transTool locals NHC98 = return NHC98
transTool locals YHC = return YHC
transTool locals HADDOCK = return HADDOCK
transTool locals (UnknownTool string) = return (UnknownTool string)

transTyVarBind :: LocalEnv -> TyVarBind l -> DesugarM (TyVarBind l)
transTyVarBind locals (KindedVar l nameL kindL)
  = do nameL' <- transName locals nameL
       kindL' <- transKind locals kindL
       return (KindedVar l nameL' kindL')
transTyVarBind locals (UnkindedVar l nameL)
  = do nameL' <- transName locals nameL
       return (UnkindedVar l nameL')

transType :: LocalEnv -> Type l -> DesugarM (Type l)
transType locals (TyForall l maybeTyVarBindLs maybeContextL typeL)
  = do maybeTyVarBindLs' <- mapM (mapM (transTyVarBind locals))
                              maybeTyVarBindLs
       maybeContextL' <- mapM (transContext locals) maybeContextL
       typeL' <- transType locals typeL
       return (TyForall l maybeTyVarBindLs' maybeContextL' typeL')
transType locals (TyFun l typeL typeL1)
  = do typeL' <- transType locals typeL
       typeL1' <- transType locals typeL1
       return (TyFun l typeL' typeL1')
transType locals (TyTuple l boxed typeLs)
  = do boxed' <- transBoxed locals boxed
       typeLs' <- mapM (transType locals) typeLs
       return (TyTuple l boxed' typeLs')
transType locals (TyList l typeL)
  = do typeL' <- transType locals typeL
       return (TyList l typeL')
transType locals (TyParArray l typeL)
  = do typeL' <- transType locals typeL
       return (TyParArray l typeL')
transType locals (TyApp l typeL typeL1)
  = do typeL' <- transType locals typeL
       typeL1' <- transType locals typeL1
       return (TyApp l typeL' typeL1')
-- desugar:
transType locals v@(TyVar l nameL@(extractName -> n))
-- FIXME: 小寫的 type var 直接當成 local variable 不 rename
  | isLower . head $ n = return v
  | otherwise =  do
       nameL' <- transName locals nameL
       return (TyVar l nameL')
transType locals (TyCon l qNameL)
  = do qNameL' <- transQName locals qNameL
       return (TyCon l qNameL')
transType locals (TyParen l typeL)
  = do typeL' <- transType locals typeL
       return (TyParen l typeL')
transType locals (TyInfix l typeL qNameL typeL1)
  = do typeL' <- transType locals typeL
       qNameL' <- transQName locals qNameL
       typeL1' <- transType locals typeL1
       return (TyInfix l typeL' qNameL' typeL1')
transType locals (TyKind l typeL kindL)
  = do typeL' <- transType locals typeL
       kindL' <- transKind locals kindL
       return (TyKind l typeL' kindL')
transType locals (TyPromoted l promotedL)
  = do promotedL' <- transPromoted locals promotedL
       return (TyPromoted l promotedL')
transType locals (TyEquals l typeL typeL1)
  = do typeL' <- transType locals typeL
       typeL1' <- transType locals typeL1
       return (TyEquals l typeL' typeL1')
transType locals (TySplice l spliceL)
  = do spliceL' <- transSplice locals spliceL
       return (TySplice l spliceL')
transType locals (TyBang l bangTypeL typeL)
  = do bangTypeL' <- transBangType locals bangTypeL
       typeL' <- transType locals typeL
       return (TyBang l bangTypeL' typeL')
transType locals (TyWildCard l maybeNameL)
  = do maybeNameL' <- mapM (transName locals) maybeNameL
       return (TyWildCard l maybeNameL')

transTypeEqn :: LocalEnv -> TypeEqn l -> DesugarM (TypeEqn l)
transTypeEqn locals (TypeEqn l typeL typeL1)
  = do typeL' <- transType locals typeL
       typeL1' <- transType locals typeL1
       return (TypeEqn l typeL' typeL1')

transWarningText ::
                 LocalEnv -> WarningText l -> DesugarM (WarningText l)
transWarningText locals (DeprText l string)
  = return (DeprText l string)
transWarningText locals (WarnText l string)
  = return (WarnText l string)

transXAttr :: LocalEnv -> XAttr l -> DesugarM (XAttr l)
transXAttr locals (XAttr l xNameL expL)
  = do xNameL' <- transXName locals xNameL
       expL' <- transExp locals expL
       return (XAttr l xNameL' expL')

transXName :: LocalEnv -> XName l -> DesugarM (XName l)
transXName locals (XName l string) = return (XName l string)
transXName locals (XDomName l string string1)
  = return (XDomName l string string1)
