{-
這個 pass

1. 先記下 module name

2. 蒐集所有 top-level definition 的名字，包含下列：

    * PatBind (一個變數的)
    * GDataDecl
    * GadtDecl

-}

module Desugar.Rename.CollectGlobal where
import Desugar.Monad
import Language.Haskell.Exts.Annotated

import Control.Monad (void)

withPat :: Pat SrcSpanInfo -> DesugarM ()
withPat (PVar l name) = do
    qualified <- moduleDefined name
    addGlobal name qualified
withPat (PParen l pat) = withPat pat
withPat (PApp l qname pats) = withPats pats
withPat p = error $ show $ void p

withPats :: [Pat SrcSpanInfo] -> DesugarM ()
withPats = mapM_ withPat

desugarCollectGlobal :: Module SrcSpanInfo -> DesugarM (Module SrcSpanInfo)
desugarCollectGlobal = transModule

transActivation ::
                Activation SrcSpanInfo -> DesugarM (Activation SrcSpanInfo)
transActivation (ActiveFrom l int) = return (ActiveFrom l int)
transActivation (ActiveUntil l int) = return (ActiveUntil l int)

transAlt :: Alt SrcSpanInfo -> DesugarM (Alt SrcSpanInfo)
transAlt (Alt l patL rhsL maybeBindsL)
  = do patL' <- transPat patL
       rhsL' <- transRhs rhsL
       maybeBindsL' <- mapM transBinds maybeBindsL
       return (Alt l patL' rhsL' maybeBindsL')

transAnnotation ::
                Annotation SrcSpanInfo -> DesugarM (Annotation SrcSpanInfo)
transAnnotation (Ann l nameL expL)
  = do nameL' <- transName nameL
       expL' <- transExp expL
       return (Ann l nameL' expL')
transAnnotation (TypeAnn l nameL expL)
  = do nameL' <- transName nameL
       expL' <- transExp expL
       return (TypeAnn l nameL' expL')
transAnnotation (ModuleAnn l expL)
  = do expL' <- transExp expL
       return (ModuleAnn l expL')

transAssoc :: Assoc SrcSpanInfo -> DesugarM (Assoc SrcSpanInfo)
transAssoc (AssocNone l) = return (AssocNone l)
transAssoc (AssocLeft l) = return (AssocLeft l)
transAssoc (AssocRight l) = return (AssocRight l)

transAsst :: Asst SrcSpanInfo -> DesugarM (Asst SrcSpanInfo)
transAsst (ClassA l qNameL typeLs)
  = do qNameL' <- transQName qNameL
       typeLs' <- mapM transType typeLs
       return (ClassA l qNameL' typeLs')
transAsst (AppA l nameL typeLs)
  = do nameL' <- transName nameL
       typeLs' <- mapM transType typeLs
       return (AppA l nameL' typeLs')
transAsst (InfixA l typeL qNameL typeL1)
  = do typeL' <- transType typeL
       qNameL' <- transQName qNameL
       typeL1' <- transType typeL1
       return (InfixA l typeL' qNameL' typeL1')
transAsst (IParam l iPNameL typeL)
  = do iPNameL' <- transIPName iPNameL
       typeL' <- transType typeL
       return (IParam l iPNameL' typeL')
transAsst (EqualP l typeL typeL1)
  = do typeL' <- transType typeL
       typeL1' <- transType typeL1
       return (EqualP l typeL' typeL1')
transAsst (ParenA l asstL)
  = do asstL' <- transAsst asstL
       return (ParenA l asstL')
transAsst (WildCardA l maybeNameL)
  = do maybeNameL' <- mapM transName maybeNameL
       return (WildCardA l maybeNameL')

transBangType ::
              BangType SrcSpanInfo -> DesugarM (BangType SrcSpanInfo)
transBangType (BangedTy l) = return (BangedTy l)
transBangType (UnpackedTy l) = return (UnpackedTy l)

transBinds :: Binds SrcSpanInfo -> DesugarM (Binds SrcSpanInfo)
transBinds (BDecls l declLs)
  = do declLs' <- mapM transDecl declLs
       return (BDecls l declLs')
transBinds (IPBinds l iPBindLs)
  = do iPBindLs' <- mapM transIPBind iPBindLs
       return (IPBinds l iPBindLs')

transBooleanFormula ::
                    BooleanFormula SrcSpanInfo -> DesugarM (BooleanFormula SrcSpanInfo)
transBooleanFormula (VarFormula l nameL)
  = do nameL' <- transName nameL
       return (VarFormula l nameL')
transBooleanFormula (AndFormula l booleanFormulaLs)
  = do booleanFormulaLs' <- mapM transBooleanFormula booleanFormulaLs
       return (AndFormula l booleanFormulaLs')
transBooleanFormula (OrFormula l booleanFormulaLs)
  = do booleanFormulaLs' <- mapM transBooleanFormula booleanFormulaLs
       return (OrFormula l booleanFormulaLs')
transBooleanFormula (ParenFormula l booleanFormulaL)
  = do booleanFormulaL' <- transBooleanFormula booleanFormulaL
       return (ParenFormula l booleanFormulaL')

transBoxed :: Boxed -> DesugarM Boxed
transBoxed Boxed = return Boxed
transBoxed Unboxed = return Unboxed

transBracket ::
             Bracket SrcSpanInfo -> DesugarM (Bracket SrcSpanInfo)
transBracket (ExpBracket l expL)
  = do expL' <- transExp expL
       return (ExpBracket l expL')
transBracket (PatBracket l patL)
  = do patL' <- transPat patL
       return (PatBracket l patL')
transBracket (TypeBracket l typeL)
  = do typeL' <- transType typeL
       return (TypeBracket l typeL')
transBracket (DeclBracket l declLs)
  = do declLs' <- mapM transDecl declLs
       return (DeclBracket l declLs')

transCName :: CName SrcSpanInfo -> DesugarM (CName SrcSpanInfo)
transCName (VarName l nameL)
  = do nameL' <- transName nameL
       return (VarName l nameL')
transCName (ConName l nameL)
  = do nameL' <- transName nameL
       return (ConName l nameL')

transCallConv ::
              CallConv SrcSpanInfo -> DesugarM (CallConv SrcSpanInfo)
transCallConv (StdCall l) = return (StdCall l)
transCallConv (CCall l) = return (CCall l)
transCallConv (CPlusPlus l) = return (CPlusPlus l)
transCallConv (DotNet l) = return (DotNet l)
transCallConv (Jvm l) = return (Jvm l)
transCallConv (Js l) = return (Js l)
transCallConv (JavaScript l) = return (JavaScript l)
transCallConv (CApi l) = return (CApi l)

transClassDecl ::
               ClassDecl SrcSpanInfo -> DesugarM (ClassDecl SrcSpanInfo)
transClassDecl (ClsDecl l declL)
  = do declL' <- transDecl declL
       return (ClsDecl l declL')
transClassDecl (ClsDataFam l maybeContextL declHeadL maybeKindL)
  = do maybeContextL' <- mapM transContext maybeContextL
       declHeadL' <- transDeclHead declHeadL
       maybeKindL' <- mapM transKind maybeKindL
       return (ClsDataFam l maybeContextL' declHeadL' maybeKindL')
transClassDecl (ClsTyFam l declHeadL maybeKindL)
  = do declHeadL' <- transDeclHead declHeadL
       maybeKindL' <- mapM transKind maybeKindL
       return (ClsTyFam l declHeadL' maybeKindL')
transClassDecl (ClsTyDef l typeL typeL1)
  = do typeL' <- transType typeL
       typeL1' <- transType typeL1
       return (ClsTyDef l typeL' typeL1')
transClassDecl (ClsDefSig l nameL typeL)
  = do nameL' <- transName nameL
       typeL' <- transType typeL
       return (ClsDefSig l nameL' typeL')

transConDecl ::
             ConDecl SrcSpanInfo -> DesugarM (ConDecl SrcSpanInfo)
transConDecl (ConDecl l nameL typeLs)
  = do nameL' <- transName nameL
       typeLs' <- mapM transType typeLs
       return (ConDecl l nameL' typeLs')
transConDecl (InfixConDecl l typeL nameL typeL1)
  = do typeL' <- transType typeL
       nameL' <- transName nameL
       typeL1' <- transType typeL1
       return (InfixConDecl l typeL' nameL' typeL1')
transConDecl (RecDecl l nameL fieldDeclLs)
  = do nameL' <- transName nameL
       fieldDeclLs' <- mapM transFieldDecl fieldDeclLs
       return (RecDecl l nameL' fieldDeclLs')

transContext ::
             Context SrcSpanInfo -> DesugarM (Context SrcSpanInfo)
transContext (CxSingle l asstL)
  = do asstL' <- transAsst asstL
       return (CxSingle l asstL')
transContext (CxTuple l asstLs)
  = do asstLs' <- mapM transAsst asstLs
       return (CxTuple l asstLs')
transContext (CxEmpty l) = return (CxEmpty l)

transDataOrNew ::
               DataOrNew SrcSpanInfo -> DesugarM (DataOrNew SrcSpanInfo)
transDataOrNew (DataType l) = return (DataType l)
transDataOrNew (NewType l) = return (NewType l)

transDecl :: Decl SrcSpanInfo -> DesugarM (Decl SrcSpanInfo)
transDecl (TypeDecl l declHeadL typeL)
  = do declHeadL' <- transDeclHead declHeadL
       typeL' <- transType typeL
       return (TypeDecl l declHeadL' typeL')
transDecl (TypeFamDecl l declHeadL maybeKindL)
  = do declHeadL' <- transDeclHead declHeadL
       maybeKindL' <- mapM transKind maybeKindL
       return (TypeFamDecl l declHeadL' maybeKindL')
transDecl (ClosedTypeFamDecl l declHeadL maybeKindL typeEqnLs)
  = do declHeadL' <- transDeclHead declHeadL
       maybeKindL' <- mapM transKind maybeKindL
       typeEqnLs' <- mapM transTypeEqn typeEqnLs
       return (ClosedTypeFamDecl l declHeadL' maybeKindL' typeEqnLs')
transDecl
  (DataDecl l dataOrNewL maybeContextL declHeadL qualConDeclLs
     maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew dataOrNewL
       maybeContextL' <- mapM transContext maybeContextL
       declHeadL' <- transDeclHead declHeadL
       qualConDeclLs' <- mapM transQualConDecl qualConDeclLs
       maybeDerivingL' <- mapM transDeriving maybeDerivingL
       return
         (DataDecl l dataOrNewL' maybeContextL' declHeadL' qualConDeclLs'
            maybeDerivingL')
-- desugar:
transDecl
  (GDataDecl l dataOrNewL maybeContextL declHeadL maybeKindL
     gadtDeclLs maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew dataOrNewL
       maybeContextL' <- mapM transContext maybeContextL
       declHeadL' <- transDeclHead declHeadL
       maybeKindL' <- mapM transKind maybeKindL
       gadtDeclLs' <- mapM transGadtDecl gadtDeclLs
       maybeDerivingL' <- mapM transDeriving maybeDerivingL
       qualified <- moduleDefined declName
       addGlobal declName qualified
       return
         (GDataDecl l dataOrNewL' maybeContextL' declHeadL' maybeKindL'
            gadtDeclLs'
            maybeDerivingL')
  where declName | DHead l name <- declHeadL = name
transDecl (DataFamDecl l maybeContextL declHeadL maybeKindL)
  = do maybeContextL' <- mapM transContext maybeContextL
       declHeadL' <- transDeclHead declHeadL
       maybeKindL' <- mapM transKind maybeKindL
       return (DataFamDecl l maybeContextL' declHeadL' maybeKindL')
transDecl (TypeInsDecl l typeL typeL1)
  = do typeL' <- transType typeL
       typeL1' <- transType typeL1
       return (TypeInsDecl l typeL' typeL1')
transDecl
  (DataInsDecl l dataOrNewL typeL qualConDeclLs maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew dataOrNewL
       typeL' <- transType typeL
       qualConDeclLs' <- mapM transQualConDecl qualConDeclLs
       maybeDerivingL' <- mapM transDeriving maybeDerivingL
       return
         (DataInsDecl l dataOrNewL' typeL' qualConDeclLs' maybeDerivingL')
transDecl
  (GDataInsDecl l dataOrNewL typeL maybeKindL gadtDeclLs
     maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew dataOrNewL
       typeL' <- transType typeL
       maybeKindL' <- mapM transKind maybeKindL
       gadtDeclLs' <- mapM transGadtDecl gadtDeclLs
       maybeDerivingL' <- mapM transDeriving maybeDerivingL
       return
         (GDataInsDecl l dataOrNewL' typeL' maybeKindL' gadtDeclLs'
            maybeDerivingL')
transDecl
  (ClassDecl l maybeContextL declHeadL funDepLs maybeClassDeclLs)
  = do maybeContextL' <- mapM transContext maybeContextL
       declHeadL' <- transDeclHead declHeadL
       funDepLs' <- mapM transFunDep funDepLs
       maybeClassDeclLs' <- mapM (mapM transClassDecl) maybeClassDeclLs
       return
         (ClassDecl l maybeContextL' declHeadL' funDepLs' maybeClassDeclLs')
transDecl (InstDecl l maybeOverlapL instRuleL maybeInstDeclLs)
  = do maybeOverlapL' <- mapM transOverlap maybeOverlapL
       instRuleL' <- transInstRule instRuleL
       maybeInstDeclLs' <- mapM (mapM transInstDecl) maybeInstDeclLs
       return (InstDecl l maybeOverlapL' instRuleL' maybeInstDeclLs')
transDecl (DerivDecl l maybeOverlapL instRuleL)
  = do maybeOverlapL' <- mapM transOverlap maybeOverlapL
       instRuleL' <- transInstRule instRuleL
       return (DerivDecl l maybeOverlapL' instRuleL')
transDecl (InfixDecl l assocL maybeInt opLs)
  = do assocL' <- transAssoc assocL
       maybeInt' <- mapM return maybeInt
       opLs' <- mapM transOp opLs
       return (InfixDecl l assocL' maybeInt' opLs')
transDecl (DefaultDecl l typeLs)
  = do typeLs' <- mapM transType typeLs
       return (DefaultDecl l typeLs')
transDecl (SpliceDecl l expL)
  = do expL' <- transExp expL
       return (SpliceDecl l expL')
transDecl (TypeSig l nameLs typeL)
  = do nameLs' <- mapM transName nameLs
       typeL' <- transType typeL
       return (TypeSig l nameLs' typeL')
transDecl
  (PatSynSig l nameL maybeTyVarBindLs maybeContextL maybeContextL1
     typeL)
  = do nameL' <- transName nameL
       maybeTyVarBindLs' <- mapM (mapM transTyVarBind) maybeTyVarBindLs
       maybeContextL' <- mapM transContext maybeContextL
       maybeContextL1' <- mapM transContext maybeContextL1
       typeL' <- transType typeL
       return
         (PatSynSig l nameL' maybeTyVarBindLs' maybeContextL'
            maybeContextL1'
            typeL')
-- desugar:
transDecl (FunBind l matchLs)
  = do matchLs' <- mapM transMatch matchLs
       let Match l nameL patLs rhsL maybeBindsL = head matchLs
       qualified <- moduleDefined nameL
       addGlobal nameL qualified
       return (FunBind l matchLs')
-- desugar:
transDecl (PatBind l patL rhsL maybeBindsL)
  = do withPat patL
       return (PatBind l patL rhsL maybeBindsL)
transDecl (PatSyn l patL patL1 patternSynDirectionL)
  = do patL' <- transPat patL
       patL1' <- transPat patL1
       patternSynDirectionL' <- transPatternSynDirection
                                  patternSynDirectionL
       return (PatSyn l patL' patL1' patternSynDirectionL')
transDecl (ForImp l callConvL maybeSafetyL maybeString nameL typeL)
  = do callConvL' <- transCallConv callConvL
       maybeSafetyL' <- mapM transSafety maybeSafetyL
       maybeString' <- mapM return maybeString
       nameL' <- transName nameL
       typeL' <- transType typeL
       return
         (ForImp l callConvL' maybeSafetyL' maybeString' nameL' typeL')
transDecl (ForExp l callConvL maybeString nameL typeL)
  = do callConvL' <- transCallConv callConvL
       maybeString' <- mapM return maybeString
       nameL' <- transName nameL
       typeL' <- transType typeL
       return (ForExp l callConvL' maybeString' nameL' typeL')
transDecl (RulePragmaDecl l ruleLs)
  = do ruleLs' <- mapM transRule ruleLs
       return (RulePragmaDecl l ruleLs')
transDecl (DeprPragmaDecl l tuple2NameLs_Strings)
  = do tuple2NameLs_Strings' <- mapM
                                  (transTupleWith (mapM transName, return))
                                  tuple2NameLs_Strings
       return (DeprPragmaDecl l tuple2NameLs_Strings')
transDecl (WarnPragmaDecl l tuple2NameLs_Strings)
  = do tuple2NameLs_Strings' <- mapM
                                  (transTupleWith (mapM transName, return))
                                  tuple2NameLs_Strings
       return (WarnPragmaDecl l tuple2NameLs_Strings')
transDecl (InlineSig l bool maybeActivationL qNameL)
  = do maybeActivationL' <- mapM transActivation maybeActivationL
       qNameL' <- transQName qNameL
       return (InlineSig l bool maybeActivationL' qNameL')
transDecl (InlineConlikeSig l maybeActivationL qNameL)
  = do maybeActivationL' <- mapM transActivation maybeActivationL
       qNameL' <- transQName qNameL
       return (InlineConlikeSig l maybeActivationL' qNameL')
transDecl (SpecSig l maybeActivationL qNameL typeLs)
  = do maybeActivationL' <- mapM transActivation maybeActivationL
       qNameL' <- transQName qNameL
       typeLs' <- mapM transType typeLs
       return (SpecSig l maybeActivationL' qNameL' typeLs')
transDecl (SpecInlineSig l bool maybeActivationL qNameL typeLs)
  = do maybeActivationL' <- mapM transActivation maybeActivationL
       qNameL' <- transQName qNameL
       typeLs' <- mapM transType typeLs
       return (SpecInlineSig l bool maybeActivationL' qNameL' typeLs')
transDecl (InstSig l instRuleL)
  = do instRuleL' <- transInstRule instRuleL
       return (InstSig l instRuleL')
transDecl (AnnPragma l annotationL)
  = do annotationL' <- transAnnotation annotationL
       return (AnnPragma l annotationL')
transDecl (MinimalPragma l maybeBooleanFormulaL)
  = do maybeBooleanFormulaL' <- mapM transBooleanFormula
                                  maybeBooleanFormulaL
       return (MinimalPragma l maybeBooleanFormulaL')
transDecl (RoleAnnotDecl l qNameL roleLs)
  = do qNameL' <- transQName qNameL
       roleLs' <- mapM transRole roleLs
       return (RoleAnnotDecl l qNameL' roleLs')

transDeclHead ::
              DeclHead SrcSpanInfo -> DesugarM (DeclHead SrcSpanInfo)
transDeclHead (DHead l nameL)
  = do nameL' <- transName nameL
       return (DHead l nameL')
transDeclHead (DHInfix l tyVarBindL nameL)
  = do tyVarBindL' <- transTyVarBind tyVarBindL
       nameL' <- transName nameL
       return (DHInfix l tyVarBindL' nameL')
transDeclHead (DHParen l declHeadL)
  = do declHeadL' <- transDeclHead declHeadL
       return (DHParen l declHeadL')
transDeclHead (DHApp l declHeadL tyVarBindL)
  = do declHeadL' <- transDeclHead declHeadL
       tyVarBindL' <- transTyVarBind tyVarBindL
       return (DHApp l declHeadL' tyVarBindL')

transDeriving ::
              Deriving SrcSpanInfo -> DesugarM (Deriving SrcSpanInfo)
transDeriving (Deriving l instRuleLs)
  = do instRuleLs' <- mapM transInstRule instRuleLs
       return (Deriving l instRuleLs')

transExp :: Exp SrcSpanInfo -> DesugarM (Exp SrcSpanInfo)
transExp (Var l qNameL)
  = do qNameL' <- transQName qNameL
       return (Var l qNameL')
transExp (IPVar l iPNameL)
  = do iPNameL' <- transIPName iPNameL
       return (IPVar l iPNameL')
transExp (Con l qNameL)
  = do qNameL' <- transQName qNameL
       return (Con l qNameL')
transExp (Lit l literalL)
  = do literalL' <- transLiteral literalL
       return (Lit l literalL')
transExp (InfixApp l expL qOpL expL1)
  = do expL' <- transExp expL
       qOpL' <- transQOp qOpL
       expL1' <- transExp expL1
       return (InfixApp l expL' qOpL' expL1')
transExp (App l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (App l expL' expL1')
transExp (NegApp l expL)
  = do expL' <- transExp expL
       return (NegApp l expL')
transExp (Lambda l patLs expL)
  = do patLs' <- mapM transPat patLs
       expL' <- transExp expL
       return (Lambda l patLs' expL')
transExp (Let l bindsL expL)
  = do bindsL' <- transBinds bindsL
       expL' <- transExp expL
       return (Let l bindsL' expL')
transExp (If l expL expL1 expL2)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       expL2' <- transExp expL2
       return (If l expL' expL1' expL2')
transExp (MultiIf l guardedRhsLs)
  = do guardedRhsLs' <- mapM transGuardedRhs guardedRhsLs
       return (MultiIf l guardedRhsLs')
transExp (Case l expL altLs)
  = do expL' <- transExp expL
       altLs' <- mapM transAlt altLs
       return (Case l expL' altLs')
transExp (Do l stmtLs)
  = do stmtLs' <- mapM transStmt stmtLs
       return (Do l stmtLs')
transExp (MDo l stmtLs)
  = do stmtLs' <- mapM transStmt stmtLs
       return (MDo l stmtLs')
transExp (Tuple l boxed expLs)
  = do boxed' <- transBoxed boxed
       expLs' <- mapM transExp expLs
       return (Tuple l boxed' expLs')
transExp (TupleSection l boxed maybeExpLs)
  = do boxed' <- transBoxed boxed
       maybeExpLs' <- mapM (mapM transExp) maybeExpLs
       return (TupleSection l boxed' maybeExpLs')
transExp (List l expLs)
  = do expLs' <- mapM transExp expLs
       return (List l expLs')
transExp (ParArray l expLs)
  = do expLs' <- mapM transExp expLs
       return (ParArray l expLs')
transExp (Paren l expL)
  = do expL' <- transExp expL
       return (Paren l expL')
transExp (LeftSection l expL qOpL)
  = do expL' <- transExp expL
       qOpL' <- transQOp qOpL
       return (LeftSection l expL' qOpL')
transExp (RightSection l qOpL expL)
  = do qOpL' <- transQOp qOpL
       expL' <- transExp expL
       return (RightSection l qOpL' expL')
transExp (RecConstr l qNameL fieldUpdateLs)
  = do qNameL' <- transQName qNameL
       fieldUpdateLs' <- mapM transFieldUpdate fieldUpdateLs
       return (RecConstr l qNameL' fieldUpdateLs')
transExp (RecUpdate l expL fieldUpdateLs)
  = do expL' <- transExp expL
       fieldUpdateLs' <- mapM transFieldUpdate fieldUpdateLs
       return (RecUpdate l expL' fieldUpdateLs')
transExp (EnumFrom l expL)
  = do expL' <- transExp expL
       return (EnumFrom l expL')
transExp (EnumFromTo l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (EnumFromTo l expL' expL1')
transExp (EnumFromThen l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (EnumFromThen l expL' expL1')
transExp (EnumFromThenTo l expL expL1 expL2)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       expL2' <- transExp expL2
       return (EnumFromThenTo l expL' expL1' expL2')
transExp (ParArrayFromTo l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (ParArrayFromTo l expL' expL1')
transExp (ParArrayFromThenTo l expL expL1 expL2)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       expL2' <- transExp expL2
       return (ParArrayFromThenTo l expL' expL1' expL2')
transExp (ListComp l expL qualStmtLs)
  = do expL' <- transExp expL
       qualStmtLs' <- mapM transQualStmt qualStmtLs
       return (ListComp l expL' qualStmtLs')
transExp (ParComp l expL qualStmtLss)
  = do expL' <- transExp expL
       qualStmtLss' <- mapM (mapM transQualStmt) qualStmtLss
       return (ParComp l expL' qualStmtLss')
transExp (ParArrayComp l expL qualStmtLss)
  = do expL' <- transExp expL
       qualStmtLss' <- mapM (mapM transQualStmt) qualStmtLss
       return (ParArrayComp l expL' qualStmtLss')
transExp (ExpTypeSig l expL typeL)
  = do expL' <- transExp expL
       typeL' <- transType typeL
       return (ExpTypeSig l expL' typeL')
transExp (VarQuote l qNameL)
  = do qNameL' <- transQName qNameL
       return (VarQuote l qNameL')
transExp (TypQuote l qNameL)
  = do qNameL' <- transQName qNameL
       return (TypQuote l qNameL')
transExp (BracketExp l bracketL)
  = do bracketL' <- transBracket bracketL
       return (BracketExp l bracketL')
transExp (SpliceExp l spliceL)
  = do spliceL' <- transSplice spliceL
       return (SpliceExp l spliceL')
transExp (QuasiQuote l string string1)
  = return (QuasiQuote l string string1)
transExp (XTag l xNameL xAttrLs maybeExpL expLs)
  = do xNameL' <- transXName xNameL
       xAttrLs' <- mapM transXAttr xAttrLs
       maybeExpL' <- mapM transExp maybeExpL
       expLs' <- mapM transExp expLs
       return (XTag l xNameL' xAttrLs' maybeExpL' expLs')
transExp (XETag l xNameL xAttrLs maybeExpL)
  = do xNameL' <- transXName xNameL
       xAttrLs' <- mapM transXAttr xAttrLs
       maybeExpL' <- mapM transExp maybeExpL
       return (XETag l xNameL' xAttrLs' maybeExpL')
transExp (XPcdata l string) = return (XPcdata l string)
transExp (XExpTag l expL)
  = do expL' <- transExp expL
       return (XExpTag l expL')
transExp (XChildTag l expLs)
  = do expLs' <- mapM transExp expLs
       return (XChildTag l expLs')
transExp (CorePragma l string expL)
  = do expL' <- transExp expL
       return (CorePragma l string expL')
transExp (SCCPragma l string expL)
  = do expL' <- transExp expL
       return (SCCPragma l string expL')
transExp (GenPragma l string tuple2Int_Int tuple2Int_Int1 expL)
  = do tuple2Int_Int' <- transTupleWith (return, return)
                           tuple2Int_Int
       tuple2Int_Int1' <- transTupleWith (return, return) tuple2Int_Int1
       expL' <- transExp expL
       return (GenPragma l string tuple2Int_Int' tuple2Int_Int1' expL')
transExp (Proc l patL expL)
  = do patL' <- transPat patL
       expL' <- transExp expL
       return (Proc l patL' expL')
transExp (LeftArrApp l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (LeftArrApp l expL' expL1')
transExp (RightArrApp l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (RightArrApp l expL' expL1')
transExp (LeftArrHighApp l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (LeftArrHighApp l expL' expL1')
transExp (RightArrHighApp l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (RightArrHighApp l expL' expL1')
transExp (LCase l altLs)
  = do altLs' <- mapM transAlt altLs
       return (LCase l altLs')
transExp (ExprHole l) = return (ExprHole l)

transExportSpec ::
                ExportSpec SrcSpanInfo -> DesugarM (ExportSpec SrcSpanInfo)
transExportSpec (EVar l qNameL)
  = do qNameL' <- transQName qNameL
       return (EVar l qNameL')
transExportSpec (EAbs l namespaceL qNameL)
  = do namespaceL' <- transNamespace namespaceL
       qNameL' <- transQName qNameL
       return (EAbs l namespaceL' qNameL')
transExportSpec (EThingAll l qNameL)
  = do qNameL' <- transQName qNameL
       return (EThingAll l qNameL')
transExportSpec (EThingWith l qNameL cNameLs)
  = do qNameL' <- transQName qNameL
       cNameLs' <- mapM transCName cNameLs
       return (EThingWith l qNameL' cNameLs')
transExportSpec (EModuleContents l moduleNameL)
  = do moduleNameL' <- transModuleName moduleNameL
       return (EModuleContents l moduleNameL')

transExportSpecList ::
                    ExportSpecList SrcSpanInfo -> DesugarM (ExportSpecList SrcSpanInfo)
transExportSpecList (ExportSpecList l exportSpecLs)
  = do exportSpecLs' <- mapM transExportSpec exportSpecLs
       return (ExportSpecList l exportSpecLs')

transFieldDecl ::
               FieldDecl SrcSpanInfo -> DesugarM (FieldDecl SrcSpanInfo)
transFieldDecl (FieldDecl l nameLs typeL)
  = do nameLs' <- mapM transName nameLs
       typeL' <- transType typeL
       return (FieldDecl l nameLs' typeL')

transFieldUpdate ::
                 FieldUpdate SrcSpanInfo -> DesugarM (FieldUpdate SrcSpanInfo)
transFieldUpdate (FieldUpdate l qNameL expL)
  = do qNameL' <- transQName qNameL
       expL' <- transExp expL
       return (FieldUpdate l qNameL' expL')
transFieldUpdate (FieldPun l qNameL)
  = do qNameL' <- transQName qNameL
       return (FieldPun l qNameL')
transFieldUpdate (FieldWildcard l) = return (FieldWildcard l)

transFunDep :: FunDep SrcSpanInfo -> DesugarM (FunDep SrcSpanInfo)
transFunDep (FunDep l nameLs nameLs1)
  = do nameLs' <- mapM transName nameLs
       nameLs1' <- mapM transName nameLs1
       return (FunDep l nameLs' nameLs1')

-- desugar:
transGadtDecl ::
              GadtDecl SrcSpanInfo -> DesugarM (GadtDecl SrcSpanInfo)
transGadtDecl (GadtDecl l nameL maybeFieldDeclLs typeL)
  = do nameL' <- transName nameL
       maybeFieldDeclLs' <- mapM (mapM transFieldDecl) maybeFieldDeclLs
       typeL' <- transType typeL
       qualified <- moduleDefined nameL
       addGlobal nameL qualified
       return (GadtDecl l nameL' maybeFieldDeclLs' typeL')

transGuardedRhs ::
                GuardedRhs SrcSpanInfo -> DesugarM (GuardedRhs SrcSpanInfo)
transGuardedRhs (GuardedRhs l stmtLs expL)
  = do stmtLs' <- mapM transStmt stmtLs
       expL' <- transExp expL
       return (GuardedRhs l stmtLs' expL')

transIPBind :: IPBind SrcSpanInfo -> DesugarM (IPBind SrcSpanInfo)
transIPBind (IPBind l iPNameL expL)
  = do iPNameL' <- transIPName iPNameL
       expL' <- transExp expL
       return (IPBind l iPNameL' expL')

transIPName :: IPName SrcSpanInfo -> DesugarM (IPName SrcSpanInfo)
transIPName (IPDup l string) = return (IPDup l string)
transIPName (IPLin l string) = return (IPLin l string)

transImportDecl ::
                ImportDecl SrcSpanInfo -> DesugarM (ImportDecl SrcSpanInfo)
transImportDecl
  (ImportDecl l moduleNameL bool bool1 bool2 maybeString
     maybeModuleNameL maybeImportSpecListL)
  = do moduleNameL' <- transModuleName moduleNameL
       maybeString' <- mapM return maybeString
       maybeModuleNameL' <- mapM transModuleName maybeModuleNameL
       maybeImportSpecListL' <- mapM transImportSpecList
                                  maybeImportSpecListL
       return
         (ImportDecl l moduleNameL' bool bool1 bool2 maybeString'
            maybeModuleNameL'
            maybeImportSpecListL')

transImportSpec ::
                ImportSpec SrcSpanInfo -> DesugarM (ImportSpec SrcSpanInfo)
transImportSpec (IVar l nameL)
  = do nameL' <- transName nameL
       return (IVar l nameL')
transImportSpec (IAbs l namespaceL nameL)
  = do namespaceL' <- transNamespace namespaceL
       nameL' <- transName nameL
       return (IAbs l namespaceL' nameL')
transImportSpec (IThingAll l nameL)
  = do nameL' <- transName nameL
       return (IThingAll l nameL')
transImportSpec (IThingWith l nameL cNameLs)
  = do nameL' <- transName nameL
       cNameLs' <- mapM transCName cNameLs
       return (IThingWith l nameL' cNameLs')

transImportSpecList ::
                    ImportSpecList SrcSpanInfo -> DesugarM (ImportSpecList SrcSpanInfo)
transImportSpecList (ImportSpecList l bool importSpecLs)
  = do importSpecLs' <- mapM transImportSpec importSpecLs
       return (ImportSpecList l bool importSpecLs')

transInstDecl ::
              InstDecl SrcSpanInfo -> DesugarM (InstDecl SrcSpanInfo)
transInstDecl (InsDecl l declL)
  = do declL' <- transDecl declL
       return (InsDecl l declL')
transInstDecl (InsType l typeL typeL1)
  = do typeL' <- transType typeL
       typeL1' <- transType typeL1
       return (InsType l typeL' typeL1')
transInstDecl
  (InsData l dataOrNewL typeL qualConDeclLs maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew dataOrNewL
       typeL' <- transType typeL
       qualConDeclLs' <- mapM transQualConDecl qualConDeclLs
       maybeDerivingL' <- mapM transDeriving maybeDerivingL
       return
         (InsData l dataOrNewL' typeL' qualConDeclLs' maybeDerivingL')
transInstDecl
  (InsGData l dataOrNewL typeL maybeKindL gadtDeclLs maybeDerivingL)
  = do dataOrNewL' <- transDataOrNew dataOrNewL
       typeL' <- transType typeL
       maybeKindL' <- mapM transKind maybeKindL
       gadtDeclLs' <- mapM transGadtDecl gadtDeclLs
       maybeDerivingL' <- mapM transDeriving maybeDerivingL
       return
         (InsGData l dataOrNewL' typeL' maybeKindL' gadtDeclLs'
            maybeDerivingL')

transInstHead ::
              InstHead SrcSpanInfo -> DesugarM (InstHead SrcSpanInfo)
transInstHead (IHCon l qNameL)
  = do qNameL' <- transQName qNameL
       return (IHCon l qNameL')
transInstHead (IHInfix l typeL qNameL)
  = do typeL' <- transType typeL
       qNameL' <- transQName qNameL
       return (IHInfix l typeL' qNameL')
transInstHead (IHParen l instHeadL)
  = do instHeadL' <- transInstHead instHeadL
       return (IHParen l instHeadL')
transInstHead (IHApp l instHeadL typeL)
  = do instHeadL' <- transInstHead instHeadL
       typeL' <- transType typeL
       return (IHApp l instHeadL' typeL')

transInstRule ::
              InstRule SrcSpanInfo -> DesugarM (InstRule SrcSpanInfo)
transInstRule (IRule l maybeTyVarBindLs maybeContextL instHeadL)
  = do maybeTyVarBindLs' <- mapM (mapM transTyVarBind)
                              maybeTyVarBindLs
       maybeContextL' <- mapM transContext maybeContextL
       instHeadL' <- transInstHead instHeadL
       return (IRule l maybeTyVarBindLs' maybeContextL' instHeadL')
transInstRule (IParen l instRuleL)
  = do instRuleL' <- transInstRule instRuleL
       return (IParen l instRuleL')

transKind :: Kind SrcSpanInfo -> DesugarM (Kind SrcSpanInfo)
transKind (KindStar l) = return (KindStar l)
transKind (KindFn l kindL kindL1)
  = do kindL' <- transKind kindL
       kindL1' <- transKind kindL1
       return (KindFn l kindL' kindL1')
transKind (KindParen l kindL)
  = do kindL' <- transKind kindL
       return (KindParen l kindL')
transKind (KindVar l qNameL)
  = do qNameL' <- transQName qNameL
       return (KindVar l qNameL')
transKind (KindApp l kindL kindL1)
  = do kindL' <- transKind kindL
       kindL1' <- transKind kindL1
       return (KindApp l kindL' kindL1')
transKind (KindTuple l kindLs)
  = do kindLs' <- mapM transKind kindLs
       return (KindTuple l kindLs')
transKind (KindList l kindL)
  = do kindL' <- transKind kindL
       return (KindList l kindL')

transLiteral ::
             Literal SrcSpanInfo -> DesugarM (Literal SrcSpanInfo)
transLiteral (Char l char string) = return (Char l char string)
transLiteral (String l string string1)
  = return (String l string string1)
transLiteral (Int l integer string) = return (Int l integer string)
transLiteral (Frac l rational string)
  = return (Frac l rational string)
transLiteral (PrimInt l integer string)
  = return (PrimInt l integer string)
transLiteral (PrimWord l integer string)
  = return (PrimWord l integer string)
transLiteral (PrimFloat l rational string)
  = return (PrimFloat l rational string)
transLiteral (PrimDouble l rational string)
  = return (PrimDouble l rational string)
transLiteral (PrimChar l char string)
  = return (PrimChar l char string)
transLiteral (PrimString l string string1)
  = return (PrimString l string string1)

transMatch :: Match SrcSpanInfo -> DesugarM (Match SrcSpanInfo)
transMatch (Match l nameL patLs rhsL maybeBindsL)
  = do nameL' <- transName nameL
       patLs' <- mapM transPat patLs
       rhsL' <- transRhs rhsL
       maybeBindsL' <- mapM transBinds maybeBindsL
       return (Match l nameL' patLs' rhsL' maybeBindsL')
transMatch (InfixMatch l patL nameL patLs rhsL maybeBindsL)
  = do patL' <- transPat patL
       nameL' <- transName nameL
       patLs' <- mapM transPat patLs
       rhsL' <- transRhs rhsL
       maybeBindsL' <- mapM transBinds maybeBindsL
       return (InfixMatch l patL' nameL' patLs' rhsL' maybeBindsL')

-- desugar: 記下 module name
transModule :: Module SrcSpanInfo -> DesugarM (Module SrcSpanInfo)
transModule
  (Module l maybeModuleHeadL modulePragmaLs importDeclLs declLs)
  = do setModuleName moduleName
       maybeModuleHeadL' <- mapM transModuleHead maybeModuleHeadL
       modulePragmaLs' <- mapM transModulePragma modulePragmaLs
       importDeclLs' <- mapM transImportDecl importDeclLs
       declLs' <- mapM transDecl declLs
       return
         (Module l maybeModuleHeadL' modulePragmaLs' importDeclLs' declLs')
   where moduleName = case maybeModuleHeadL of
                          Just (ModuleHead l (ModuleName l' name) _ _) -> name
                          Nothing -> "Main"
transModule
  (XmlPage l moduleNameL modulePragmaLs xNameL xAttrLs maybeExpL
     expLs)
  = do moduleNameL' <- transModuleName moduleNameL
       modulePragmaLs' <- mapM transModulePragma modulePragmaLs
       xNameL' <- transXName xNameL
       xAttrLs' <- mapM transXAttr xAttrLs
       maybeExpL' <- mapM transExp maybeExpL
       expLs' <- mapM transExp expLs
       return
         (XmlPage l moduleNameL' modulePragmaLs' xNameL' xAttrLs' maybeExpL'
            expLs')
transModule
  (XmlHybrid l maybeModuleHeadL modulePragmaLs importDeclLs declLs
     xNameL xAttrLs maybeExpL expLs)
  = do maybeModuleHeadL' <- mapM transModuleHead maybeModuleHeadL
       modulePragmaLs' <- mapM transModulePragma modulePragmaLs
       importDeclLs' <- mapM transImportDecl importDeclLs
       declLs' <- mapM transDecl declLs
       xNameL' <- transXName xNameL
       xAttrLs' <- mapM transXAttr xAttrLs
       maybeExpL' <- mapM transExp maybeExpL
       expLs' <- mapM transExp expLs
       return
         (XmlHybrid l maybeModuleHeadL' modulePragmaLs' importDeclLs'
            declLs'
            xNameL'
            xAttrLs'
            maybeExpL'
            expLs')

transModuleHead ::
                ModuleHead SrcSpanInfo -> DesugarM (ModuleHead SrcSpanInfo)
transModuleHead
  (ModuleHead l moduleNameL maybeWarningTextL maybeExportSpecListL)
  = do moduleNameL' <- transModuleName moduleNameL
       maybeWarningTextL' <- mapM transWarningText maybeWarningTextL
       maybeExportSpecListL' <- mapM transExportSpecList
                                  maybeExportSpecListL
       return
         (ModuleHead l moduleNameL' maybeWarningTextL'
            maybeExportSpecListL')

transModuleName ::
                ModuleName SrcSpanInfo -> DesugarM (ModuleName SrcSpanInfo)
transModuleName (ModuleName l string)
  = return (ModuleName l string)

transModulePragma ::
                  ModulePragma SrcSpanInfo -> DesugarM (ModulePragma SrcSpanInfo)
transModulePragma (LanguagePragma l nameLs)
  = do nameLs' <- mapM transName nameLs
       return (LanguagePragma l nameLs')
transModulePragma (OptionsPragma l maybeTool string)
  = do maybeTool' <- mapM transTool maybeTool
       return (OptionsPragma l maybeTool' string)
transModulePragma (AnnModulePragma l annotationL)
  = do annotationL' <- transAnnotation annotationL
       return (AnnModulePragma l annotationL')

transName :: Name SrcSpanInfo -> DesugarM (Name SrcSpanInfo)
transName (Ident l string) = return (Ident l string)
transName (Symbol l string) = return (Symbol l string)

transNamespace ::
               Namespace SrcSpanInfo -> DesugarM (Namespace SrcSpanInfo)
transNamespace (NoNamespace l) = return (NoNamespace l)
transNamespace (TypeNamespace l) = return (TypeNamespace l)
transNamespace (PatternNamespace l) = return (PatternNamespace l)

transOp :: Op SrcSpanInfo -> DesugarM (Op SrcSpanInfo)
transOp (VarOp l nameL)
  = do nameL' <- transName nameL
       return (VarOp l nameL')
transOp (ConOp l nameL)
  = do nameL' <- transName nameL
       return (ConOp l nameL')

transOverlap ::
             Overlap SrcSpanInfo -> DesugarM (Overlap SrcSpanInfo)
transOverlap (NoOverlap l) = return (NoOverlap l)
transOverlap (Overlap l) = return (Overlap l)
transOverlap (Incoherent l) = return (Incoherent l)

transPXAttr :: PXAttr SrcSpanInfo -> DesugarM (PXAttr SrcSpanInfo)
transPXAttr (PXAttr l xNameL patL)
  = do xNameL' <- transXName xNameL
       patL' <- transPat patL
       return (PXAttr l xNameL' patL')

transPat :: Pat SrcSpanInfo -> DesugarM (Pat SrcSpanInfo)
transPat (PVar l nameL)
  = do nameL' <- transName nameL
       return (PVar l nameL')
transPat (PLit l signL literalL)
  = do signL' <- transSign signL
       literalL' <- transLiteral literalL
       return (PLit l signL' literalL')
transPat (PNPlusK l nameL integer)
  = do nameL' <- transName nameL
       return (PNPlusK l nameL' integer)
transPat (PInfixApp l patL qNameL patL1)
  = do patL' <- transPat patL
       qNameL' <- transQName qNameL
       patL1' <- transPat patL1
       return (PInfixApp l patL' qNameL' patL1')
transPat (PApp l qNameL patLs)
  = do qNameL' <- transQName qNameL
       patLs' <- mapM transPat patLs
       return (PApp l qNameL' patLs')
transPat (PTuple l boxed patLs)
  = do boxed' <- transBoxed boxed
       patLs' <- mapM transPat patLs
       return (PTuple l boxed' patLs')
transPat (PList l patLs)
  = do patLs' <- mapM transPat patLs
       return (PList l patLs')
transPat (PParen l patL)
  = do patL' <- transPat patL
       return (PParen l patL')
transPat (PRec l qNameL patFieldLs)
  = do qNameL' <- transQName qNameL
       patFieldLs' <- mapM transPatField patFieldLs
       return (PRec l qNameL' patFieldLs')
transPat (PAsPat l nameL patL)
  = do nameL' <- transName nameL
       patL' <- transPat patL
       return (PAsPat l nameL' patL')
transPat (PWildCard l) = return (PWildCard l)
transPat (PIrrPat l patL)
  = do patL' <- transPat patL
       return (PIrrPat l patL')
transPat (PatTypeSig l patL typeL)
  = do patL' <- transPat patL
       typeL' <- transType typeL
       return (PatTypeSig l patL' typeL')
transPat (PViewPat l expL patL)
  = do expL' <- transExp expL
       patL' <- transPat patL
       return (PViewPat l expL' patL')
transPat (PRPat l rPatLs)
  = do rPatLs' <- mapM transRPat rPatLs
       return (PRPat l rPatLs')
transPat (PXTag l xNameL pXAttrLs maybePatL patLs)
  = do xNameL' <- transXName xNameL
       pXAttrLs' <- mapM transPXAttr pXAttrLs
       maybePatL' <- mapM transPat maybePatL
       patLs' <- mapM transPat patLs
       return (PXTag l xNameL' pXAttrLs' maybePatL' patLs')
transPat (PXETag l xNameL pXAttrLs maybePatL)
  = do xNameL' <- transXName xNameL
       pXAttrLs' <- mapM transPXAttr pXAttrLs
       maybePatL' <- mapM transPat maybePatL
       return (PXETag l xNameL' pXAttrLs' maybePatL')
transPat (PXPcdata l string) = return (PXPcdata l string)
transPat (PXPatTag l patL)
  = do patL' <- transPat patL
       return (PXPatTag l patL')
transPat (PXRPats l rPatLs)
  = do rPatLs' <- mapM transRPat rPatLs
       return (PXRPats l rPatLs')
transPat (PQuasiQuote l string string1)
  = return (PQuasiQuote l string string1)
transPat (PBangPat l patL)
  = do patL' <- transPat patL
       return (PBangPat l patL')

transPatField ::
              PatField SrcSpanInfo -> DesugarM (PatField SrcSpanInfo)
transPatField (PFieldPat l qNameL patL)
  = do qNameL' <- transQName qNameL
       patL' <- transPat patL
       return (PFieldPat l qNameL' patL')
transPatField (PFieldPun l qNameL)
  = do qNameL' <- transQName qNameL
       return (PFieldPun l qNameL')
transPatField (PFieldWildcard l) = return (PFieldWildcard l)

transPatternSynDirection ::
                         PatternSynDirection SrcSpanInfo ->
                           DesugarM (PatternSynDirection SrcSpanInfo)
transPatternSynDirection Unidirectional = return Unidirectional
transPatternSynDirection ImplicitBidirectional
  = return ImplicitBidirectional
transPatternSynDirection (ExplicitBidirectional l declLs)
  = do declLs' <- mapM transDecl declLs
       return (ExplicitBidirectional l declLs')

transPromoted ::
              Promoted SrcSpanInfo -> DesugarM (Promoted SrcSpanInfo)
transPromoted (PromotedInteger l integer string)
  = return (PromotedInteger l integer string)
transPromoted (PromotedString l string string1)
  = return (PromotedString l string string1)
transPromoted (PromotedCon l bool qNameL)
  = do qNameL' <- transQName qNameL
       return (PromotedCon l bool qNameL')
transPromoted (PromotedList l bool typeLs)
  = do typeLs' <- mapM transType typeLs
       return (PromotedList l bool typeLs')
transPromoted (PromotedTuple l typeLs)
  = do typeLs' <- mapM transType typeLs
       return (PromotedTuple l typeLs')
transPromoted (PromotedUnit l) = return (PromotedUnit l)

transQName :: QName SrcSpanInfo -> DesugarM (QName SrcSpanInfo)
transQName (Qual l moduleNameL nameL)
  = do moduleNameL' <- transModuleName moduleNameL
       nameL' <- transName nameL
       return (Qual l moduleNameL' nameL')
transQName (UnQual l nameL)
  = do nameL' <- transName nameL
       return (UnQual l nameL')
transQName (Special l specialConL)
  = do specialConL' <- transSpecialCon specialConL
       return (Special l specialConL')

transQOp :: QOp SrcSpanInfo -> DesugarM (QOp SrcSpanInfo)
transQOp (QVarOp l qNameL)
  = do qNameL' <- transQName qNameL
       return (QVarOp l qNameL')
transQOp (QConOp l qNameL)
  = do qNameL' <- transQName qNameL
       return (QConOp l qNameL')

transQualConDecl ::
                 QualConDecl SrcSpanInfo -> DesugarM (QualConDecl SrcSpanInfo)
transQualConDecl
  (QualConDecl l maybeTyVarBindLs maybeContextL conDeclL)
  = do maybeTyVarBindLs' <- mapM (mapM transTyVarBind)
                              maybeTyVarBindLs
       maybeContextL' <- mapM transContext maybeContextL
       conDeclL' <- transConDecl conDeclL
       return (QualConDecl l maybeTyVarBindLs' maybeContextL' conDeclL')

transQualStmt ::
              QualStmt SrcSpanInfo -> DesugarM (QualStmt SrcSpanInfo)
transQualStmt (QualStmt l stmtL)
  = do stmtL' <- transStmt stmtL
       return (QualStmt l stmtL')
transQualStmt (ThenTrans l expL)
  = do expL' <- transExp expL
       return (ThenTrans l expL')
transQualStmt (ThenBy l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (ThenBy l expL' expL1')
transQualStmt (GroupBy l expL)
  = do expL' <- transExp expL
       return (GroupBy l expL')
transQualStmt (GroupUsing l expL)
  = do expL' <- transExp expL
       return (GroupUsing l expL')
transQualStmt (GroupByUsing l expL expL1)
  = do expL' <- transExp expL
       expL1' <- transExp expL1
       return (GroupByUsing l expL' expL1')

transRPat :: RPat SrcSpanInfo -> DesugarM (RPat SrcSpanInfo)
transRPat (RPOp l rPatL rPatOpL)
  = do rPatL' <- transRPat rPatL
       rPatOpL' <- transRPatOp rPatOpL
       return (RPOp l rPatL' rPatOpL')
transRPat (RPEither l rPatL rPatL1)
  = do rPatL' <- transRPat rPatL
       rPatL1' <- transRPat rPatL1
       return (RPEither l rPatL' rPatL1')
transRPat (RPSeq l rPatLs)
  = do rPatLs' <- mapM transRPat rPatLs
       return (RPSeq l rPatLs')
transRPat (RPGuard l patL stmtLs)
  = do patL' <- transPat patL
       stmtLs' <- mapM transStmt stmtLs
       return (RPGuard l patL' stmtLs')
transRPat (RPCAs l nameL rPatL)
  = do nameL' <- transName nameL
       rPatL' <- transRPat rPatL
       return (RPCAs l nameL' rPatL')
transRPat (RPAs l nameL rPatL)
  = do nameL' <- transName nameL
       rPatL' <- transRPat rPatL
       return (RPAs l nameL' rPatL')
transRPat (RPParen l rPatL)
  = do rPatL' <- transRPat rPatL
       return (RPParen l rPatL')
transRPat (RPPat l patL)
  = do patL' <- transPat patL
       return (RPPat l patL')

transRPatOp :: RPatOp SrcSpanInfo -> DesugarM (RPatOp SrcSpanInfo)
transRPatOp (RPStar l) = return (RPStar l)
transRPatOp (RPStarG l) = return (RPStarG l)
transRPatOp (RPPlus l) = return (RPPlus l)
transRPatOp (RPPlusG l) = return (RPPlusG l)
transRPatOp (RPOpt l) = return (RPOpt l)
transRPatOp (RPOptG l) = return (RPOptG l)

transRhs :: Rhs SrcSpanInfo -> DesugarM (Rhs SrcSpanInfo)
transRhs (UnGuardedRhs l expL)
  = do expL' <- transExp expL
       return (UnGuardedRhs l expL')
transRhs (GuardedRhss l guardedRhsLs)
  = do guardedRhsLs' <- mapM transGuardedRhs guardedRhsLs
       return (GuardedRhss l guardedRhsLs')

transRole :: Role SrcSpanInfo -> DesugarM (Role SrcSpanInfo)
transRole (Nominal l) = return (Nominal l)
transRole (Representational l) = return (Representational l)
transRole (Phantom l) = return (Phantom l)
transRole (RoleWildcard l) = return (RoleWildcard l)

transRule :: Rule SrcSpanInfo -> DesugarM (Rule SrcSpanInfo)
transRule
  (Rule l string maybeActivationL maybeRuleVarLs expL expL1)
  = do maybeActivationL' <- mapM transActivation maybeActivationL
       maybeRuleVarLs' <- mapM (mapM transRuleVar) maybeRuleVarLs
       expL' <- transExp expL
       expL1' <- transExp expL1
       return
         (Rule l string maybeActivationL' maybeRuleVarLs' expL' expL1')

transRuleVar ::
             RuleVar SrcSpanInfo -> DesugarM (RuleVar SrcSpanInfo)
transRuleVar (RuleVar l nameL)
  = do nameL' <- transName nameL
       return (RuleVar l nameL')
transRuleVar (TypedRuleVar l nameL typeL)
  = do nameL' <- transName nameL
       typeL' <- transType typeL
       return (TypedRuleVar l nameL' typeL')

transSafety :: Safety SrcSpanInfo -> DesugarM (Safety SrcSpanInfo)
transSafety (PlayRisky l) = return (PlayRisky l)
transSafety (PlaySafe l bool) = return (PlaySafe l bool)
transSafety (PlayInterruptible l) = return (PlayInterruptible l)

transSign :: Sign SrcSpanInfo -> DesugarM (Sign SrcSpanInfo)
transSign (Signless l) = return (Signless l)
transSign (Negative l) = return (Negative l)

transSpecialCon ::
                SpecialCon SrcSpanInfo -> DesugarM (SpecialCon SrcSpanInfo)
transSpecialCon (UnitCon l) = return (UnitCon l)
transSpecialCon (ListCon l) = return (ListCon l)
transSpecialCon (FunCon l) = return (FunCon l)
transSpecialCon (TupleCon l boxed int)
  = do boxed' <- transBoxed boxed
       return (TupleCon l boxed' int)
transSpecialCon (Cons l) = return (Cons l)
transSpecialCon (UnboxedSingleCon l) = return (UnboxedSingleCon l)

transSplice :: Splice SrcSpanInfo -> DesugarM (Splice SrcSpanInfo)
transSplice (IdSplice l string) = return (IdSplice l string)
transSplice (ParenSplice l expL)
  = do expL' <- transExp expL
       return (ParenSplice l expL')

transStmt :: Stmt SrcSpanInfo -> DesugarM (Stmt SrcSpanInfo)
transStmt (Generator l patL expL)
  = do patL' <- transPat patL
       expL' <- transExp expL
       return (Generator l patL' expL')
transStmt (Qualifier l expL)
  = do expL' <- transExp expL
       return (Qualifier l expL')
transStmt (LetStmt l bindsL)
  = do bindsL' <- transBinds bindsL
       return (LetStmt l bindsL')
transStmt (RecStmt l stmtLs)
  = do stmtLs' <- mapM transStmt stmtLs
       return (RecStmt l stmtLs')

transTool :: Tool -> DesugarM Tool
transTool GHC = return GHC
transTool HUGS = return HUGS
transTool NHC98 = return NHC98
transTool YHC = return YHC
transTool HADDOCK = return HADDOCK
transTool (UnknownTool string) = return (UnknownTool string)

transTyVarBind ::
               TyVarBind SrcSpanInfo -> DesugarM (TyVarBind SrcSpanInfo)
transTyVarBind (KindedVar l nameL kindL)
  = do nameL' <- transName nameL
       kindL' <- transKind kindL
       return (KindedVar l nameL' kindL')
transTyVarBind (UnkindedVar l nameL)
  = do nameL' <- transName nameL
       return (UnkindedVar l nameL')

transType :: Type SrcSpanInfo -> DesugarM (Type SrcSpanInfo)
transType (TyForall l maybeTyVarBindLs maybeContextL typeL)
  = do maybeTyVarBindLs' <- mapM (mapM transTyVarBind)
                              maybeTyVarBindLs
       maybeContextL' <- mapM transContext maybeContextL
       typeL' <- transType typeL
       return (TyForall l maybeTyVarBindLs' maybeContextL' typeL')
transType (TyFun l typeL typeL1)
  = do typeL' <- transType typeL
       typeL1' <- transType typeL1
       return (TyFun l typeL' typeL1')
transType (TyTuple l boxed typeLs)
  = do boxed' <- transBoxed boxed
       typeLs' <- mapM transType typeLs
       return (TyTuple l boxed' typeLs')
transType (TyList l typeL)
  = do typeL' <- transType typeL
       return (TyList l typeL')
transType (TyParArray l typeL)
  = do typeL' <- transType typeL
       return (TyParArray l typeL')
transType (TyApp l typeL typeL1)
  = do typeL' <- transType typeL
       typeL1' <- transType typeL1
       return (TyApp l typeL' typeL1')
transType (TyVar l nameL)
  = do nameL' <- transName nameL
       return (TyVar l nameL')
transType (TyCon l qNameL)
  = do qNameL' <- transQName qNameL
       return (TyCon l qNameL')
transType (TyParen l typeL)
  = do typeL' <- transType typeL
       return (TyParen l typeL')
transType (TyInfix l typeL qNameL typeL1)
  = do typeL' <- transType typeL
       qNameL' <- transQName qNameL
       typeL1' <- transType typeL1
       return (TyInfix l typeL' qNameL' typeL1')
transType (TyKind l typeL kindL)
  = do typeL' <- transType typeL
       kindL' <- transKind kindL
       return (TyKind l typeL' kindL')
transType (TyPromoted l promotedL)
  = do promotedL' <- transPromoted promotedL
       return (TyPromoted l promotedL')
transType (TyEquals l typeL typeL1)
  = do typeL' <- transType typeL
       typeL1' <- transType typeL1
       return (TyEquals l typeL' typeL1')
transType (TySplice l spliceL)
  = do spliceL' <- transSplice spliceL
       return (TySplice l spliceL')
transType (TyBang l bangTypeL typeL)
  = do bangTypeL' <- transBangType bangTypeL
       typeL' <- transType typeL
       return (TyBang l bangTypeL' typeL')
transType (TyWildCard l maybeNameL)
  = do maybeNameL' <- mapM transName maybeNameL
       return (TyWildCard l maybeNameL')

transTypeEqn ::
             TypeEqn SrcSpanInfo -> DesugarM (TypeEqn SrcSpanInfo)
transTypeEqn (TypeEqn l typeL typeL1)
  = do typeL' <- transType typeL
       typeL1' <- transType typeL1
       return (TypeEqn l typeL' typeL1')

transWarningText ::
                 WarningText SrcSpanInfo -> DesugarM (WarningText SrcSpanInfo)
transWarningText (DeprText l string) = return (DeprText l string)
transWarningText (WarnText l string) = return (WarnText l string)

transXAttr :: XAttr SrcSpanInfo -> DesugarM (XAttr SrcSpanInfo)
transXAttr (XAttr l xNameL expL)
  = do xNameL' <- transXName xNameL
       expL' <- transExp expL
       return (XAttr l xNameL' expL')

transXName :: XName SrcSpanInfo -> DesugarM (XName SrcSpanInfo)
transXName (XName l string) = return (XName l string)
transXName (XDomName l string string1)
  = return (XDomName l string string1)
