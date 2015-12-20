module Desugar.If (desugarIf) where

import Language.Haskell.Exts.Annotated.Build
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow (first)

desugarIf :: Module l -> Module l
desugarIf = transModule

transActivation :: Activation l -> Activation l
transActivation (ActiveFrom l int) =
    ActiveFrom l int
transActivation (ActiveUntil l int) =
    ActiveUntil l int

transAlt :: Alt l -> Alt l
transAlt (Alt l pat rhs binds) =
    Alt l (transPat pat) (transRhs rhs) (fmap transBinds binds)

transAnnotation :: Annotation l -> Annotation l
transAnnotation (Ann l name exp) =
    Ann l (transName name) (transExp exp)
transAnnotation (TypeAnn l name exp) =
    TypeAnn l (transName name) (transExp exp)
transAnnotation (ModuleAnn l exp) =
    ModuleAnn l (transExp exp)

transAssoc :: Assoc l -> Assoc l
transAssoc (AssocNone l) =
    AssocNone l
transAssoc (AssocLeft l) =
    AssocLeft l
transAssoc (AssocRight l) =
    AssocRight l

transAsst :: Asst l -> Asst l
transAsst (ClassA l qName type0) =
    ClassA l (transQName qName) (fmap transType type0)
transAsst (AppA l name type0) =
    AppA l (transName name) (fmap transType type0)
transAsst (InfixA l type1 qName type2) =
    InfixA l (transType type1) (transQName qName) (transType type2)
transAsst (IParam l iPName type0) =
    IParam l (transIPName iPName) (transType type0)
transAsst (EqualP l type1 type2) =
    EqualP l (transType type1) (transType type2)
transAsst (ParenA l asst) =
    ParenA l (transAsst asst)
transAsst (WildCardA l name) =
    WildCardA l (fmap transName name)

transBangType :: BangType l -> BangType l
transBangType (BangedTy l) =
    BangedTy l
transBangType (UnpackedTy l) =
    UnpackedTy l

transBinds :: Binds l -> Binds l
transBinds (BDecls l decl) =
    BDecls l (fmap transDecl decl)
transBinds (IPBinds l iPBind) =
    IPBinds l (fmap transIPBind iPBind)

transBooleanFormula :: BooleanFormula l -> BooleanFormula l
transBooleanFormula (VarFormula l name) =
    VarFormula l (transName name)
transBooleanFormula (AndFormula l booleanFormula) =
    AndFormula l (fmap transBooleanFormula booleanFormula)
transBooleanFormula (OrFormula l booleanFormula) =
    OrFormula l (fmap transBooleanFormula booleanFormula)
transBooleanFormula (ParenFormula l booleanFormula) =
    ParenFormula l (transBooleanFormula booleanFormula)

transBoxed :: Boxed -> Boxed
transBoxed Boxed =
    Boxed
transBoxed Unboxed =
    Unboxed

transBracket :: Bracket l -> Bracket l
transBracket (ExpBracket l exp) =
    ExpBracket l (transExp exp)
transBracket (PatBracket l pat) =
    PatBracket l (transPat pat)
transBracket (TypeBracket l type0) =
    TypeBracket l (transType type0)
transBracket (DeclBracket l decl) =
    DeclBracket l (fmap transDecl decl)

transCName :: CName l -> CName l
transCName (VarName l name) =
    VarName l (transName name)
transCName (ConName l name) =
    ConName l (transName name)

transCallConv :: CallConv l -> CallConv l
transCallConv (StdCall l) =
    StdCall l
transCallConv (CCall l) =
    CCall l
transCallConv (CPlusPlus l) =
    CPlusPlus l
transCallConv (DotNet l) =
    DotNet l
transCallConv (Jvm l) =
    Jvm l
transCallConv (Js l) =
    Js l
transCallConv (JavaScript l) =
    JavaScript l
transCallConv (CApi l) =
    CApi l

transClassDecl :: ClassDecl l -> ClassDecl l
transClassDecl (ClsDecl l decl) =
    ClsDecl l (transDecl decl)
transClassDecl (ClsDataFam l context declHead kind) =
    ClsDataFam l (fmap transContext context) (transDeclHead declHead) (fmap transKind kind)
transClassDecl (ClsTyFam l declHead kind) =
    ClsTyFam l (transDeclHead declHead) (fmap transKind kind)
transClassDecl (ClsTyDef l type1 type2) =
    ClsTyDef l (transType type1) (transType type2)
transClassDecl (ClsDefSig l name type0) =
    ClsDefSig l (transName name) (transType type0)

transConDecl :: ConDecl l -> ConDecl l
transConDecl (ConDecl l name type0) =
    ConDecl l (transName name) (fmap transType type0)
transConDecl (InfixConDecl l type1 name type2) =
    InfixConDecl l (transType type1) (transName name) (transType type2)
transConDecl (RecDecl l name fieldDecl) =
    RecDecl l (transName name) (fmap transFieldDecl fieldDecl)

transContext :: Context l -> Context l
transContext (CxSingle l asst) =
    CxSingle l (transAsst asst)
transContext (CxTuple l asst) =
    CxTuple l (fmap transAsst asst)
transContext (CxEmpty l) =
    CxEmpty l

transDataOrNew :: DataOrNew l -> DataOrNew l
transDataOrNew (DataType l) =
    DataType l
transDataOrNew (NewType l) =
    NewType l

transDecl :: Decl l -> Decl l
transDecl (TypeDecl l declHead type0) =
    TypeDecl l (transDeclHead declHead) (transType type0)
transDecl (TypeFamDecl l declHead kind) =
    TypeFamDecl l (transDeclHead declHead) (fmap transKind kind)
transDecl (ClosedTypeFamDecl l declHead kind typeEqn) =
    ClosedTypeFamDecl l (transDeclHead declHead) (fmap transKind kind) (fmap transTypeEqn typeEqn)
transDecl (DataDecl l dataOrNew context declHead qualConDecl deriving0) =
    DataDecl l (transDataOrNew dataOrNew) (fmap transContext context) (transDeclHead declHead) (fmap transQualConDecl qualConDecl) (fmap transDeriving deriving0)
transDecl (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) =
    GDataDecl l (transDataOrNew dataOrNew) (fmap transContext context) (transDeclHead declHead) (fmap transKind kind) (fmap transGadtDecl gadtDecl) (fmap transDeriving deriving0)
transDecl (DataFamDecl l context declHead kind) =
    DataFamDecl l (fmap transContext context) (transDeclHead declHead) (fmap transKind kind)
transDecl (TypeInsDecl l type1 type2) =
    TypeInsDecl l (transType type1) (transType type2)
transDecl (DataInsDecl l dataOrNew type0 qualConDecl deriving0) =
    DataInsDecl l (transDataOrNew dataOrNew) (transType type0) (fmap transQualConDecl qualConDecl) (fmap transDeriving deriving0)
transDecl (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) =
    GDataInsDecl l (transDataOrNew dataOrNew) (transType type0) (fmap transKind kind) (fmap transGadtDecl gadtDecl) (fmap transDeriving deriving0)
transDecl (ClassDecl l context declHead funDep classDecl) =
    ClassDecl l (fmap transContext context) (transDeclHead declHead) (fmap transFunDep funDep) (fmap (fmap transClassDecl) classDecl)
transDecl (InstDecl l overlap instRule instDecl) =
    InstDecl l (fmap transOverlap overlap) (transInstRule instRule) (fmap (fmap transInstDecl) instDecl)
transDecl (DerivDecl l overlap instRule) =
    DerivDecl l (fmap transOverlap overlap) (transInstRule instRule)
transDecl (InfixDecl l assoc int op) =
    InfixDecl l (transAssoc assoc) int (fmap transOp op)
transDecl (DefaultDecl l type0) =
    DefaultDecl l (fmap transType type0)
transDecl (SpliceDecl l exp) =
    SpliceDecl l (transExp exp)
transDecl (TypeSig l name type0) =
    TypeSig l (fmap transName name) (transType type0)
transDecl (PatSynSig l name tyVarBind context1 context2 type0) =
    PatSynSig l (transName name) (fmap (fmap transTyVarBind) tyVarBind) (fmap transContext context1) (fmap transContext context2) (transType type0)
transDecl (FunBind l match) =
    FunBind l (fmap transMatch match)
transDecl (PatBind l pat rhs binds) =
    PatBind l (transPat pat) (transRhs rhs) (fmap transBinds binds)
transDecl (PatSyn l pat1 pat2 patternSynDirection) =
    PatSyn l (transPat pat1) (transPat pat2) (transPatternSynDirection patternSynDirection)
transDecl (ForImp l callConv safety string name type0) =
    ForImp l (transCallConv callConv) (fmap transSafety safety) string (transName name) (transType type0)
transDecl (ForExp l callConv string name type0) =
    ForExp l (transCallConv callConv) string (transName name) (transType type0)
transDecl (RulePragmaDecl l rule) =
    RulePragmaDecl l (fmap transRule rule)
transDecl (DeprPragmaDecl l name) =
    DeprPragmaDecl l (fmap (first (fmap transName)) name)
transDecl (WarnPragmaDecl l name) =
    WarnPragmaDecl l (fmap (first (fmap transName)) name)
transDecl (InlineSig l bool activation qName) =
    InlineSig l bool (fmap transActivation activation) (transQName qName)
transDecl (InlineConlikeSig l activation qName) =
    InlineConlikeSig l (fmap transActivation activation) (transQName qName)
transDecl (SpecSig l activation qName type0) =
    SpecSig l (fmap transActivation activation) (transQName qName) (fmap transType type0)
transDecl (SpecInlineSig l bool activation qName type0) =
    SpecInlineSig l bool (fmap transActivation activation) (transQName qName) (fmap transType type0)
transDecl (InstSig l instRule) =
    InstSig l (transInstRule instRule)
transDecl (AnnPragma l annotation) =
    AnnPragma l (transAnnotation annotation)
transDecl (MinimalPragma l booleanFormula) =
    MinimalPragma l (fmap transBooleanFormula booleanFormula)
transDecl (RoleAnnotDecl l qName r) =
    RoleAnnotDecl l (transQName qName) (fmap transRole r)

transDeclHead :: DeclHead l -> DeclHead l
transDeclHead (DHead l name) =
    DHead l (transName name)
transDeclHead (DHInfix l tyVarBind name) =
    DHInfix l (transTyVarBind tyVarBind) (transName name)
transDeclHead (DHParen l declHead) =
    DHParen l (transDeclHead declHead)
transDeclHead (DHApp l declHead tyVarBind) =
    DHApp l (transDeclHead declHead) (transTyVarBind tyVarBind)

transDeriving :: Deriving l -> Deriving l
transDeriving (Deriving l instRule) =
    Deriving l (fmap transInstRule instRule)

transExp :: Exp l -> Exp l
transExp (Var l qName) =
    Var l (transQName qName)
transExp (IPVar l iPName) =
    IPVar l (transIPName iPName)
transExp (Con l qName) =
    Con l (transQName qName)
transExp (Lit l literal) =
    Lit l (transLiteral literal)
transExp (InfixApp l exp1 qOp exp2) =
    InfixApp l (transExp exp1) (transQOp qOp) (transExp exp2)
transExp (App l exp1 exp2) =
    App l (transExp exp1) (transExp exp2)
transExp (NegApp l exp) =
    NegApp l (transExp exp)
transExp (Lambda l pat exp) =
    Lambda l (fmap transPat pat) (transExp exp)
transExp (Let l binds exp) =
    Let l (transBinds binds) (transExp exp)
transExp (If l exp1 exp2 exp3) =
    caseE l exp1' [ alt l2 (pApp l2 (name l2 "True")  []) exp2'
                  , alt l3 (pApp l3 (name l3 "False") []) exp3' ]
    where l2 = ann exp2
          l3 = ann exp3
          exp1' = transExp exp1
          exp2' = transExp exp2
          exp3' = transExp exp3
transExp (MultiIf l guardedRhs) =
    MultiIf l (fmap transGuardedRhs guardedRhs)
transExp (Case l exp alt) =
    Case l (transExp exp) (fmap transAlt alt)
transExp (Do l stmt) =
    Do l (fmap transStmt stmt)
transExp (MDo l stmt) =
    MDo l (fmap transStmt stmt)
transExp (Tuple l boxed exp) =
    Tuple l (transBoxed boxed) (fmap transExp exp)
transExp (TupleSection l boxed exp) =
    TupleSection l (transBoxed boxed) (fmap (fmap transExp) exp)
transExp (List l exp) =
    List l (fmap transExp exp)
transExp (ParArray l exp) =
    ParArray l (fmap transExp exp)
transExp (Paren l exp) =
    Paren l (transExp exp)
transExp (LeftSection l exp qOp) =
    LeftSection l (transExp exp) (transQOp qOp)
transExp (RightSection l qOp exp) =
    RightSection l (transQOp qOp) (transExp exp)
transExp (RecConstr l qName fieldUpdate) =
    RecConstr l (transQName qName) (fmap transFieldUpdate fieldUpdate)
transExp (RecUpdate l exp fieldUpdate) =
    RecUpdate l (transExp exp) (fmap transFieldUpdate fieldUpdate)
transExp (EnumFrom l exp) =
    EnumFrom l (transExp exp)
transExp (EnumFromTo l exp1 exp2) =
    EnumFromTo l (transExp exp1) (transExp exp2)
transExp (EnumFromThen l exp1 exp2) =
    EnumFromThen l (transExp exp1) (transExp exp2)
transExp (EnumFromThenTo l exp1 exp2 exp3) =
    EnumFromThenTo l (transExp exp1) (transExp exp2) (transExp exp3)
transExp (ParArrayFromTo l exp1 exp2) =
    ParArrayFromTo l (transExp exp1) (transExp exp2)
transExp (ParArrayFromThenTo l exp1 exp2 exp3) =
    ParArrayFromThenTo l (transExp exp1) (transExp exp2) (transExp exp3)
transExp (ListComp l exp qualStmt) =
    ListComp l (transExp exp) (fmap transQualStmt qualStmt)
transExp (ParComp l exp qualStmt) =
    ParComp l (transExp exp) (fmap (fmap transQualStmt) qualStmt)
transExp (ParArrayComp l exp qualStmt) =
    ParArrayComp l (transExp exp) (fmap (fmap transQualStmt) qualStmt)
transExp (ExpTypeSig l exp type0) =
    ExpTypeSig l (transExp exp) (transType type0)
transExp (VarQuote l qName) =
    VarQuote l (transQName qName)
transExp (TypQuote l qName) =
    TypQuote l (transQName qName)
transExp (BracketExp l bracket) =
    BracketExp l (transBracket bracket)
transExp (SpliceExp l splice) =
    SpliceExp l (transSplice splice)
transExp (QuasiQuote l string1 string2) =
    QuasiQuote l string1 string2
transExp (XTag l xName xAttr exp1 exp2) =
    XTag l (transXName xName) (fmap transXAttr xAttr) (fmap transExp exp1) (fmap transExp exp2)
transExp (XETag l xName xAttr exp) =
    XETag l (transXName xName) (fmap transXAttr xAttr) (fmap transExp exp)
transExp (XPcdata l string) =
    XPcdata l string
transExp (XExpTag l exp) =
    XExpTag l (transExp exp)
transExp (XChildTag l exp) =
    XChildTag l (fmap transExp exp)
transExp (CorePragma l string exp) =
    CorePragma l string (transExp exp)
transExp (SCCPragma l string exp) =
    SCCPragma l string (transExp exp)
transExp (GenPragma l string int1 int2 exp) =
    GenPragma l string int1 int2 (transExp exp)
transExp (Proc l pat exp) =
    Proc l (transPat pat) (transExp exp)
transExp (LeftArrApp l exp1 exp2) =
    LeftArrApp l (transExp exp1) (transExp exp2)
transExp (RightArrApp l exp1 exp2) =
    RightArrApp l (transExp exp1) (transExp exp2)
transExp (LeftArrHighApp l exp1 exp2) =
    LeftArrHighApp l (transExp exp1) (transExp exp2)
transExp (RightArrHighApp l exp1 exp2) =
    RightArrHighApp l (transExp exp1) (transExp exp2)
transExp (LCase l alt) =
    LCase l (fmap transAlt alt)
transExp (ExprHole l) =
    ExprHole l

transExportSpec :: ExportSpec l -> ExportSpec l
transExportSpec (EVar l qName) =
    EVar l (transQName qName)
transExportSpec (EAbs l namespace qName) =
    EAbs l (transNamespace namespace) (transQName qName)
transExportSpec (EThingAll l qName) =
    EThingAll l (transQName qName)
transExportSpec (EThingWith l qName cName) =
    EThingWith l (transQName qName) (fmap transCName cName)
transExportSpec (EModuleContents l moduleName) =
    EModuleContents l (transModuleName moduleName)

transExportSpecList :: ExportSpecList l -> ExportSpecList l
transExportSpecList (ExportSpecList l exportSpec) =
    ExportSpecList l (fmap transExportSpec exportSpec)

transFieldDecl :: FieldDecl l -> FieldDecl l
transFieldDecl (FieldDecl l name type0) =
    FieldDecl l (fmap transName name) (transType type0)

transFieldUpdate :: FieldUpdate l -> FieldUpdate l
transFieldUpdate (FieldUpdate l qName exp) =
    FieldUpdate l (transQName qName) (transExp exp)
transFieldUpdate (FieldPun l qName) =
    FieldPun l (transQName qName)
transFieldUpdate (FieldWildcard l) =
    FieldWildcard l

transFunDep :: FunDep l -> FunDep l
transFunDep (FunDep l name1 name2) =
    FunDep l (fmap transName name1) (fmap transName name2)

transGadtDecl :: GadtDecl l -> GadtDecl l
transGadtDecl (GadtDecl l name fieldDecl type0) =
    GadtDecl l (transName name) (fmap (fmap transFieldDecl) fieldDecl) (transType type0)

transGuardedRhs :: GuardedRhs l -> GuardedRhs l
transGuardedRhs (GuardedRhs l stmt exp) =
    GuardedRhs l (fmap transStmt stmt) (transExp exp)

transIPBind :: IPBind l -> IPBind l
transIPBind (IPBind l iPName exp) =
    IPBind l (transIPName iPName) (transExp exp)

transIPName :: IPName l -> IPName l
transIPName (IPDup l string) =
    IPDup l string
transIPName (IPLin l string) =
    IPLin l string

transImportDecl :: ImportDecl l -> ImportDecl l
transImportDecl (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) =
    ImportDecl importAnn (transModuleName importModule) importQualified importSrc importSafe importPkg (fmap transModuleName importAs) (fmap transImportSpecList importSpecs)

transImportSpec :: ImportSpec l -> ImportSpec l
transImportSpec (IVar l name) =
    IVar l (transName name)
transImportSpec (IAbs l namespace name) =
    IAbs l (transNamespace namespace) (transName name)
transImportSpec (IThingAll l name) =
    IThingAll l (transName name)
transImportSpec (IThingWith l name cName) =
    IThingWith l (transName name) (fmap transCName cName)

transImportSpecList :: ImportSpecList l -> ImportSpecList l
transImportSpecList (ImportSpecList l bool importSpec) =
    ImportSpecList l bool (fmap transImportSpec importSpec)

transInstDecl :: InstDecl l -> InstDecl l
transInstDecl (InsDecl l decl) =
    InsDecl l (transDecl decl)
transInstDecl (InsType l type1 type2) =
    InsType l (transType type1) (transType type2)
transInstDecl (InsData l dataOrNew type0 qualConDecl deriving0) =
    InsData l (transDataOrNew dataOrNew) (transType type0) (fmap transQualConDecl qualConDecl) (fmap transDeriving deriving0)
transInstDecl (InsGData l dataOrNew type0 kind gadtDecl deriving0) =
    InsGData l (transDataOrNew dataOrNew) (transType type0) (fmap transKind kind) (fmap transGadtDecl gadtDecl) (fmap transDeriving deriving0)

transInstHead :: InstHead l -> InstHead l
transInstHead (IHCon l qName) =
    IHCon l (transQName qName)
transInstHead (IHInfix l type0 qName) =
    IHInfix l (transType type0) (transQName qName)
transInstHead (IHParen l instHead) =
    IHParen l (transInstHead instHead)
transInstHead (IHApp l instHead type0) =
    IHApp l (transInstHead instHead) (transType type0)

transInstRule :: InstRule l -> InstRule l
transInstRule (IRule l tyVarBind context instHead) =
    IRule l (fmap (fmap transTyVarBind) tyVarBind) (fmap transContext context) (transInstHead instHead)
transInstRule (IParen l instRule) =
    IParen l (transInstRule instRule)

transKind :: Kind l -> Kind l
transKind (KindStar l) =
    KindStar l
transKind (KindFn l kind1 kind2) =
    KindFn l (transKind kind1) (transKind kind2)
transKind (KindParen l kind) =
    KindParen l (transKind kind)
transKind (KindVar l qName) =
    KindVar l (transQName qName)
transKind (KindApp l kind1 kind2) =
    KindApp l (transKind kind1) (transKind kind2)
transKind (KindTuple l kind) =
    KindTuple l (fmap transKind kind)
transKind (KindList l kind) =
    KindList l (transKind kind)

transLiteral :: Literal l -> Literal l
transLiteral (Char l char string) =
    Char l char string
transLiteral (String l string1 string2) =
    String l string1 string2
transLiteral (Int l integer string) =
    Int l integer string
transLiteral (Frac l rational string) =
    Frac l rational string
transLiteral (PrimInt l integer string) =
    PrimInt l integer string
transLiteral (PrimWord l integer string) =
    PrimWord l integer string
transLiteral (PrimFloat l rational string) =
    PrimFloat l rational string
transLiteral (PrimDouble l rational string) =
    PrimDouble l rational string
transLiteral (PrimChar l char string) =
    PrimChar l char string
transLiteral (PrimString l string1 string2) =
    PrimString l string1 string2

transMatch :: Match l -> Match l
transMatch (Match l name pat rhs binds) =
    Match l (transName name) (fmap transPat pat) (transRhs rhs) (fmap transBinds binds)
transMatch (InfixMatch l pat1 name pat2 rhs binds) =
    InfixMatch l (transPat pat1) (transName name) (fmap transPat pat2) (transRhs rhs) (fmap transBinds binds)

transModule :: Module l -> Module l
transModule (Module l moduleHead modulePragma importDecl decl) =
    Module l (fmap transModuleHead moduleHead) (fmap transModulePragma modulePragma) (fmap transImportDecl importDecl) (fmap transDecl decl)
transModule (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) =
    XmlPage l (transModuleName moduleName) (fmap transModulePragma modulePragma) (transXName xName) (fmap transXAttr xAttr) (fmap transExp exp1) (fmap transExp exp2)
transModule (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) =
    XmlHybrid l (fmap transModuleHead moduleHead) (fmap transModulePragma modulePragma) (fmap transImportDecl importDecl) (fmap transDecl decl) (transXName xName) (fmap transXAttr xAttr) (fmap transExp exp1) (fmap transExp exp2)

transModuleHead :: ModuleHead l -> ModuleHead l
transModuleHead (ModuleHead l moduleName warningText exportSpecList) =
    ModuleHead l (transModuleName moduleName) (fmap transWarningText warningText) (fmap transExportSpecList exportSpecList)

transModuleName :: ModuleName l -> ModuleName l
transModuleName (ModuleName l string) =
    ModuleName l string

transModulePragma :: ModulePragma l -> ModulePragma l
transModulePragma (LanguagePragma l name) =
    LanguagePragma l (fmap transName name)
transModulePragma (OptionsPragma l tool string) =
    OptionsPragma l (fmap transTool tool) string
transModulePragma (AnnModulePragma l annotation) =
    AnnModulePragma l (transAnnotation annotation)

transName :: Name l -> Name l
transName (Ident l string) =
    Ident l string
transName (Symbol l string) =
    Symbol l string

transNamespace :: Namespace l -> Namespace l
transNamespace (NoNamespace l) =
    NoNamespace l
transNamespace (TypeNamespace l) =
    TypeNamespace l
transNamespace (PatternNamespace l) =
    PatternNamespace l

transOp :: Op l -> Op l
transOp (VarOp l name) =
    VarOp l (transName name)
transOp (ConOp l name) =
    ConOp l (transName name)

transOverlap :: Overlap l -> Overlap l
transOverlap (NoOverlap l) =
    NoOverlap l
transOverlap (Overlap l) =
    Overlap l
transOverlap (Incoherent l) =
    Incoherent l

transPXAttr :: PXAttr l -> PXAttr l
transPXAttr (PXAttr l xName pat) =
    PXAttr l (transXName xName) (transPat pat)

transPat :: Pat l -> Pat l
transPat (PVar l name) =
    PVar l (transName name)
transPat (PLit l sign literal) =
    PLit l (transSign sign) (transLiteral literal)
transPat (PNPlusK l name integer) =
    PNPlusK l (transName name) integer
transPat (PInfixApp l pat1 qName pat2) =
    PInfixApp l (transPat pat1) (transQName qName) (transPat pat2)
transPat (PApp l qName pat) =
    PApp l (transQName qName) (fmap transPat pat)
transPat (PTuple l boxed pat) =
    PTuple l (transBoxed boxed) (fmap transPat pat)
transPat (PList l pat) =
    PList l (fmap transPat pat)
transPat (PParen l pat) =
    PParen l (transPat pat)
transPat (PRec l qName patField) =
    PRec l (transQName qName) (fmap transPatField patField)
transPat (PAsPat l name pat) =
    PAsPat l (transName name) (transPat pat)
transPat (PWildCard l) =
    PWildCard l
transPat (PIrrPat l pat) =
    PIrrPat l (transPat pat)
transPat (PatTypeSig l pat type0) =
    PatTypeSig l (transPat pat) (transType type0)
transPat (PViewPat l exp pat) =
    PViewPat l (transExp exp) (transPat pat)
transPat (PRPat l rPat) =
    PRPat l (fmap transRPat rPat)
transPat (PXTag l xName pXAttr pat1 pat2) =
    PXTag l (transXName xName) (fmap transPXAttr pXAttr) (fmap transPat pat1) (fmap transPat pat2)
transPat (PXETag l xName pXAttr pat) =
    PXETag l (transXName xName) (fmap transPXAttr pXAttr) (fmap transPat pat)
transPat (PXPcdata l string) =
    PXPcdata l string
transPat (PXPatTag l pat) =
    PXPatTag l (transPat pat)
transPat (PXRPats l rPat) =
    PXRPats l (fmap transRPat rPat)
transPat (PQuasiQuote l string1 string2) =
    PQuasiQuote l string1 string2
transPat (PBangPat l pat) =
    PBangPat l (transPat pat)

transPatField :: PatField l -> PatField l
transPatField (PFieldPat l qName pat) =
    PFieldPat l (transQName qName) (transPat pat)
transPatField (PFieldPun l qName) =
    PFieldPun l (transQName qName)
transPatField (PFieldWildcard l) =
    PFieldWildcard l

transPatternSynDirection :: PatternSynDirection l -> PatternSynDirection l
transPatternSynDirection Unidirectional =
    Unidirectional
transPatternSynDirection ImplicitBidirectional =
    ImplicitBidirectional
transPatternSynDirection (ExplicitBidirectional l decl) =
    ExplicitBidirectional l (fmap transDecl decl)

transPromoted :: Promoted l -> Promoted l
transPromoted (PromotedInteger l integer string) =
    PromotedInteger l integer string
transPromoted (PromotedString l string1 string2) =
    PromotedString l string1 string2
transPromoted (PromotedCon l bool qName) =
    PromotedCon l bool (transQName qName)
transPromoted (PromotedList l bool type0) =
    PromotedList l bool (fmap transType type0)
transPromoted (PromotedTuple l type0) =
    PromotedTuple l (fmap transType type0)
transPromoted (PromotedUnit l) =
    PromotedUnit l

transQName :: QName l -> QName l
transQName (Qual l moduleName name) =
    Qual l (transModuleName moduleName) (transName name)
transQName (UnQual l name) =
    UnQual l (transName name)
transQName (Special l specialCon) =
    Special l (transSpecialCon specialCon)

transQOp :: QOp l -> QOp l
transQOp (QVarOp l qName) =
    QVarOp l (transQName qName)
transQOp (QConOp l qName) =
    QConOp l (transQName qName)

transQualConDecl :: QualConDecl l -> QualConDecl l
transQualConDecl (QualConDecl l tyVarBind context conDecl) =
    QualConDecl l (fmap (fmap transTyVarBind) tyVarBind) (fmap transContext context) (transConDecl conDecl)

transQualStmt :: QualStmt l -> QualStmt l
transQualStmt (QualStmt l stmt) =
    QualStmt l (transStmt stmt)
transQualStmt (ThenTrans l exp) =
    ThenTrans l (transExp exp)
transQualStmt (ThenBy l exp1 exp2) =
    ThenBy l (transExp exp1) (transExp exp2)
transQualStmt (GroupBy l exp) =
    GroupBy l (transExp exp)
transQualStmt (GroupUsing l exp) =
    GroupUsing l (transExp exp)
transQualStmt (GroupByUsing l exp1 exp2) =
    GroupByUsing l (transExp exp1) (transExp exp2)

transRPat :: RPat l -> RPat l
transRPat (RPOp l rPat rPatOp) =
    RPOp l (transRPat rPat) (transRPatOp rPatOp)
transRPat (RPEither l rPat1 rPat2) =
    RPEither l (transRPat rPat1) (transRPat rPat2)
transRPat (RPSeq l rPat) =
    RPSeq l (fmap transRPat rPat)
transRPat (RPGuard l pat stmt) =
    RPGuard l (transPat pat) (fmap transStmt stmt)
transRPat (RPCAs l name rPat) =
    RPCAs l (transName name) (transRPat rPat)
transRPat (RPAs l name rPat) =
    RPAs l (transName name) (transRPat rPat)
transRPat (RPParen l rPat) =
    RPParen l (transRPat rPat)
transRPat (RPPat l pat) =
    RPPat l (transPat pat)

transRPatOp :: RPatOp l -> RPatOp l
transRPatOp (RPStar l) =
    RPStar l
transRPatOp (RPStarG l) =
    RPStarG l
transRPatOp (RPPlus l) =
    RPPlus l
transRPatOp (RPPlusG l) =
    RPPlusG l
transRPatOp (RPOpt l) =
    RPOpt l
transRPatOp (RPOptG l) =
    RPOptG l

transRhs :: Rhs l -> Rhs l
transRhs (UnGuardedRhs l exp) =
    UnGuardedRhs l (transExp exp)
transRhs (GuardedRhss l guardedRhs) =
    GuardedRhss l (fmap transGuardedRhs guardedRhs)

transRole :: Role l -> Role l
transRole (Nominal l) =
    Nominal l
transRole (Representational l) =
    Representational l
transRole (Phantom l) =
    Phantom l
transRole (RoleWildcard l) =
    RoleWildcard l

transRule :: Rule l -> Rule l
transRule (Rule l string activation ruleVar exp1 exp2) =
    Rule l string (fmap transActivation activation) (fmap (fmap transRuleVar) ruleVar) (transExp exp1) (transExp exp2)

transRuleVar :: RuleVar l -> RuleVar l
transRuleVar (RuleVar l name) =
    RuleVar l (transName name)
transRuleVar (TypedRuleVar l name type0) =
    TypedRuleVar l (transName name) (transType type0)

transSafety :: Safety l -> Safety l
transSafety (PlayRisky l) =
    PlayRisky l
transSafety (PlaySafe l bool) =
    PlaySafe l bool
transSafety (PlayInterruptible l) =
    PlayInterruptible l

transSign :: Sign l -> Sign l
transSign (Signless l) =
    Signless l
transSign (Negative l) =
    Negative l

transSpecialCon :: SpecialCon l -> SpecialCon l
transSpecialCon (UnitCon l) =
    UnitCon l
transSpecialCon (ListCon l) =
    ListCon l
transSpecialCon (FunCon l) =
    FunCon l
transSpecialCon (TupleCon l boxed int) =
    TupleCon l (transBoxed boxed) int
transSpecialCon (Cons l) =
    Cons l
transSpecialCon (UnboxedSingleCon l) =
    UnboxedSingleCon l

transSplice :: Splice l -> Splice l
transSplice (IdSplice l string) =
    IdSplice l string
transSplice (ParenSplice l exp) =
    ParenSplice l (transExp exp)

transStmt :: Stmt l -> Stmt l
transStmt (Generator l pat exp) =
    Generator l (transPat pat) (transExp exp)
transStmt (Qualifier l exp) =
    Qualifier l (transExp exp)
transStmt (LetStmt l binds) =
    LetStmt l (transBinds binds)
transStmt (RecStmt l stmt) =
    RecStmt l (fmap transStmt stmt)

transTool :: Tool -> Tool
transTool GHC =
    GHC
transTool HUGS =
    HUGS
transTool NHC98 =
    NHC98
transTool YHC =
    YHC
transTool HADDOCK =
    HADDOCK
transTool (UnknownTool string) =
    UnknownTool string


transTyVarBind :: TyVarBind l -> TyVarBind l
transTyVarBind (KindedVar l name kind) =
    KindedVar l (transName name) (transKind kind)
transTyVarBind (UnkindedVar l name) =
    UnkindedVar l (transName name)


transType :: Type l -> Type l
transType (TyForall l tyVarBind context type0) =
    TyForall l (fmap (fmap transTyVarBind) tyVarBind) (fmap transContext context) (transType type0)
transType (TyFun l type1 type2) =
    TyFun l (transType type1) (transType type2)
transType (TyTuple l boxed type0) =
    TyTuple l (transBoxed boxed) (fmap transType type0)
transType (TyList l type0) =
    TyList l (transType type0)
transType (TyParArray l type0) =
    TyParArray l (transType type0)
transType (TyApp l type1 type2) =
    TyApp l (transType type1) (transType type2)
transType (TyVar l name) =
    TyVar l (transName name)
transType (TyCon l qName) =
    TyCon l (transQName qName)
transType (TyParen l type0) =
    TyParen l (transType type0)
transType (TyInfix l type1 qName type2) =
    TyInfix l (transType type1) (transQName qName) (transType type2)
transType (TyKind l type0 kind) =
    TyKind l (transType type0) (transKind kind)
transType (TyPromoted l promoted) =
    TyPromoted l (transPromoted promoted)
transType (TyEquals l type1 type2) =
    TyEquals l (transType type1) (transType type2)
transType (TySplice l splice) =
    TySplice l (transSplice splice)
transType (TyBang l bangType type0) =
    TyBang l (transBangType bangType) (transType type0)
transType (TyWildCard l name) =
    TyWildCard l (fmap transName name)

transTypeEqn :: TypeEqn l -> TypeEqn l
transTypeEqn (TypeEqn l type1 type2) =
    TypeEqn l (transType type1) (transType type2)

transWarningText :: WarningText l -> WarningText l
transWarningText (DeprText l string) =
    DeprText l string
transWarningText (WarnText l string) =
    WarnText l string

transXAttr :: XAttr l -> XAttr l
transXAttr (XAttr l xName exp) =
    XAttr l (transXName xName) (transExp exp)

transXName :: XName l -> XName l
transXName (XName l string) =
    XName l string
transXName (XDomName l string1 string2) =
    XDomName l string1 string2

