module Fay.Compiler.Desugar
  (desugarModule
  ) where

import           Control.Applicative
import           Data.Maybe
import           Language.Haskell.Exts.Annotated hiding (binds, loc)
import           Prelude                         hiding (exp)


desugarModule :: Module l -> Module l
desugarModule m = case m of
  Module l h ps is decls -> Module l h ps is (map desugarDecl decls)
  _ -> m

desugarDecl :: Decl l -> Decl l
desugarDecl d = case d of
  FunBind l ms -> FunBind l (map desugarMatch ms)
  PatBind l p mt rhs mbs -> PatBind l (desugarPat p) (desugarType <$> mt) (desugarRhs rhs) (desugarBinds <$> mbs)
  _ -> d
  -- TODO

desugarBinds :: Binds l -> Binds l
desugarBinds bs = case bs of
  BDecls l ds -> BDecls l $ map desugarDecl ds
  _ -> bs

desugarMatch :: Match l -> Match l
desugarMatch m = case m of
  Match l n ps rhs mb -> Match l n ps (desugarRhs rhs) (desugarBinds <$> mb)
  _ -> m

desugarRhs :: Rhs l -> Rhs l
desugarRhs r = case r of
  UnGuardedRhs l e -> UnGuardedRhs l (desugarExp e)
  GuardedRhss l gs -> GuardedRhss l (map desugarGuardedRhs gs)

desugarGuardedRhs :: GuardedRhs l -> GuardedRhs l
desugarGuardedRhs g = case g of
  GuardedRhs l stmts exp -> GuardedRhs l stmts (desugarExp exp)

desugarExp :: Exp l -> Exp l
desugarExp ex = case ex of
  Var{} -> ex
  IPVar{} -> ex
  Con{} -> ex
  Lit{} -> ex
  InfixApp l e1 qop e2 -> InfixApp l (desugarExp e1) (desugarQOp qop) (desugarExp e2)
  App l e1 e2 -> App l (desugarExp e1) (desugarExp e2)
  NegApp l e -> NegApp l (desugarExp e)
  Lambda l ps e -> Lambda l (map desugarPat ps) (desugarExp e)
  Let l b e -> Let l (desugarBinds b) (desugarExp e)
  If l e1 e2 e3 -> If l (desugarExp e1) (desugarExp e2) (desugarExp e3)
  Case l e as -> Case l (desugarExp e) (map desugarAlt as)
  Do _ stmts -> fromMaybe (error "EmptyDoBlock") $ foldl desugarStmt' Nothing (reverse stmts)
  MDo l ss -> MDo l (map desugarStmt ss)
  Tuple l b es -> Tuple l b (map desugarExp es)
  TupleSection l b mes -> TupleSection l b (map (desugarExp <$>) mes)
  List l es -> List l (map desugarExp es)
  Paren l e -> Paren l (desugarExp e)
  LeftSection l e q -> LeftSection l (desugarExp e) (desugarQOp q)
  RightSection l q e -> RightSection l (desugarQOp q) (desugarExp e)
  RecConstr l q f -> RecConstr l (desugarQName q) (map desugarFieldUpdate f)
  RecUpdate l e f -> RecUpdate l (desugarExp e) (map desugarFieldUpdate f)
  EnumFrom l e -> EnumFrom l (desugarExp e)
  EnumFromTo l e1 e2 -> EnumFromTo l (desugarExp e1) (desugarExp e2)
  EnumFromThen l e1 e2 -> EnumFromThen l (desugarExp e1) (desugarExp e2)
  EnumFromThenTo l e1 e2 e3 -> EnumFromThenTo l (desugarExp e1) (desugarExp e2) (desugarExp e3)
  ListComp l e qs -> ListComp l (desugarExp e) (map desugarQualStmt qs)
  ParComp l e qqs -> ParComp l (desugarExp e) (map (map desugarQualStmt) qqs)
  ExpTypeSig l e t -> ExpTypeSig l (desugarExp e) (desugarType t)
  VarQuote l q -> VarQuote l (desugarQName q)
  TypQuote l q -> TypQuote l (desugarQName q)
  BracketExp l b -> BracketExp l (desugarBracket b)
  SpliceExp l s -> SpliceExp l (desugarSplice s)
  QuasiQuote{} -> ex
  XTag{} -> ex
  XETag{} -> ex
  XPcdata{} -> ex
  XExpTag{} -> ex
  XChildTag{} -> ex
  GenPragma{} -> ex
  Proc l p e -> Proc l (desugarPat p) (desugarExp e)
  LeftArrApp{} -> ex
  RightArrApp{} -> ex
  LeftArrHighApp{} -> ex
  RightArrHighApp{} -> ex
  CorePragma{} -> ex
  SCCPragma{} -> ex


desugarStmt' :: Maybe (Exp l) -> (Stmt l) -> Maybe (Exp l)
desugarStmt' inner stmt =
  maybe initStmt subsequentStmt inner
  where
    initStmt = case stmt of
      Qualifier _ exp -> Just (desugarExp exp)
      LetStmt{}     -> error "UnsupportedLet"
      _             -> error "InvalidDoBlock"

    subsequentStmt inner' = case stmt of
      Generator loc pat exp -> desugarGenerator loc pat inner' exp
      Qualifier s exp -> Just $ desugarExp $ InfixApp s exp
                                               (QVarOp s $ UnQual s $ Symbol s ">>")
                                               inner'
      LetStmt _ (BDecls s binds) -> Just $ desugarExp $ Let s (BDecls s binds) inner'
      LetStmt _ _ -> error "UnsupportedLet"
      RecStmt{} -> error "UnsupportedRecursiveDo"

    desugarGenerator :: l -> Pat l -> Exp l -> Exp l -> Maybe (Exp l)
    desugarGenerator s pat inner' exp =
      Just $ desugarExp $ InfixApp s
                      exp
                      (QVarOp s $ UnQual s $ Symbol s ">>=")
                      (Lambda s [pat] (inner'))

desugarPat :: Pat l -> Pat l
desugarPat = id

desugarGuardedAlts :: GuardedAlts l -> GuardedAlts l
desugarGuardedAlts g = case g of
  UnGuardedAlt l e -> UnGuardedAlt l (desugarExp e)
  GuardedAlts l gas -> GuardedAlts l (map desugarGuardedAlt gas)

desugarQOp :: QOp l -> QOp l
desugarQOp = id

desugarType :: Type l -> Type l
desugarType = id

desugarQName :: QName l -> QName l
desugarQName = id

desugarQualStmt :: QualStmt l -> QualStmt l
desugarQualStmt q = case q of
  QualStmt l s -> QualStmt l (desugarStmt s)
  ThenTrans l e -> ThenTrans l (desugarExp e)
  ThenBy l e1 e2 -> ThenBy l (desugarExp e1) (desugarExp e2)
  GroupBy l e -> GroupBy l (desugarExp e)
  GroupUsing l e -> GroupUsing l (desugarExp e)
  GroupByUsing l e1 e2 -> GroupByUsing l (desugarExp e1) (desugarExp e2)

desugarAlt :: Alt l -> Alt l
desugarAlt (Alt l p ga mb) = Alt l (desugarPat p) (desugarGuardedAlts ga) (desugarBinds <$> mb)

desugarFieldUpdate :: FieldUpdate l -> FieldUpdate l
desugarFieldUpdate f = case f of
  FieldUpdate l q e -> FieldUpdate l (desugarQName q) (desugarExp e)
  FieldPun l n -> FieldPun l (desugarName n)
  FieldWildcard{} -> f

desugarBracket :: Bracket l -> Bracket l
desugarBracket = id

desugarSplice :: Splice l -> Splice l
desugarSplice = id

desugarGuardedAlt :: GuardedAlt l -> GuardedAlt l
desugarGuardedAlt (GuardedAlt l ss e) = GuardedAlt l (map desugarStmt ss) (desugarExp e)

desugarStmt :: Stmt l -> Stmt l
desugarStmt s = case s of
  Generator l p e -> Generator l (desugarPat p) (desugarExp e)
  Qualifier l e -> Qualifier l (desugarExp e)
  LetStmt l b -> LetStmt l (desugarBinds b)
  RecStmt l ss -> RecStmt l (map desugarStmt ss)

desugarName :: Name a -> Name a
desugarName = id
