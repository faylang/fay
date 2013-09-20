{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Fay.Compiler.Desugar
  (desugar
  ) where

import           Fay.Types                       (CompileError (..))

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Maybe
import           Language.Haskell.Exts.Annotated hiding (binds, loc)
import           Prelude                         hiding (exp)


-- Types

data DesugarReader = DesugarReader { readerNameDepth :: Int }

newtype Desugar a = Desugar
  { unDesugar :: (ReaderT DesugarReader
                       (ErrorT CompileError IO))
                       a
  } deriving ( MonadReader DesugarReader
             , MonadError CompileError
             , MonadIO
             , Monad
             , Functor
             , Applicative
             )

runDesugar :: Desugar a -> IO (Either CompileError a)
runDesugar m = runErrorT (runReaderT (unDesugar m) (DesugarReader 0))

-- | Generate a temporary, SCOPED name for testing conditions and
-- such. We don't have name tracking yet, so instead we use this.
withScopedTmpName :: l -> (Name l -> Desugar a) -> Desugar a
withScopedTmpName l f = do
  n <- asks readerNameDepth
  local (\r -> DesugarReader $ readerNameDepth r + 1) $
   f $ Ident l $ "$gen" ++ show n


-- | Top level, desugar a whole module possibly returning errors
desugar :: Module l -> IO (Either CompileError (Module l))
desugar md = runDesugar (desugarModule md)

-- | Desugaring

desugarModule :: Module l -> Desugar (Module l)
desugarModule m = case m of
  Module l h ps is decls -> Module l h ps is <$> mapM desugarDecl decls
  _ -> return $ m

desugarDecl :: Decl l -> Desugar (Decl l)
desugarDecl d = case d of
  FunBind l ms -> FunBind l <$> mapM desugarMatch ms
  PatBind l p mt rhs mbs -> PatBind l <$> desugarPat p <*> return mt <*> desugarRhs rhs <*> mmap desugarBinds mbs

  _ -> return d

mmap :: (Applicative f) => (t -> f a) -> Maybe t -> f (Maybe a)
mmap f mbs' = case mbs' of Just b -> return <$> f b; Nothing -> pure Nothing

desugarBinds :: Binds l -> Desugar (Binds l)
desugarBinds bs = case bs of
  BDecls l ds -> BDecls l <$> mapM desugarDecl ds
  _ -> return bs

desugarMatch :: Match l -> Desugar (Match l)
desugarMatch m = case m of
  Match l n ps rhs mb -> Match l (desugarName n) <$> mapM desugarPat ps <*> desugarRhs rhs <*> mmap desugarBinds mb
  InfixMatch l p n ps r mb -> InfixMatch l <$> desugarPat p <*> return (desugarName n) <*> mapM desugarPat ps <*> desugarRhs r <*> mmap desugarBinds mb

desugarRhs :: Rhs l -> Desugar (Rhs l)
desugarRhs r = case r of
  UnGuardedRhs l e -> UnGuardedRhs l <$> desugarExp e
  GuardedRhss l gs -> GuardedRhss l <$> mapM desugarGuardedRhs gs

desugarGuardedRhs :: GuardedRhs l -> Desugar (GuardedRhs l)
desugarGuardedRhs g = case g of
  GuardedRhs l stmts exp -> GuardedRhs l <$> mapM desugarStmt stmts <*> desugarExp exp

desugarExp :: Exp l -> Desugar (Exp l)
desugarExp ex = case ex of
  -- (a `f`) => (\b -> f a b)
  LeftSection l e q -> desugarExp =<<
    (withScopedTmpName l $ \v ->
      return $ Lambda l [PVar l v] (InfixApp l e q (Var l (UnQual l v))))
  -- (`f` b) => (\a -> f a b)
  RightSection l q e -> desugarExp =<<
    (withScopedTmpName l $ \tmp ->
      return (Lambda l [PVar l tmp] (InfixApp l (Var l (UnQual l tmp)) q e)))

  -- Check for TupleCon
  Var _ q -> return $ desugarVar ex q
  Con _ q -> return $ desugarVar ex q

  IPVar{} -> return ex
  Lit{} -> return ex
  InfixApp l e1 qop e2 -> InfixApp l <$> desugarExp e1 <*> return (desugarQOp qop) <*> desugarExp e2
  App l e1 e2 -> App l <$> desugarExp e1 <*> desugarExp e2
  NegApp l e -> NegApp l <$> desugarExp e
  Lambda l ps e -> Lambda l <$> mapM desugarPat ps <*> desugarExp e
  Let l b e -> Let l <$> desugarBinds b <*> desugarExp e
  If l e1 e2 e3 -> If l <$> desugarExp e1 <*> desugarExp e2 <*> desugarExp e3
  Case l e as -> Case l <$> desugarExp e <*> mapM desugarAlt as
  Do _ stmts -> maybe (throwError EmptyDoBlock) return =<< (mmap desugarExp $ foldl desugarStmt' Nothing (reverse stmts))
  MDo l ss -> MDo l <$> mapM desugarStmt ss
  Tuple l b es -> Tuple l b <$> mapM desugarExp es
  TupleSection l b mes -> TupleSection l b <$> mapM (mmap desugarExp) mes
  List l es -> List l <$> mapM desugarExp es
  Paren l e -> Paren l <$> desugarExp e
  RecConstr l q f -> RecConstr l (desugarQName q) <$> mapM desugarFieldUpdate f
  RecUpdate l e f -> RecUpdate l <$> desugarExp e <*> mapM desugarFieldUpdate f
  EnumFrom l e -> EnumFrom l <$> desugarExp e
  EnumFromTo l e1 e2 -> EnumFromTo l <$> desugarExp e1 <*> desugarExp e2
  EnumFromThen l e1 e2 -> EnumFromThen l <$> desugarExp e1 <*> desugarExp e2
  EnumFromThenTo l e1 e2 e3 -> EnumFromThenTo l <$> desugarExp e1 <*> desugarExp e2 <*> desugarExp e3
  ListComp l e qs -> ListComp l <$> desugarExp e <*> mapM desugarQualStmt qs
  ParComp l e qqs -> ParComp l <$> desugarExp e <*> mapM (mapM desugarQualStmt) qqs
  ExpTypeSig l e t -> ExpTypeSig l <$> desugarExp e <*> return (desugarType t)
  VarQuote l q -> return $ VarQuote l (desugarQName q)
  TypQuote l q -> return $ TypQuote l (desugarQName q)
  BracketExp l b -> return $ BracketExp l (desugarBracket b)
  SpliceExp l s -> return $ SpliceExp l (desugarSplice s)
  QuasiQuote{} -> return ex
  XTag{} -> return ex
  XETag{} -> return ex
  XPcdata{} -> return ex
  XExpTag{} -> return ex
  XChildTag{} -> return ex
  GenPragma{} -> return ex
  Proc l p e -> Proc l <$> desugarPat p <*> desugarExp e
  LeftArrApp{} -> return ex
  RightArrApp{} -> return ex
  LeftArrHighApp{} -> return ex
  RightArrHighApp{} -> return ex
  CorePragma{} -> return ex
  SCCPragma{} -> return ex

-- | Convert do notation into binds and thens.
desugarStmt' :: Maybe (Exp l) -> (Stmt l) -> Maybe (Exp l)
desugarStmt' inner stmt =
  maybe initStmt subsequentStmt inner
  where
    initStmt = case stmt of
      Qualifier _ exp -> Just exp
      LetStmt{}     -> error "UnsupportedLet"
      _             -> error "InvalidDoBlock"

    subsequentStmt inner' = case stmt of
      Generator loc pat exp -> desugarGenerator loc pat inner' exp
      Qualifier s exp -> Just $ InfixApp s exp
                                         (QVarOp s $ UnQual s $ Symbol s ">>")
                                         inner'
      LetStmt _ (BDecls s binds) -> Just $ Let s (BDecls s binds) inner'
      LetStmt _ _ -> error "UnsupportedLet"
      RecStmt{} -> error "UnsupportedRecursiveDo"

    desugarGenerator :: l -> Pat l -> Exp l -> Exp l -> Maybe (Exp l)
    desugarGenerator s pat inner' exp =
      Just $ InfixApp s
                      exp
                      (QVarOp s $ UnQual s $ Symbol s ">>=")
                      (Lambda s [pat] (inner'))


desugarPat :: Pat l -> Desugar (Pat l)
desugarPat pt = case pt of
  -- (p) => p
  PParen _ p -> desugarPat p

  PVar l n -> return $ PVar l (desugarName n)
  PLit {} -> return pt
  PNeg l p -> PNeg l <$> desugarPat p
  PNPlusK{} -> return pt
  PInfixApp l p1 q p2 -> PInfixApp l <$> desugarPat p1 <*> return (desugarQName q) <*> desugarPat p2
  PApp l q ps -> PApp l (desugarQName q) <$> mapM desugarPat ps
  PTuple l b ps -> PTuple l b <$> mapM desugarPat ps
  PList l ps -> PList l <$> mapM desugarPat ps
  PRec l q pfs -> PRec l (desugarQName q) <$> mapM desugarPatField pfs
  PAsPat l n p -> PAsPat l (desugarName n) <$> desugarPat p
  PWildCard{} -> return pt
  PIrrPat l p -> PIrrPat l <$> desugarPat p
  PatTypeSig l p t -> PatTypeSig l <$> desugarPat p <*> return (desugarType t)
  PViewPat l e p -> PViewPat l <$> desugarExp e <*> desugarPat p
  PBangPat l p -> PBangPat l <$> desugarPat p
  _ -> return pt

desugarPatField :: PatField l -> Desugar (PatField l)
desugarPatField pf = case pf of
  -- {a} => {a=a} for R{a}
  PFieldPun l n -> let dn = desugarName n in desugarPatField $ PFieldPat l (UnQual l dn) (PVar l dn)

  PFieldPat l q p -> PFieldPat l (desugarQName q) <$> desugarPat p
  PFieldWildcard l -> return $ PFieldWildcard l

desugarGuardedAlts :: GuardedAlts l -> Desugar (GuardedAlts l)
desugarGuardedAlts g = case g of
  UnGuardedAlt l e -> UnGuardedAlt l <$> desugarExp e
  GuardedAlts l gas -> GuardedAlts l <$> mapM desugarGuardedAlt gas

desugarQOp :: QOp l -> QOp l
desugarQOp = id

desugarType :: Type l -> Type l
desugarType = id

desugarQName :: QName l -> QName l
desugarQName = id

desugarQualStmt :: QualStmt l -> Desugar (QualStmt l)
desugarQualStmt q = case q of
  QualStmt l s -> QualStmt l <$> desugarStmt s
  ThenTrans l e -> ThenTrans l <$> desugarExp e
  ThenBy l e1 e2 -> ThenBy l <$> desugarExp e1 <*> desugarExp e2
  GroupBy l e -> GroupBy l <$> desugarExp e
  GroupUsing l e -> GroupUsing l <$> desugarExp e
  GroupByUsing l e1 e2 -> GroupByUsing l <$> desugarExp e1 <*> desugarExp e2

desugarAlt :: Alt l -> Desugar (Alt l)
desugarAlt (Alt l p ga mb) = Alt l <$> desugarPat p <*> desugarGuardedAlts ga <*> mmap desugarBinds mb

desugarFieldUpdate :: FieldUpdate l -> Desugar (FieldUpdate l)
desugarFieldUpdate f = case f of
  FieldUpdate l q e -> FieldUpdate l (desugarQName q) <$> desugarExp e
  FieldPun l n -> let dn = UnQual l (desugarName n)
                  in desugarFieldUpdate $ FieldUpdate l dn (Var l dn)
  FieldWildcard{} -> return f

desugarBracket :: Bracket l -> Bracket l
desugarBracket = id

desugarSplice :: Splice l -> Splice l
desugarSplice = id

desugarGuardedAlt :: GuardedAlt l -> Desugar (GuardedAlt l)
desugarGuardedAlt (GuardedAlt l ss e) = GuardedAlt l <$> mapM desugarStmt ss <*> desugarExp e

desugarStmt :: Stmt l -> Desugar (Stmt l)
desugarStmt s = case s of
  Generator l p e -> Generator l <$> desugarPat p <*> desugarExp e
  Qualifier l e -> Qualifier l <$> desugarExp e
  LetStmt l b -> LetStmt l <$> desugarBinds b
  RecStmt l ss -> RecStmt l <$> mapM desugarStmt ss

desugarName :: Name a -> Name a
desugarName = id

desugarVar :: Exp l -> QName l -> Exp l
desugarVar e q = case q of
  Special _ t@TupleCon{} -> fromMaybe e $ desugarTupleCon t
  _ -> e

-- | (,) => \x y -> (x,y)
desugarTupleCon :: SpecialCon l -> Maybe (Exp l)
desugarTupleCon s = case s of
  TupleCon l b n -> Just $ Lambda l params body
    where
      -- It doesn't matter if these variable names shadow anything since
      -- this lambda won't have inner scopes.
      names  = take n $ map (Ident l . ("$gen" ++) . show) [(1::Int)..]
      params = PVar l <$> names
      body   = Tuple l b (Var l . UnQual l <$> names)
  _ -> Nothing
