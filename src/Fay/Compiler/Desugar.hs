{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Fay.Compiler.Desugar
  (desugar
  ) where

import           Fay.Exts.NoAnnotation           (unAnn)
import           Fay.Types                       (CompileError (..))

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Data                       (Data)
import           Data.Generics.Uniplate.Data
import           Data.Maybe
import           Data.Typeable                   (Typeable)
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
withScopedTmpName :: (Data l, Typeable l) => l -> (Name l -> Desugar a) -> Desugar a
withScopedTmpName l f = do
  n <- asks readerNameDepth
  local (\r -> DesugarReader $ readerNameDepth r + 1) $
   f $ Ident l $ "$gen" ++ show n



-- | Top level, desugar a whole module possibly returning errors
desugar :: (Data l, Typeable l) => Module l -> IO (Either CompileError (Module l))
desugar md = runDesugar $
      desugarSection md
  >>= return . desugarPatField
  >>= desugarDo
  >>= return . desugarTupleCon
  >>= desugarModule

-- | Desugaring

desugarSection :: forall l. (Data l, Typeable l) => Module l -> Desugar (Module l)
desugarSection = t $ \ex -> case ex of
  LeftSection  l e q -> withScopedTmpName l $ \tmp ->
      return $ Lambda l [PVar l tmp] (InfixApp l e q (Var l (UnQual l tmp)))
  RightSection l q e -> withScopedTmpName l $ \tmp ->
      return $ Lambda l [PVar l tmp] (InfixApp l (Var l (UnQual l tmp)) q e)
  _                  -> return $ ex
  where
    t :: (Data l, Typeable l) => (Exp l -> Desugar (Exp l)) -> Module l -> Desugar (Module l)
    t = transformBiM

desugarPatField :: forall l. (Data l, Typeable l) => Module l -> Module l
desugarPatField = t $ \pf -> case pf of
  -- {a} => {a=a} for R{a}
  PFieldPun l n -> PFieldPat l (UnQual l n) (PVar l n)
  _             -> pf
  where
    t :: (Data l, Typeable l) => (PatField l -> PatField l) -> Module l -> Module l
    t = transformBi


--  Do _ stmts -> maybe (throwError EmptyDoBlock) return =<< (mmap desugarExp $ foldl desugarStmt' Nothing (reverse stmts))
desugarDo :: forall l. (Data l, Typeable l) => Module l -> Desugar (Module l)
desugarDo = t $ \ex -> case ex of
  Do _ stmts -> maybe (throwError EmptyDoBlock) return $ foldl desugarStmt' Nothing (reverse stmts)
  _ -> return ex
  where
    t :: (Data l, Typeable l) => (Exp l -> Desugar (Exp l)) -> Module l -> Desugar (Module l)
    t = transformBiM

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

-- | (,)  => \x y   -> (x,y)
--   (,,) => \x y z -> (x,y,z)
-- etc
desugarTupleCon :: forall l. (Data l, Typeable l) => Module l -> Module l
desugarTupleCon = t $ \ex -> case ex of
  Var _ (Special _ t@TupleCon{}) -> fromTupleCon ex t
  Con _ (Special _ t@TupleCon{}) -> fromTupleCon ex t
  _       -> ex
  where
    t :: (Data l, Typeable l) => (Exp l -> Exp l) -> Module l -> Module l
    t = transformBi

    fromTupleCon :: Exp l -> SpecialCon l -> Exp l
    fromTupleCon e s = fromMaybe e $ case s of
      TupleCon l b n -> Just $ Lambda l params body
        where
          -- It doesn't matter if these variable names shadow anything since
          -- this lambda won't have inner scopes.
          names  = take n $ map (Ident l . ("$gen" ++) . show) [(1::Int)..]
          params = PVar l <$> names
          body   = Tuple l b (Var l . UnQual l <$> names)
      _ -> Nothing



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
  Match l n ps rhs mb -> Match l n <$> mapM desugarPat ps <*> desugarRhs rhs <*> mmap desugarBinds mb
  InfixMatch l p n ps r mb -> InfixMatch l <$> desugarPat p <*> return n <*> mapM desugarPat ps <*> desugarRhs r <*> mmap desugarBinds mb

desugarRhs :: Rhs l -> Desugar (Rhs l)
desugarRhs r = case r of
  UnGuardedRhs l e -> UnGuardedRhs l <$> desugarExp e
  GuardedRhss l gs -> GuardedRhss l <$> mapM desugarGuardedRhs gs

desugarGuardedRhs :: GuardedRhs l -> Desugar (GuardedRhs l)
desugarGuardedRhs g = case g of
  GuardedRhs l stmts exp -> GuardedRhs l <$> mapM desugarStmt stmts <*> desugarExp exp

desugarExp :: Exp l -> Desugar (Exp l)
desugarExp ex = case ex of
  IPVar{} -> return ex
  Lit{} -> return ex
  InfixApp l e1 qop e2 -> InfixApp l <$> desugarExp e1 <*> return qop <*> desugarExp e2
  App l e1 e2 -> App l <$> desugarExp e1 <*> desugarExp e2
  NegApp l e -> NegApp l <$> desugarExp e
  Lambda l ps e -> Lambda l <$> mapM desugarPat ps <*> desugarExp e
  Let l b e -> Let l <$> desugarBinds b <*> desugarExp e
  If l e1 e2 e3 -> If l <$> desugarExp e1 <*> desugarExp e2 <*> desugarExp e3
  Case l e as -> Case l <$> desugarExp e <*> mapM desugarAlt as
  MDo l ss -> MDo l <$> mapM desugarStmt ss
  Tuple l b es -> Tuple l b <$> mapM desugarExp es
  TupleSection l _ mes -> desugarTupleSec l =<< (mapM (mmap desugarExp) mes)
  List l es -> List l <$> mapM desugarExp es
  Paren l e -> Paren l <$> desugarExp e
  RecConstr l q f -> RecConstr l q <$> mapM desugarFieldUpdate f
  RecUpdate l e f -> RecUpdate l <$> desugarExp e <*> mapM desugarFieldUpdate f
  e@(EnumFrom l e1) -> checkEnum e >> (EnumFrom l <$> desugarExp e1)
  e@(EnumFromTo l e1 e2) -> checkEnum e >> (EnumFromTo l <$> desugarExp e1 <*> desugarExp e2)
  e@(EnumFromThen l e1 e2) -> checkEnum e >> (EnumFromThen l <$> desugarExp e1 <*> desugarExp e2)
  e@(EnumFromThenTo l e1 e2 e3) -> checkEnum e >> (EnumFromThenTo l <$> desugarExp e1 <*> desugarExp e2 <*> desugarExp e3)
  ListComp l e qs -> ListComp l <$> desugarExp e <*> mapM desugarQualStmt qs
  ParComp l e qqs -> ParComp l <$> desugarExp e <*> mapM (mapM desugarQualStmt) qqs
  ExpTypeSig l e t -> ExpTypeSig l <$> desugarExp e <*> return t
  VarQuote l q -> return $ VarQuote l q
  TypQuote l q -> return $ TypQuote l q
  BracketExp l b -> return $ BracketExp l b
  SpliceExp l s -> return $ SpliceExp l s
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
  _ -> return ex

desugarPat :: Pat l -> Desugar (Pat l)
desugarPat pt = case pt of
  -- (p) => p
  PParen _ p -> desugarPat p

  PVar l n -> return $ PVar l n
  PLit {} -> return pt
  PNeg l p -> PNeg l <$> desugarPat p
  PNPlusK{} -> return pt
  PInfixApp l p1 q p2 -> PInfixApp l <$> desugarPat p1 <*> return q <*> desugarPat p2
  PApp l q ps -> PApp l q <$> mapM desugarPat ps
  PTuple l b ps -> PTuple l b <$> mapM desugarPat ps
  PList l ps -> PList l <$> mapM desugarPat ps
  PRec l q pfs -> return $ PRec l q pfs
  PAsPat l n p -> PAsPat l n <$> desugarPat p
  PWildCard{} -> return pt
  PIrrPat l p -> PIrrPat l <$> desugarPat p
  PatTypeSig l p t -> PatTypeSig l <$> desugarPat p <*> return t
  PViewPat l e p -> PViewPat l <$> desugarExp e <*> desugarPat p
  PBangPat l p -> PBangPat l <$> desugarPat p
  _ -> return pt

desugarGuardedAlts :: GuardedAlts l -> Desugar (GuardedAlts l)
desugarGuardedAlts g = case g of
  UnGuardedAlt l e -> UnGuardedAlt l <$> desugarExp e
  GuardedAlts l gas -> GuardedAlts l <$> mapM desugarGuardedAlt gas


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
  FieldUpdate l q e -> FieldUpdate l q <$> desugarExp e
  FieldPun l n -> let dn = UnQual l n
                  in desugarFieldUpdate $ FieldUpdate l dn (Var l dn)
  FieldWildcard{} -> return f

desugarGuardedAlt :: GuardedAlt l -> Desugar (GuardedAlt l)
desugarGuardedAlt (GuardedAlt l ss e) = GuardedAlt l <$> mapM desugarStmt ss <*> desugarExp e

desugarStmt :: Stmt l -> Desugar (Stmt l)
desugarStmt s = case s of
  Generator l p e -> Generator l <$> desugarPat p <*> desugarExp e
  Qualifier l e -> Qualifier l <$> desugarExp e
  LetStmt l b -> LetStmt l <$> desugarBinds b
  RecStmt l ss -> RecStmt l <$> mapM desugarStmt ss


desugarTupleSec :: l -> [Maybe (Exp l)] -> Desugar (Exp l)
desugarTupleSec l xs = do
    (names, lst) <- genSlotNames l xs (varNames l)
    return $ Lambda l (map (PVar l) names) (Tuple l Unboxed lst)
  where
    varNames :: l -> [Name l]
    varNames l = map (\i -> Ident l ("$gen_" ++ show i)) [0::Int ..]

    genSlotNames :: l -> [Maybe (Exp l)] -> [Name l] -> Desugar ([Name l], [Exp l])
    genSlotNames _ [] _ = return ([], [])
    genSlotNames l (Nothing : rest) ns = do
      -- it's safe to use head/tail here because ns is an infinite list
      (rn, re) <- genSlotNames l rest (tail ns)
      return (head ns : rn, Var l (UnQual l (head ns)) : re)
    genSlotNames l (Just e : rest) ns = do
      (rn, re) <- genSlotNames l rest ns
      e' <- desugarExp e
      return (rn, e' : re)

-- | We only have Enum instance for Int, but GHC hard codes [x..y]
-- syntax to GHC.Base.Enum instead of using our Enum class so we check
-- for obviously incorrect usages and throw an error on them. This can
-- only checks literals, but it helps a bit.
checkEnum :: Exp l -> Desugar ()
checkEnum exp = case exp of
  EnumFrom       _ e        -> checkIntOrUnknown [e]
  EnumFromTo     _ e1 e2    -> checkIntOrUnknown [e1,e2]
  EnumFromThen   _ e1 e2    -> checkIntOrUnknown [e1,e2]
  EnumFromThenTo _ e1 e2 e3 -> checkIntOrUnknown [e1,e2,e3]
  _ -> error "checkEnum: Only for Enums"
  where
    checkIntOrUnknown :: [Exp l] -> Desugar ()
    checkIntOrUnknown es = if any isIntOrUnknown es
      then return ()
      else throwError . UnsupportedEnum $ unAnn exp
    isIntOrUnknown :: Exp l -> Bool
    isIntOrUnknown e = case e of
      Con            {} -> False
      Lit _ Int{}       -> True
      Lit            {} -> False
      Tuple          {} -> False
      List           {} -> False
      EnumFrom       {} -> False
      EnumFromTo     {} -> False
      EnumFromThen   {} -> False
      EnumFromThenTo {} -> False
      _                 -> True
