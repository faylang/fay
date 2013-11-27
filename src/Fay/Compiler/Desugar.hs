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
      checkEnum md
  >>  desugarSection md
  >>= return . desugarTupleCon
  >>= return . desugarPatParen
  >>= return . desugarFieldPun
  >>= return . desugarPatFieldPun
  >>= desugarDo
  >>= desugarTupleSection

-- | Desugaring

desugarSection :: forall l. (Data l, Typeable l) => Module l -> Desugar (Module l)
desugarSection = t $ \ex -> case ex of
  LeftSection  l e q -> withScopedTmpName l $ \tmp ->
      return $ Lambda l [PVar l tmp] (InfixApp l e q (Var l (UnQual l tmp)))
  RightSection l q e -> withScopedTmpName l $ \tmp ->
      return $ Lambda l [PVar l tmp] (InfixApp l (Var l (UnQual l tmp)) q e)
  _ -> return ex
  where
    t :: (Data l, Typeable l) => (Exp l -> Desugar (Exp l)) -> Module l -> Desugar (Module l)
    t = transformBiM

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
  _ -> ex
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

desugarTupleSection :: forall l. (Data l, Typeable l) => Module l -> Desugar (Module l)
desugarTupleSection = t $ \ex -> case ex of
  TupleSection l _ mes -> do
    (names, lst) <- genSlotNames l mes (varNames l)
    return $ Lambda l (map (PVar l) names) (Tuple l Unboxed lst)
  _ -> return ex
  where
    t :: (Data l, Typeable l) => (Exp l -> Desugar (Exp l)) -> Module l -> Desugar (Module l)
    t = transformBiM

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
      return (rn, e : re)

-- (p) => p for patterns
desugarPatParen :: forall l. (Data l, Typeable l) => Module l -> Module l
desugarPatParen = t $ \pt -> case pt of
  PParen _ p -> p
  _ -> pt
  where
    t :: (Data l, Typeable l) => (Pat l -> Pat l) -> Module l -> Module l
    t = transformBi

desugarFieldPun :: forall l. (Data l, Typeable l) => Module l -> Module l
desugarFieldPun = t $ \f -> case f of
  FieldPun l n -> let dn = UnQual l n in FieldUpdate l dn (Var l dn)
  _ -> f
  where
    t :: (Data l, Typeable l) => (FieldUpdate l -> FieldUpdate l) -> Module l -> Module l
    t = transformBi

desugarPatFieldPun :: forall l. (Data l, Typeable l) => Module l -> Module l
desugarPatFieldPun = t $ \pf -> case pf of
  -- {a} => {a=a} for R{a}
  PFieldPun l n -> PFieldPat l (UnQual l n) (PVar l n)
  _             -> pf
  where
    t :: (Data l, Typeable l) => (PatField l -> PatField l) -> Module l -> Module l
    t = transformBi

-- | We only have Enum instance for Int, but GHC hard codes [x..y]
-- syntax to GHC.Base.Enum instead of using our Enum class so we check
-- for obviously incorrect usages and throw an error on them. This can
-- only checks literals, but it helps a bit.
checkEnum :: forall l . (Data l, Typeable l) => Module l -> Desugar ()
checkEnum = mapM_ f . t
  where
    t :: (Data l, Typeable l) => Module l -> [Exp l]
    t = universeBi

    f = (\ex -> case ex of
      e@(EnumFrom       _ e1)       -> checkIntOrUnknown e [e1]
      e@(EnumFromTo     _ e1 e2)    -> checkIntOrUnknown e [e1,e2]
      e@(EnumFromThen   _ e1 e2)    -> checkIntOrUnknown e [e1,e2]
      e@(EnumFromThenTo _ e1 e2 e3) -> checkIntOrUnknown e [e1,e2,e3]
      _ -> return ())

    checkIntOrUnknown :: Exp l -> [Exp l] -> Desugar ()
    checkIntOrUnknown exp es = when (not $ any isIntOrUnknown es) (throwError . UnsupportedEnum $ unAnn exp)
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
