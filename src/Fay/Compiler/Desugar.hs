{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Fay.Compiler.Desugar
  (desugar
  ) where

import           Fay.Exts.NoAnnotation           (unAnn)
import           Fay.Types                       (CompileError (..))
import           Fay.Compiler.Misc               (hasLanguagePragma)

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Data                       (Data)
import qualified Data.Generics.Uniplate.Data     as U
import           Data.Maybe
import           Data.Typeable                   (Typeable)
import           Language.Haskell.Exts.Annotated hiding (binds, loc)
import           Prelude                         hiding (exp)

-- Types

data DesugarReader l = DesugarReader { readerNameDepth :: Int
                                     , readerNoInfo :: l
                                     }

newtype Desugar l a = Desugar
  { unDesugar :: (ReaderT (DesugarReader l)
                       (ErrorT CompileError IO))
                       a
  } deriving ( MonadReader (DesugarReader l)
             , MonadError CompileError
             , MonadIO
             , Monad
             , Functor
             , Applicative
             )

runDesugar :: l -> Desugar l a -> IO (Either CompileError a)
runDesugar emptyAnnotation m =
    runErrorT (runReaderT (unDesugar m) (DesugarReader 0 emptyAnnotation))

-- | Generate a temporary, SCOPED name for testing conditions and
-- such. We don't have name tracking yet, so instead we use this.
withScopedTmpName :: (Data l, Typeable l) => l -> (Name l -> Desugar l a) -> Desugar l a
withScopedTmpName l f = do
  n <- asks readerNameDepth
  local (\r -> r { readerNameDepth = n + 1 }) $
   f $ Ident l $ "$gen" ++ show n



-- | Top level, desugar a whole module possibly returning errors
desugar :: (Data l, Typeable l) => l -> Module l -> IO (Either CompileError (Module l))
desugar emptyAnnotation md = runDesugar emptyAnnotation $
      checkEnum md
  >>  desugarSection md
  >>= desugarListComp
  >>= return . desugarTupleCon
  >>= return . desugarPatParen
  >>= return . desugarFieldPun
  >>= return . desugarPatFieldPun
  >>= desugarDo
  >>= desugarTupleSection
  >>= desugarImplicitPrelude

-- | Desugaring

desugarSection :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarSection = transformBiM $ \ex -> case ex of
  LeftSection  l e q -> withScopedTmpName l $ \tmp ->
      return $ Lambda l [PVar l tmp] (InfixApp l e q (Var l (UnQual l tmp)))
  RightSection l q e -> withScopedTmpName l $ \tmp ->
      return $ Lambda l [PVar l tmp] (InfixApp l (Var l (UnQual l tmp)) q e)
  _ -> return ex

desugarDo :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarDo = transformBiM $ \ex -> case ex of
  Do _ stmts -> maybe (throwError EmptyDoBlock) return $ foldl desugarStmt' Nothing (reverse stmts)
  _ -> return ex

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
desugarTupleCon :: (Data l, Typeable l) => Module l -> Module l
desugarTupleCon = transformBi $ \ex -> case ex of
  Var _ (Special _ t@TupleCon{}) -> fromTupleCon ex t
  Con _ (Special _ t@TupleCon{}) -> fromTupleCon ex t
  _ -> ex
  where
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

desugarTupleSection :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarTupleSection = transformBiM $ \ex -> case ex of
  TupleSection l _ mes -> do
    (names, lst) <- genSlotNames l mes (varNames l)
    return $ Lambda l (map (PVar l) names) (Tuple l Unboxed lst)
  _ -> return ex
  where
    varNames :: l -> [Name l]
    varNames l = map (\i -> Ident l ("$gen_" ++ show i)) [0::Int ..]

    genSlotNames :: l -> [Maybe (Exp l)] -> [Name l] -> Desugar l ([Name l], [Exp l])
    genSlotNames _ [] _ = return ([], [])
    genSlotNames l (Nothing : rest) ns = do
      -- it's safe to use head/tail here because ns is an infinite list
      (rn, re) <- genSlotNames l rest (tail ns)
      return (head ns : rn, Var l (UnQual l (head ns)) : re)
    genSlotNames l (Just e : rest) ns = do
      (rn, re) <- genSlotNames l rest ns
      return (rn, e : re)

-- (p) => p for patterns
desugarPatParen :: (Data l, Typeable l) => Module l -> Module l
desugarPatParen = transformBi $ \pt -> case pt of
  PParen _ p -> p
  _ -> pt

desugarFieldPun :: (Data l, Typeable l) => Module l -> Module l
desugarFieldPun = transformBi $ \f -> case f of
  FieldPun l n -> let dn = UnQual l n in FieldUpdate l dn (Var l dn)
  _ -> f

desugarPatFieldPun :: (Data l, Typeable l) => Module l -> Module l
desugarPatFieldPun = transformBi $ \pf -> case pf of
  -- {a} => {a=a} for R{a}
  PFieldPun l n -> PFieldPat l (UnQual l n) (PVar l n)
  _             -> pf

-- | Desugar l list comprehensions.
desugarListComp :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarListComp = transformBiM $ \ex -> case ex of
    ListComp l exp stmts -> desugarListComp' l exp stmts
    _ -> return ex
  where
    desugarListComp' l e [] = return (List l [ e ])
    desugarListComp' l e (QualStmt _ (Generator _ p e2) : stmts) = do
      nested <- desugarListComp' l e stmts
      withScopedTmpName l $ \f ->
        return (Let l (BDecls l [ FunBind l [
            Match l f [ p           ] (UnGuardedRhs l nested) Nothing
          , Match l f [ PWildCard l ] (UnGuardedRhs l (List l [])) Nothing
          ]]) (App l (App l (Var l (Qual l (ModuleName l "$Prelude") (Ident l "concatMap"))) (Var l (UnQual l f))) e2))
    desugarListComp' l e (QualStmt _ (Qualifier _ e2) : stmts) = do
      nested <- desugarListComp' l e stmts
      return (If l e2 nested (List l []))
    desugarListComp' l e (QualStmt _ (LetStmt _ bs) : stmts) = do
      nested <- desugarListComp' l e stmts
      return (Let l bs nested)
    desugarListComp' _ _ (_ : _) =
      error "UnsupportedListComprehension"

-- | We only have Enum instance for Int, but GHC hard codes [x..y]
-- syntax to GHC.Base.Enum instead of using our Enum class so we check
-- for obviously incorrect usages and throw an error on them. This can
-- only checks literals, but it helps a bit.
checkEnum :: (Data l, Typeable l) => Module l -> Desugar l ()
checkEnum = mapM_ f . universeBi
  where
    f ex = case ex of
      e@(EnumFrom       _ e1)       -> checkIntOrUnknown e [e1]
      e@(EnumFromTo     _ e1 e2)    -> checkIntOrUnknown e [e1,e2]
      e@(EnumFromThen   _ e1 e2)    -> checkIntOrUnknown e [e1,e2]
      e@(EnumFromThenTo _ e1 e2 e3) -> checkIntOrUnknown e [e1,e2,e3]
      _ -> return ()

    checkIntOrUnknown :: Exp l -> [Exp l] -> Desugar l ()
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

desugarImplicitPrelude :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarImplicitPrelude m =
    if preludeNotNeeded
        then return m
        else addPrelude m
  where
    preludeNotNeeded = hasExplicitPrelude m ||
                       hasLanguagePragma "NoImplicitPrelude" (getPragmas m)

    getPragmas :: (Data l, Typeable l) => Module l -> [ModulePragma l]
    getPragmas = universeBi

    getImportDecls :: Module l -> [ImportDecl l]
    getImportDecls (Module _ _ _ decls _) = decls
    getImportDecls _ = []

    setImportDecls :: [ImportDecl l] -> Module l -> Module l
    setImportDecls decls (Module a b c _ d) = Module a b c decls d
    setImportDecls _ mod = mod

    hasExplicitPrelude :: Module l -> Bool
    hasExplicitPrelude = any isPrelude . getImportDecls

    isPrelude :: ImportDecl l -> Bool
    isPrelude decl = case importModule decl of
                      ModuleName _ name -> name == "Prelude"

    addPrelude :: Module l -> Desugar l (Module l)
    addPrelude mod = do
        let decls = getImportDecls mod
        prelude <- getPrelude
        return $ setImportDecls (prelude : decls) mod

    getPrelude :: Desugar l (ImportDecl l)
    getPrelude = do
        noInfo <- asks readerNoInfo
        return $
            ImportDecl noInfo (ModuleName noInfo "Prelude")
                False False Nothing Nothing Nothing

transformBi :: U.Biplate (from a) (to a) => (to a -> to a) -> from a -> from a
transformBi = U.transformBi

universeBi :: U.Biplate (from a) (to a) => from a -> [to a]
universeBi = U.universeBi

transformBiM :: (Monad m, U.Biplate (from a) (to a)) => (to a -> m (to a)) -> from a -> m (from a)
transformBiM = U.transformBiM
