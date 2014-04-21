{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Fay.Compiler.Desugar
  (desugar
  ,desugar'
  ,desugarExpParen
  ,desugarPatParen
  ) where

import           Fay.Compiler.QName              (unname)
import           Fay.Compiler.Misc               (hasLanguagePragma, ffiExp)
import           Fay.Exts.NoAnnotation           (unAnn)
import           Fay.Types                       (CompileError (..))

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Data                       (Data)
import           Data.Maybe
import           Data.Typeable                   (Typeable)
import           Language.Haskell.Exts.Annotated hiding (binds, loc, name)
import           Prelude                         hiding (exp, mod)
import qualified Data.Generics.Uniplate.Data     as U

-- Types

data DesugarReader l = DesugarReader
  { readerNameDepth     :: Int
  , readerNoInfo        :: l
  , readerTmpNamePrefix :: String
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

runDesugar :: String -> l -> Desugar l a -> IO (Either CompileError a)
runDesugar tmpNamePrefix emptyAnnotation m =
    runErrorT (runReaderT (unDesugar m) (DesugarReader 0 emptyAnnotation tmpNamePrefix))

-- | Generate a temporary, SCOPED name for testing conditions and
-- such. We don't have name tracking yet, so instead we use this.
withScopedTmpName :: (Data l, Typeable l) => l -> (Name l -> Desugar l a) -> Desugar l a
withScopedTmpName l f = do
  prefix <- asks readerTmpNamePrefix
  n <- asks readerNameDepth
  local (\r -> r { readerNameDepth = n + 1 }) $
    f $ tmpName l prefix n

-- | Generates temporary names where the scope doesn't matter.
unscopedTmpNames :: l -> String -> [Name l]
unscopedTmpNames l prefix = map (tmpName l prefix) [0..]

-- | Don't call this directly, use withScopedTmpName or unscopedTmpNames instead.
tmpName :: l -> String -> Int -> Name l
tmpName l prefix n = Ident l $ prefix ++ show n

-- | Top level, desugar a whole module possibly returning errors
desugar :: (Data l, Typeable l) => l -> Module l -> IO (Either CompileError (Module l))
desugar = desugar' "$gen"

-- | Desugar with the option to specify a prefix for generated names.
-- Useful if you want to provide valid haskell name that HSE can print.
desugar' :: (Data l, Typeable l) => String -> l -> Module l -> IO (Either CompileError (Module l))
desugar' prefix emptyAnnotation md = runDesugar prefix emptyAnnotation $
      checkEnum md
  >>  desugarSection md
  >>= desugarListComp
  >>= desugarTupleCon
  >>= return . desugarPatParen
  >>= return . desugarFieldPun
  >>= return . desugarPatFieldPun
  >>= desugarDo
  >>= desugarTupleSection
  >>= desugarImplicitPrelude
  >>= desugarFFITypeSigs
  >>= desugarLCase
  >>= return . desugarMultiIf
  >>= return . desugarInfixOp
  >>= return . desugarInfixPat
  >>= return . desugarExpParen

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
desugarTupleCon :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarTupleCon md = do
  prefix <- asks readerTmpNamePrefix
  return $ flip transformBi md $ \ex -> case ex of
    Var _ (Special _ t@TupleCon{}) -> fromTupleCon prefix ex t
    Con _ (Special _ t@TupleCon{}) -> fromTupleCon prefix ex t
    _ -> ex
  where
    fromTupleCon :: String -> Exp l -> SpecialCon l -> Exp l
    fromTupleCon prefix e s = fromMaybe e $ case s of
      TupleCon l b n -> Just $ Lambda l params body
        where
          -- It doesn't matter if these variable names shadow anything since
          -- this lambda won't have inner scopes.
          names  = take n $ unscopedTmpNames l prefix
          params = PVar l <$> names
          body   = Tuple l b (Var l . UnQual l <$> names)
      _ -> Nothing

-- | \case x of [...] -> \foo -> case foo of [...]
desugarLCase :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarLCase = transformBiM $ \ex -> case ex of
  LCase l alts -> withScopedTmpName l $ \n -> return $ Lambda l [PVar l n] (Case l (Var l (UnQual l n)) alts)
  _ -> return ex

desugarMultiIf :: (Data l, Typeable l) => Module l -> Module l
desugarMultiIf = transformBi $ \ex -> case ex of
  MultiIf l alts -> Case l (Con l (Special l (UnitCon l)))
                           [Alt l (PWildCard l) (GuardedAlts l gas) Nothing]
    where gas = map (\(IfAlt l' p a) -> GuardedAlt l' [Qualifier l' p] a) alts
  _ -> ex

desugarTupleSection :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarTupleSection md = do
  prefix <- asks readerTmpNamePrefix
  flip transformBiM md $ \ex -> case ex of
    TupleSection l _ mes -> do
      (names, lst) <- genSlotNames l mes (unscopedTmpNames l prefix)
      return $ Lambda l (map (PVar l) names) (Tuple l Boxed lst)
    _ -> return ex
  where

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

-- | {a} => {a=a} for R{a}
desugarPatFieldPun :: (Data l, Typeable l) => Module l -> Module l
desugarPatFieldPun = transformBi $ \pf -> case pf of
  PFieldPun l n -> PFieldPat l (UnQual l n) (PVar l n)
  _             -> pf

-- | Desugar list comprehensions.
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
      return $ ImportDecl noInfo (ModuleName noInfo "Prelude") False False Nothing Nothing Nothing

desugarFFITypeSigs :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarFFITypeSigs = desugarToplevelFFITypeSigs >=> desugarBindsTypeSigs

-- | For each toplevel FFI pattern binding, search the module for the relevant
-- type declaration; if found, add a type signature to the ffi expression.
-- e.g.
--  foo :: Int
--  foo = ffi "3"
-- becomes
--  foo :: Int
--  foo = ffi "3" :: Int
desugarToplevelFFITypeSigs :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarToplevelFFITypeSigs m = case m of
  Module a b c d decls -> do
    decls' <- addFFIExpTypeSigs decls
    return $ Module a b c d decls'
  _ -> return m

desugarBindsTypeSigs :: (Data l, Typeable l) => Module l -> Desugar l (Module l)
desugarBindsTypeSigs = transformBiM $ \(BDecls srcInfo decls) -> do
  decls' <- addFFIExpTypeSigs decls
  return $ BDecls srcInfo decls'

addFFIExpTypeSigs :: (Data l, Typeable l) => [Decl l] -> Desugar l [Decl l]
addFFIExpTypeSigs decls = do
  let typeSigs = getTypeSigs decls
  sequence $ go typeSigs decls
  where
  -- | Create a lookup list mapping names to types, for all the types declared
  -- through standalone (ie: not in an expression) type signatures at this
  -- scope level.
  getTypeSigs ds = [ (unname n, typ) | TypeSig _ names typ <- ds, n <- names ]

  go typeSigs ds = map (addTypeSig typeSigs) ds

  addTypeSig typeSigs decl = case decl of
    (PatBind loc pat typ rhs binds) ->
      case getUnguardedRhs rhs of
        Just (srcInfo, rhExp) ->
          if isFFI rhExp
            then do
              rhExp' <- addSigToExp typeSigs decl rhExp
              return $ PatBind loc pat typ (UnGuardedRhs srcInfo rhExp') binds
            else return decl
        _ -> return decl
    _ -> return decl

  getUnguardedRhs rhs = case rhs of
    (UnGuardedRhs srcInfo exp) -> Just (srcInfo, exp)
    _ -> Nothing

  isFFI = isJust . ffiExp

  -- | Adds an explicit type signature to an expression (which is assumed to
  -- be the RHS of a declaration). This should only need to be called for FFI
  -- function declarations.
  -- Arguments:
  --  sigs:  List of toplevel type signatures
  --  decl:  The declaration, which should be a PatBind.
  --  rhExp: Expression comprising the RHS of the declaration
  addSigToExp typeSigs decl rhExp = case getTypeFor typeSigs decl of
    Just typ -> do
      noInfo <- asks readerNoInfo
      return $ ExpTypeSig noInfo rhExp typ
    Nothing -> return rhExp

  getTypeFor typeSigs decl = case decl of
    (PatBind _ (PVar _ name) _ _ _) -> lookup (unname name) typeSigs
    _ -> Nothing

desugarInfixOp :: (Data l, Typeable l) => Module l -> Module l
desugarInfixOp = transformBi $ \ex -> case ex of
  InfixApp l e1 oper e2 -> App l (App l (getOp oper) e1) e2
    where
      getOp (QVarOp l' o) = Var l' o
      getOp (QConOp l' o) = Con l' o
  _ -> ex

desugarInfixPat :: (Data l, Typeable l) => Module l -> Module l
desugarInfixPat = transformBi $ \pt -> case pt of
  PInfixApp l p1 iop p2 -> PApp l iop [p1, p2]
  _ -> pt

desugarExpParen :: (Data l, Typeable l) => Module l -> Module l
desugarExpParen = transformBi $ \ex -> case ex of
  Paren _ e -> e
  _ -> ex

transformBi :: U.Biplate (from a) (to a) => (to a -> to a) -> from a -> from a
transformBi = U.transformBi

universeBi :: U.Biplate (from a) (to a) => from a -> [to a]
universeBi = U.universeBi

transformBiM :: (Monad m, U.Biplate (from a) (to a)) => (to a -> m (to a)) -> from a -> m (from a)
transformBiM = U.transformBiM
