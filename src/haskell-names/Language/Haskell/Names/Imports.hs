{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies  #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-orphans #-} -- ModName (ModuleName l)
module Language.Haskell.Names.Imports (processImports) where

import           Fay.Compiler.ModuleT
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import           Language.Haskell.Names.ScopeUtils
import           Language.Haskell.Names.SyntaxUtils
import           Language.Haskell.Names.Types

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Writer
import           Data.Either
import           Data.Foldable                            (fold, foldMap)
import           Data.Lens.Light
import qualified Data.Map                                 as Map
import           Data.Maybe
import qualified Data.Set                                 as Set
import           Language.Haskell.Exts.Annotated

instance ModName (ModuleName l) where
  modToString (ModuleName _ s) = s

preludeName :: String
preludeName = "Prelude"

processImports
  :: (MonadModule m, ModuleInfo m ~ Symbols)
  => ExtensionSet
  -> [ImportDecl l]
  -> m ([ImportDecl (Scoped l)], Global.Table)
processImports exts importDecls = do

  (annotated, tbl) <- runWriterT $ mapM (WriterT . processImport) importDecls

  let
    isPreludeImported = not . null $
      [ () | ImportDecl { importModule = ModuleName _ modName } <- importDecls
           , modName == preludeName ]

    importPrelude =
      ImplicitPrelude `Set.member` exts &&
      not isPreludeImported

  tbl' <-
    if not importPrelude
      then return tbl
      else do
        -- FIXME currently we don't have a way to signal an error when
        -- Prelude cannot be found
        syms <- fold `liftM` getModuleInfo preludeName
        return $ tbl <>
          computeSymbolTable
            False -- not qualified
            (ModuleName () preludeName)
            syms

  return (annotated, tbl')

processImport
  :: (MonadModule m, ModuleInfo m ~ Symbols)
  => ImportDecl l
  -> m (ImportDecl (Scoped l), Global.Table)
processImport imp = do
  mbi <- getModuleInfo (importModule imp)
  case mbi of
    Nothing ->
      let e = EModNotFound (importModule imp)
      in return (scopeError e imp, Global.empty)
    Just syms -> return $ resolveImportDecl syms imp

resolveImportDecl
  :: Symbols
  -> ImportDecl l
  -> (ImportDecl (Scoped l), Global.Table)
resolveImportDecl syms (ImportDecl l mod qual src impSafe pkg mbAs mbSpecList) =
  let
    (mbSpecList', impSyms) =
      (fmap fst &&& maybe syms snd) $
        resolveImportSpecList mod syms <$> mbSpecList
    tbl = computeSymbolTable qual (fromMaybe mod mbAs) impSyms
    info =
      case mbSpecList' of
        Just sl | Scoped (ScopeError e) _ <- ann sl ->
          ScopeError e
        _ -> Import tbl
  in
    (ImportDecl
      (Scoped info l)
      (Scoped (ImportPart syms) <$> mod)
      qual
      src
      impSafe
      pkg
      (fmap noScope mbAs)
      mbSpecList'
    , tbl)

resolveImportSpecList
  :: ModuleName l
  -> Symbols
  -> ImportSpecList l
  -> (ImportSpecList (Scoped l), Symbols)
resolveImportSpecList mod allSyms (ImportSpecList l isHiding specs) =
  let specs' = map (resolveImportSpec mod isHiding allSyms) specs
      mentionedSyms = mconcat $ rights $ map ann2syms specs'
      importedSyms = computeImportedSymbols isHiding allSyms mentionedSyms
      newAnn = Scoped (ImportPart importedSyms) l
  in
    (ImportSpecList newAnn isHiding specs', importedSyms)

-- | This function takes care of the possible 'hiding' clause
computeImportedSymbols
  :: Bool
  -> Symbols -- ^ all symbols
  -> Symbols -- ^ mentioned symbols
  -> Symbols -- ^ imported symbols
computeImportedSymbols isHiding (Symbols vs ts) mentionedSyms =
  case isHiding of
    False -> mentionedSyms
    True ->
      let
        Symbols hvs hts = mentionedSyms
        allTys = symbolMap st_origName ts
        hidTys = symbolMap st_origName hts
        allVls = symbolMap sv_origName vs
        hidVls = symbolMap sv_origName hvs
      in
        Symbols
          (Set.fromList $ Map.elems $ allVls Map.\\ hidVls)
          (Set.fromList $ Map.elems $ allTys Map.\\ hidTys)

symbolMap
  :: Ord s
  => (a -> s)
  -> Set.Set a
  -> Map.Map s a
symbolMap f is = Map.fromList [(f i, i) | i <- Set.toList is]

resolveImportSpec
  :: ModuleName l
  -> Bool
  -> Symbols
  -> ImportSpec l
  -> ImportSpec (Scoped l)
-- NB: this can be made more efficient
resolveImportSpec mod isHiding syms spec =
  case spec of
    IVar _ (NoNamespace {}) n ->
      let
        matches = mconcat $
          -- Strictly speaking, the isConstructor check is unnecessary
          -- because constructors are lexically different from anything
          -- else.
          [ mkVal info
          | info <- vs
          , not (isConstructor info)
          , sv_origName info ~~ n]
      in
        checkUnique
          (ENotExported Nothing n mod)
          matches
          spec
    -- FIXME think about data families etc.
    IVar _ (TypeNamespace {}) _ -> error "'type' namespace is not supported yet" -- FIXME
    IAbs _ n
      | isHiding ->
          -- This is a bit special. 'C' may match both types/classes and
          -- data constructors.
          -- FIXME Still check for uniqueness?
          let
            Symbols vlMatches tyMatches =
              mconcat [ mkVal info | info <- vs, sv_origName info ~~ n]
              <>
              mconcat [ mkTy info | info <- ts, st_origName info ~~ n]
          in
            if Set.null tyMatches && Set.null vlMatches
              then
                scopeError (ENotExported Nothing n mod) spec
              else
                Scoped (ImportPart (Symbols vlMatches tyMatches)) <$> spec
      | otherwise ->
          let
            matches = mconcat
              [mkTy info | info <- ts, st_origName info ~~ n]
          in
            checkUnique
              (ENotExported Nothing n mod)
              matches
              spec

    -- FIXME
    -- What about things like:
    -- head(..)
    -- String(..)
    -- ?
    IThingAll l n ->
      let
        matches = [ info | info <- ts, st_origName info ~~ n]
        subs = mconcat
          [ mkVal info
          | n <- matches
          , info <- vs
          , Just n' <- return $ sv_parent info
          , n' == st_origName n ]
        n' =
          checkUnique
            (ENotExported Nothing n mod)
            (foldMap mkTy matches)
            n
        in
          case ann n' of
            e@(Scoped ScopeError{} _) -> IThingAll e n'
            _ ->
              IThingAll
                (Scoped
                  (ImportPart (subs <> foldMap mkTy matches))
                  l
                )
                n'

    IThingWith l n cns ->
      let
        matches = [info | info <- ts, st_origName info ~~ n]
        n' =
          checkUnique
            (ENotExported Nothing n mod)
            (foldMap mkTy matches)
            n
        typeName = st_origName $ head matches -- should be safe
        (cns', cnSyms) =
          resolveCNames
            syms
            typeName
            (\cn -> ENotExported (Just n) (unCName cn) mod)
            cns
      in
        IThingWith
          (Scoped
            (ImportPart (cnSyms <> foldMap mkTy matches))
            l
          )
          n'
          cns'
  where
    (~~) :: OrigName -> Name l -> Bool
    OrigName { origGName = GName { gName = n } } ~~ n' = n == nameToString n'

    isConstructor :: SymValueInfo n -> Bool
    isConstructor SymConstructor {} = True
    isConstructor _ = False

    vs = Set.toList $ syms^.valSyms
    ts = Set.toList $ syms^.tySyms

ann2syms :: Annotated a => a (Scoped l) -> Either (Error l) (Symbols)
ann2syms a =
  case ann a of
    Scoped (ScopeError e) _ -> Left e
    Scoped (ImportPart syms) _ -> Right syms
    _ -> Left $ EInternal "ann2syms"

checkUnique
  :: Functor f =>
  Error l ->
  Symbols ->
  f l ->
  f (Scoped l)
checkUnique notFound syms@(Symbols vs ts) f =
  case Set.size vs + Set.size ts of
    0 -> scopeError notFound f
    1 -> Scoped (ImportPart syms) <$> f
    -- there should be no clashes, and it should be checked elsewhere
    _ -> scopeError (EInternal "ambiguous import") f
