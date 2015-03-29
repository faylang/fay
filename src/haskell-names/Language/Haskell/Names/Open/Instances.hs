{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- MonoLocalBinds extension prevents premature generalization, which
-- results in the "default" instance being picked.
{-# LANGUAGE MonoLocalBinds        #-}
module Language.Haskell.Names.Open.Instances () where

import           Language.Haskell.Names.GetBound
import           Language.Haskell.Names.Open.Base
import           Language.Haskell.Names.Open.Derived    ()
import           Language.Haskell.Names.RecordWildcards
import           Language.Haskell.Names.Types

import           Control.Applicative
import qualified Data.Data                              as D
import           Data.Lens.Light
import           Data.List
import qualified Data.Traversable                       as T
import           Data.Typeable
import           Language.Haskell.Exts.Annotated

c :: Applicative w => c -> w c
c = pure

(<|)
  :: (Applicative w, Resolvable b, ?alg :: Alg w)
  => w (b -> c) -> (b, Scope) -> w c
(<|) k (b, sc) = k <*> alg b sc
infixl 4 <|

(-:) :: Scope -> a -> (a, Scope)
sc -: b = (b, sc)
infix 5 -:

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (Decl l) where
  rtraverse e sc =
    case e of
      -- N.B. We do not add pat to the local scope.
      --
      -- If this is a top-level binding, then we shouldn't do so, lest
      -- global values are marked as local.
      -- (see https://github.com/haskell-suite/haskell-names/issues/35)
      --
      -- If this is a local binding, then we have already introduced these
      -- variables when processing the enclosing Binds.
      PatBind l pat rhs mbWhere ->
        let
          scWithWhere = intro mbWhere sc
        in
        c PatBind
          <| sc                -: l
          <| sc                -: pat
          <| exprV scWithWhere -: rhs
          <| sc                -: mbWhere
      -- FunBind consists of Matches, which we handle below anyway.
      TypeSig l names ty ->
        c TypeSig
          <| sc       -: l
          <| exprV sc -: names
          <| sc       -: ty
      _ -> defaultRtraverse e sc

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (Type l) where
  rtraverse e sc = defaultRtraverse e (exprT sc)

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (DeclHead l) where
  rtraverse e sc =
    case e of
      DHead l name ->
        c DHead
          <| sc -: l
          <| binderT sc -: name
      DHInfix l v1 name ->
        c DHInfix
          <| sc -: l
          <| sc -: v1
          <| binderT sc -: name
      _ -> defaultRtraverse e sc

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (ConDecl l) where
  rtraverse e sc =
    case e of
      ConDecl l name tys ->
        c ConDecl
          <| sc -: l
          <| binderV sc -: name
          <| sc -: tys
      InfixConDecl l t1 name t2 ->
        c InfixConDecl
          <| sc -: l
          <| sc -: t1
          <| binderV sc -: name
          <| sc -: t2
      RecDecl l name fields ->
        c RecDecl
          <| sc -: l
          <| binderV sc -: name
          <| sc -: fields


instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (FieldDecl l) where
  rtraverse e sc =
    case e of
      FieldDecl l name tys ->
        c FieldDecl
          <| sc -: l
          <| binderV sc -: name
          <| sc -: tys

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (Pat l) where
  rtraverse e sc =
    case e of
      PVar l name ->
        c PVar
          <| sc         -: l
          <| binderV sc -: name
      PNPlusK l name i ->
        c PNPlusK
          <| sc         -: l
          <| binderV sc -: name
          <| sc         -: i
      PInfixApp l pat1 name pat2 ->
        c PInfixApp
          <| sc       -: l
          <| sc       -: pat1
          <| exprV sc -: name
          <| sc       -: pat2
      PApp l qn pat ->
        c PApp
          <| sc       -: l
          <| exprV sc -: qn
          <| sc       -: pat
      PRec l qn pfs ->
        let
          scWc =
            setWcNames (patWcNames (sc ^. gTable) qn pfs) sc
        in
        c PRec
          <| sc       -: l
          <| exprV sc -: qn
          <| scWc     -: pfs
      PAsPat l n pat ->
        c PAsPat
          <| sc         -: l
          <| binderV sc -: n
          <| sc         -: pat
      PViewPat l exp pat ->
        c PViewPat
          <| sc       -: l
          <| exprV sc -: exp
          <| sc       -: pat
      _ -> defaultRtraverse e sc

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (PatField l) where
  rtraverse e sc =
    case e of
      PFieldPat l qn pat ->
        c PFieldPat
          <| sc       -: l
          <| exprV sc -: qn
          <| sc       -: pat
      PFieldPun l qn ->
        c PFieldPun
          <| sc -: l
          <| exprV sc -: qn
      -- In future we might want to annotate PFieldWildcard with the names
      -- it introduces.
      PFieldWildcard {} -> defaultRtraverse e sc

-- | Chain a sequence of nodes where every node may introduce some
-- variables into scope for the subsequent nodes. Examples: patterns (see
-- note [Nested pattern scopes]), statements.
chain
  :: ( Resolvable (a l)
     , GetBound (a l) l
     , Applicative w
     , SrcInfo l
     , D.Data l
     , ?alg :: Alg w)
  => [a l] -> Scope -> (w [a l], Scope)
chain pats sc =
  case pats of
    [] -> (pure [], sc)
    p:ps ->
      let
        sc' = intro p sc
        p' = alg p sc
        (ps', sc'') = chain ps sc'
      in ((:) <$> p' <*> ps', sc'')

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (Match l) where
  rtraverse e sc =
    case e of
      Match l name pats rhs mbWhere ->
        -- f x y z = ...
        --   where ...
        let
          (pats', scWithPats) = chain pats sc
          scWithWhere = intro mbWhere scWithPats
        in
        c Match
          <| sc                -: l
          <| binderV sc        -: name
          <*> pats' -- has been already traversed
          <| exprV scWithWhere -: rhs
          <| scWithPats        -: mbWhere
      InfixMatch l pat1 name patsRest rhs mbWhere ->
        let
          equivalentMatch = Match l name (pat1:patsRest) rhs mbWhere
          back (Match l name (pat1:patsRest) rhs mbWhere) =
            InfixMatch l pat1 name patsRest rhs mbWhere
          back _ = error "InfixMatch"
        in back <$> rtraverse equivalentMatch sc

-- NB: there is an inefficiency here (and in similar places), because we
-- call intro on the same subtree several times. Maybe tackle it later.
instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (Binds l) where
  rtraverse e sc =
    case e of
      BDecls l decls ->
        let scWithBinds = intro decls sc
        in
        c BDecls
          <| sc          -: l
          <| scWithBinds -: decls
      _ -> defaultRtraverse e sc

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (Exp l) where
  rtraverse e sc =
    case e of
      Let l bnds body ->
        let scWithBinds = intro bnds sc
        in
        c Let
          <| sc          -: l
          <| scWithBinds -: bnds
          <| scWithBinds -: body

      Lambda l pats body ->
        let (pats', scWithPats) = chain pats sc
        in
        c Lambda
          <|  sc         -: l
          <*> pats'
          <|  scWithPats -: body

      ListComp l e stmts ->
        let (stmts', scWithStmts) = chain stmts sc
        in
        c ListComp
          <|  sc -: l
          <|  scWithStmts -: e
          <*> stmts'

      ParComp l e stmtss ->
        let
          (stmtss', scsWithStmts) =
            unzip $ map (\stmts -> chain stmts sc) stmtss
          scWithAllStmtss = foldl1' mergeLocalScopes scsWithStmts
        in
        c ParComp
          <|  sc -: l
          <|  scWithAllStmtss -: e
          <*> T.sequenceA stmtss'

      Proc l pat e ->
        let scWithPat = intro pat sc
        in
        c Proc
          <| sc -: l
          <| sc -: pat
          <| scWithPat -: e

      RecConstr l qn fields ->
        let
          scWc =
            setWcNames
              (expWcNames
                (sc ^. gTable)
                (sc ^. lTable)
                qn
                fields)
              sc
        in
        c RecConstr
          <| sc   -: l
          <| sc   -: qn
          <| scWc -: fields

      _ -> defaultRtraverse e sc

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (Alt l) where
  rtraverse e sc =
    case e of
      Alt l pat guardedAlts mbWhere ->
        let
          scWithPat = intro pat sc
          scWithBinds = intro mbWhere scWithPat
        in
        c Alt
          <| sc -: l
          <| sc -: pat
          <| scWithBinds -: guardedAlts
          <| scWithBinds -: mbWhere

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (GuardedRhs l) where
  rtraverse e sc =
    case e of
      GuardedRhs l stmts exp ->
        let (stmts', scWithStmts) = chain stmts sc
        in
        c GuardedRhs
          <|  sc -: l
          <*> stmts'
          <|  scWithStmts -: exp

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable [Stmt l] where
  rtraverse e sc =
    fst $ chain e sc

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (QualStmt l) where
  rtraverse e sc =
    case e of
      QualStmt {} -> defaultRtraverse e sc
      _ -> error "haskell-names: TransformListComp is not supported yet"

{-
Note [Nested pattern scopes]
~~~~~~~~~~~~~~~~~~~~~~

When we resolve a group of patterns, their scopes nest.

Most of the time, this is not important, but there are two exceptions:
1. ScopedTypeVariables

Example: f (x :: a) (y :: a) = ...

The first 'a' is a binder, the second — a reference.

2. View patterns

An expression inside a view pattern may reference the variables bound
earlier.

Example: f x (find (< x) -> Just y) = ...
-}

-- Some road-block Resolvable instances
instance Typeable a => Resolvable (Scoped a) where
  rtraverse = flip $ const pure
instance Resolvable SrcSpan where
  rtraverse = flip $ const pure
instance Resolvable SrcSpanInfo where
  rtraverse = flip $ const pure
