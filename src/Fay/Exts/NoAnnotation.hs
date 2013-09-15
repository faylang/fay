{-# OPTIONS -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances #-}
module Fay.Exts.NoAnnotation where


import           Data.Char                       (isAlpha)
import           Data.List                       (intercalate)
import           Data.List.Split                 (splitOn)
import           Data.String
import qualified Language.Haskell.Exts.Annotated as A

type Alt = A.Alt ()
type BangType = A.BangType ()
type ClassDecl = A.ClassDecl ()
type Decl = A.Decl ()
type DeclHead = A.DeclHead ()
type Ex = A.Exp ()
type Exp = A.Exp ()
type ExportSpec = A.ExportSpec ()
type FieldDecl = A.FieldDecl ()
type FieldUpdate = A.FieldUpdate ()
type GadtDecl = A.GadtDecl ()
type GuardedAlts = A.GuardedAlts ()
type GuardedRhs = A.GuardedRhs ()
type ImportDecl = A.ImportDecl ()
type ImportSpec = A.ImportSpec ()
type Literal = A.Literal ()
type Match = A.Match ()
type Module = A.Module ()
type ModuleName = A.ModuleName ()
type ModulePragma = A.ModulePragma ()
type Name = A.Name ()
type Pat = A.Pat ()
type PatField = A.PatField ()
type QName = A.QName ()
type QOp = A.QOp ()
type QualConDecl = A.QualConDecl ()
type QualStmt = A.QualStmt ()
type Rhs = A.Rhs ()
type SpecialCon = A.SpecialCon ()
type SrcLoc = A.SrcLoc
type SrcSpan = A.SrcSpan
type SrcSpanInfo = A.SrcSpanInfo
type Stmt = A.Stmt ()
type TyVarBind = A.TyVarBind ()
type Type = A.Type ()

unAnn :: Functor f => f a -> f ()
unAnn = fmap (const ())

-- | Helpful for some things.
instance IsString (A.Name ()) where
  fromString n@(c:_)
    | isAlpha c || c == '_' = A.Ident () n
    | otherwise             = A.Symbol () n
  fromString [] = error "Name fromString: empty string"

-- | Helpful for some things.
instance IsString (A.QName ()) where
  fromString s = case splitOn "." s of
    []  -> error "QName fromString: empty string"
    [x] -> A.UnQual () $ fromString x
    xs  -> A.Qual () (fromString $ intercalate "." $ init xs) $ fromString (last xs)

-- | Helpful for writing qualified symbols (Fay.*).
instance IsString (A.ModuleName ()) where
   fromString = A.ModuleName ()
