module Fay.Exts where

import qualified Language.Haskell.Exts.Annotated as A

type X = A.SrcSpanInfo

type Alt = A.Alt X
type BangType = A.BangType X
type ClassDecl = A.ClassDecl X
type Decl = A.Decl X
type DeclHead = A.DeclHead X
type Ex = A.Exp X
type Exp = A.Exp X
type ExportSpec = A.ExportSpec X
type FieldDecl = A.FieldDecl X
type FieldUpdate = A.FieldUpdate X
type GadtDecl = A.GadtDecl X
type GuardedAlts = A.GuardedAlts X
type GuardedRhs = A.GuardedRhs X
type ImportDecl = A.ImportDecl X
type ImportSpec = A.ImportSpec X
type Literal = A.Literal X
type Match = A.Match X
type Module = A.Module X
type ModuleName = A.ModuleName X
type ModulePragma = A.ModulePragma X
type Name = A.Name X
type Pat = A.Pat X
type PatField = A.PatField X
type QName = A.QName X
type QOp = A.QOp X
type QualConDecl = A.QualConDecl X
type QualStmt = A.QualStmt X
type Rhs = A.Rhs X
type SpecialCon = A.SpecialCon X
type SrcLoc = A.SrcLoc
type Stmt = A.Stmt X
type TyVarBind = A.TyVarBind X
type Type = A.Type X

moduleName :: A.SrcInfo a => A.Module a -> A.ModuleName a
moduleName (A.Module _ (Just (A.ModuleHead _ n _ _)) _ _ _) = n
moduleName (A.Module a Nothing                     _ _ _) = A.ModuleName a "Main"
moduleName m = error $ "moduleName: " ++ A.prettyPrint m

moduleExports :: A.Module X -> Maybe (A.ExportSpecList X)
moduleExports (A.Module _ (Just (A.ModuleHead _ _ _ e)) _ _ _) = e
moduleExports (A.Module _ Nothing                     _ _ _) = Nothing
moduleExports m = error $ ("moduleExports: " ++ A.prettyPrint m)

moduleNameString :: A.ModuleName t -> String
moduleNameString (A.ModuleName _ n) = n

mkIdent :: String -> A.Name A.SrcSpanInfo
mkIdent = A.Ident noI

noI :: A.SrcSpanInfo
noI = A.noInfoSpan (A.mkSrcSpan A.noLoc A.noLoc)

convertFieldDecl :: A.FieldDecl a -> ([A.Name a], A.BangType a)
convertFieldDecl (A.FieldDecl _ ns b) = (ns, b)

fieldDeclNames :: A.FieldDecl a -> [A.Name a]
fieldDeclNames (A.FieldDecl _ ns _) = ns

declHeadName :: A.DeclHead a -> A.Name a
declHeadName d = case d of
  A.DHead _ n _ -> n
  A.DHInfix _ _ n _ -> n
  A.DHParen _ h -> declHeadName h
