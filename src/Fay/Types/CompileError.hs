module Fay.Types.CompileError (CompileError (..)) where

import qualified Fay.Exts                        as F
import qualified Fay.Exts.NoAnnotation           as N
import qualified Fay.Exts.Scoped                 as S

import           Language.Haskell.Exts.Annotated

-- | Error type.
data CompileError
  = Couldn'tFindImport N.ModuleName [FilePath]
  | EmptyDoBlock
  | FfiFormatBadChars SrcSpanInfo String
  | FfiFormatIncompleteArg SrcSpanInfo
  | FfiFormatInvalidJavaScript SrcSpanInfo String String
  | FfiFormatNoSuchArg SrcSpanInfo Int
  | FfiNeedsTypeSig S.Exp
  | GHCError String
  | InvalidDoBlock
  | ParseError S.SrcLoc String
  | ShouldBeDesugared String
  | UnableResolveQualified N.QName
  | UnsupportedDeclaration S.Decl
  | UnsupportedEnum N.Exp
  | UnsupportedExportSpec N.ExportSpec
  | UnsupportedExpression S.Exp
  | UnsupportedFieldPattern S.PatField
  | UnsupportedImport F.ImportDecl
  | UnsupportedLet
  | UnsupportedLetBinding S.Decl
  | UnsupportedLiteral S.Literal
  | UnsupportedModuleSyntax String F.Module
  | UnsupportedPattern S.Pat
  | UnsupportedQualStmt S.QualStmt
  | UnsupportedRecursiveDo
  | UnsupportedRhs S.Rhs
  | UnsupportedWhereInAlt S.Alt
  | UnsupportedWhereInMatch S.Match
  deriving (Show)
