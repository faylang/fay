-- | Convert GADTs into normal data types.

module Fay.Compiler.GADT
  (convertGADT
  ) where

import           Language.Haskell.Exts hiding (name)

-- | Convert a GADT to a normal data type.
convertGADT :: GadtDecl a -> QualConDecl a
convertGADT d = case d of
  GadtDecl s name tyvars context Nothing typ ->
    QualConDecl s tyvars context (ConDecl s name (convertFunc typ))
  GadtDecl s name tyvars context (Just fs) _typ ->
    QualConDecl s tyvars context (RecDecl s name fs)
  where
    convertFunc :: Type a -> [Type a]
    convertFunc (TyCon _ _) = []
    convertFunc (TyFun _ x xs) = x : convertFunc xs
    convertFunc (TyParen _ x) = convertFunc x
    convertFunc _ = []
