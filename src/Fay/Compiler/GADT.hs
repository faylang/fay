-- | Convert GADTs into normal data types.

module Fay.Compiler.GADT
  (convertGADT
  ) where

import           Language.Haskell.Exts.Annotated hiding (binds, name)

-- | Convert a GADT to a normal data type.
convertGADT :: GadtDecl a -> QualConDecl a
convertGADT d = case d of
  GadtDecl s name typ -> QualConDecl s tyvars context
                           (ConDecl s name (convertFunc typ))
  where
    tyvars = Nothing
    context = Nothing
    convertFunc :: Type a -> [BangType a]
    convertFunc (TyCon _ _) = []
    convertFunc (TyFun s x xs) = UnBangedTy s x : convertFunc xs
    convertFunc (TyParen _ x) = convertFunc x
    convertFunc _ = []
