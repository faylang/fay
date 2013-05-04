module Fay.Compiler.GADT(convertGADT) where

import           Language.Haskell.Exts hiding (name, binds)

-- | Convert a GADT to a normal data type.
convertGADT :: GadtDecl -> QualConDecl
convertGADT d =
  case d of
    GadtDecl srcloc name typ -> QualConDecl srcloc tyvars context
                                            (ConDecl name (convertFunc typ))
  where tyvars = []
        context = []
        convertFunc (TyCon _) = []
        convertFunc (TyFun x xs) = UnBangedTy x : convertFunc xs
        convertFunc (TyParen x) = convertFunc x
        convertFunc _ = []
