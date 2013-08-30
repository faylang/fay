-- | Convert GADTs into normal data types.

module Fay.Compiler.GADT
  (convertGADT
  ) where

import qualified Fay.Exts as F
import Fay.Exts (noI)
import           Language.Haskell.Exts.Annotated hiding (name, binds)

-- | Convert a GADT to a normal data type.
convertGADT :: F.GadtDecl -> F.QualConDecl
convertGADT d =
  case d of
    GadtDecl srcloc name typ -> QualConDecl srcloc tyvars context
                                            (ConDecl noI name (convertFunc typ))
  where tyvars = Nothing
        context = Nothing
        convertFunc :: F.Type -> [F.BangType]
        convertFunc (TyCon _ _) = []
        convertFunc (TyFun _ x xs) = UnBangedTy noI x : convertFunc xs
        convertFunc (TyParen _ x) = convertFunc x
        convertFunc _ = []
