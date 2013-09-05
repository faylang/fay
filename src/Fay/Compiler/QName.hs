-- | Extras for haskell-src-exts names.

module Fay.Compiler.QName where

import           Language.Haskell.Exts.Annotated

-- | Extract the module name from a qualified name.
qModName :: QName a -> Maybe (ModuleName a)
qModName (Qual _ m _) = Just m
qModName _          = Nothing

-- | Extract the name from a QName.
unQual :: QName a -> Name a
unQual (Qual _ _ n) = n
unQual (UnQual _ n) = n
unQual Special{} = error "unQual Special{}"

unQualify :: QName a -> QName a
unQualify (Qual a _ n) = UnQual a n
unQualify u@UnQual{} = u
unQualify Special{}  = error "unQualify: Special{}"

-- | Change or add the ModuleName of a QName.
changeModule :: ModuleName a -> QName a -> QName a
changeModule m (Qual a _ n) = Qual a m n
changeModule m (UnQual a n) = Qual a m n
changeModule _ Special{}  = error "changeModule Special{}"

changeModule' :: (String -> String) -> QName a -> QName a
changeModule' f (Qual l (ModuleName ml mn) n) = Qual l (ModuleName ml $ f mn) n
changeModule' _ x = x

-- | Extract the string from a Name.
unname :: Name a -> String
unname (Ident _ s) = s
unname (Symbol _ s) = s
