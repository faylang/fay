-- | Extras for haskell-src-exts names.

module Fay.Compiler.QName where

import Language.Haskell.Exts.Syntax

-- | Extract the module name from a qualified name.
qModName :: QName -> Maybe ModuleName
qModName (Qual m _) = Just m
qModName _          = Nothing

-- | Extract the name from a QName.
unQual :: QName -> Name
unQual (Qual _ n) = n
unQual (UnQual n) = n
unQual Special{} = error "unQual Special{}"

-- | Change or add the ModuleName of a QName.
changeModule :: ModuleName -> QName -> QName
changeModule m (Qual _ n) = Qual m n
changeModule m (UnQual n) = Qual m n
changeModule _ Special{}  = error "changeModule Special{}"

-- | Extract the string from a Name.
unname :: Name -> String
unname (Ident s) = s
unname (Symbol s) = s
