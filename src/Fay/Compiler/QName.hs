module Fay.Compiler.QName where

import Language.Haskell.Exts.Syntax

qModName :: QName -> Maybe ModuleName
qModName (Qual m _) = Just m
qModName _          = Nothing

unQual :: QName -> Name
unQual (Qual _ n) = n
unQual (UnQual n) = n
unQual Special{} = error "unQual Special{}"

unQual' :: QName -> QName
unQual' (Qual _ n) = UnQual n
unQual' u@UnQual{} = u
unQual' Special{}  = error "unQual' Special{}"

changeModule :: ModuleName -> QName -> QName
changeModule m (Qual _ n) = Qual m n
changeModule m (UnQual n) = Qual m n
changeModule _ Special{}  = error "changeModule Special{}"

changeName :: (Name -> Name) -> QName -> QName
changeName f (UnQual n) = UnQual $ f n
changeName f (Qual m n) = Qual m $ f n
changeName _ Special{} = error "changeName Special{}"

name2String :: Name -> String
name2String (Ident s) = s
name2String (Symbol s) = s
