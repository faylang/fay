import           Prelude

data Ty1 = Integer `InfixConst1` Integer

data Ty2 = Bool `InfixConst2` Bool

data Ty3 = Ty1 :=> Ty2

t = (123 `InfixConst1` 123) :=> (False `InfixConst2` True)

main = print t
