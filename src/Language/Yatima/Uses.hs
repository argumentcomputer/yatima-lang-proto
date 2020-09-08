module Language.Yatima.Uses where

data Uses = None | Affi | Once | Many deriving (Eq, Show, Enum)

(+#) :: Uses -> Uses -> Uses
None +# x    = x
x    +# None = x
x    +# y    = Many

(*#) :: Uses -> Uses -> Uses
None *# x    = None
Affi *# None = None
Affi *# Affi = Affi
Affi *# Once = Affi
Affi *# Many = Many
Once *# x    = x
Many *# None = None
Many *# x    = Many

(≤#) :: Uses -> Uses -> Bool
None ≤# Once = False
None ≤# x    = True
Affi ≤# None = False
Affi ≤# Once = False
Affi ≤# x    = True
Once ≤# None = False
Once ≤# x    = True
Many ≤# Many = True
Many ≤# x    = False

(>#) :: Uses -> Uses -> Bool
(>#) x y = not (x ≤# y)


