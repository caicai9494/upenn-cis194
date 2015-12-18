data NullableDouble = Nulll | Doublee Double 
    deriving (Eq, Show)

ex01 :: NullableDouble
ex01 = Nulll

ex02 :: NullableDouble
ex02 = Nulll

getDivide :: Double -> Double -> NullableDouble
getDivide _ 0 = Nulll
getDivide a b = Doublee (a / b)

newtype Book Int Int = Book Int Int
