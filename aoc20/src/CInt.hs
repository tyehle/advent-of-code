{-# LANGUAGE FlexibleInstances #-}
module CInt where


type C = (Integer, Integer)
instance Num C where
  (ar, ai) + (br, bi) = (ar+br, ai+bi)
  (ar, ai) * (br, bi) = (ar*br - ai*bi, ar*bi + ai*br)
  negate (r, i) = (negate r, negate i)
  fromInteger n = (n, 0)
  abs _ = undefined
  signum _ = undefined


rotate :: C -> C -> Integer -> C
rotate point vec 90 = point * vec
rotate point vec n = rotate (point * vec) vec (n - 90)


toVec :: Char -> C
toVec 'R' = (0, -1)
toVec 'L' = (0, 1)
toVec 'N' = (0, 1)
toVec 'E' = (1, 0)
toVec 'S' = (0, -1)
toVec 'W' = (-1, 0)