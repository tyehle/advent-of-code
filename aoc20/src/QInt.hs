{-# LANGUAGE FlexibleInstances #-}
module QInt where


type H = (Integer, Integer, Integer, Integer)
instance Num H where
  (a1, b1, c1, d1) + (a2, b2, c2, d2) = (a1+a2, b1+b2, c1+c2, d1+d2)
  (a1, b1, c1, d1) * (a2, b2, c2, d2) = ( a1*a2 - b1*b2 - c1*c2 - d1*d2
                                        , a1*b2 + b1*a2 + c1*d2 - d1*c2
                                        , a1*c2 - b1*d2 + c1*a2 + d1*b2
                                        , a1*d2 + b1*c2 - c1*b2 + d1*a2
                                        )
  negate (a, b, c, d) = (negate a, negate b, negate c, negate d)
  fromInteger n = (n, 0, 0, 0)
  abs _ = undefined
  signum _ = undefined


rotate :: H -> H -> Integer -> H
rotate point dir n
  | n == 0    = point
  | n < 0     = rotate point (negate dir) (negate n)
  | otherwise = rotate (point * dir) dir (n - 1)


fromVec :: (Integer, Integer, Integer) -> H
fromVec (b, c, d) = (0, b, c, d)


toVec :: H -> (Integer, Integer, Integer)
toVec (a, b, c, d)
  | a /= 0 = error "Not a pure quaternion"
  | otherwise = (b, c, d)


l1Distance :: H -> H -> Integer
l1Distance (a1, b1, c1, d1) (a2, b2, c2, d2) = abs (a1-a2) + abs (b1-b2) + abs (c1-c2) + abs (d1-d2)


neighbors :: H -> [H]
neighbors (a, b, c, d) = [(a+a', b+b', c+c', d+d') | a' <- diff, b' <- diff, c' <- diff, d' <- diff, (a', b', c', d') /= (0, 0, 0, 0)]
  where diff = [-1, 0, 1]