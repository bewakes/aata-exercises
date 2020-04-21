data Mat2 = Mat2 Int Int Int Int
    deriving Show

data Poly2System = Poly2System Mat2 Int
    deriving Show

extendedEuclidean:: Int -> Int -> (Int, Int, Int)
extendedEuclidean m n
  | r == 0 = (0, 1, n)
  | otherwise = (b, -q*b + a, g)
  where (a, b, g) = extendedEuclidean n r
        r = m `mod` n
        q = m `div` n

gcd' :: Int -> Int -> Int
gcd' a b = abs g where (_, _, g) = extendedEuclidean a b


inverse :: Poly2System -> Poly2System
inverse (Poly2System (Mat2 a b c d) md) = Poly2System (Mat2 d (md-b `rem` md) (md-c `rem` md) a) md


applyToPair :: Poly2System -> (Int, Int) -> (Int, Int)
applyToPair (Poly2System mat md) (m, n) = ((a * m + b * n) `rem` md, (c * m + d * n) `rem` md)
    where Mat2 a b c d = mat


determinant :: Mat2 -> Int
determinant (Mat2 a b c d) = a * c - b * d

isValid :: Poly2System -> Bool
isValid (Poly2System mat md) = gcd' (determinant mat) md == 1

mat2 :: Mat2
mat2 = Mat2 3 5 1 2

pair = (3, 5)

pairEnc = applyToPair poly2System pair

poly2System = Poly2System mat2 26

main :: IO ()
main = do
    print $ isValid (Poly2System mat2 26)
    print $ inverse poly2System
    print pair
    print pairEnc
    print $ applyToPair (inverse poly2System) pairEnc
