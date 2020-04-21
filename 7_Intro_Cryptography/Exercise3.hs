import qualified Data.Map as M
import Data.Char(ord, chr)

printList :: (Show a) => [a] -> IO ()
printList [] = print ""
printList (x:xs) = do
    print x
    printList xs

gcd' :: Int -> Int -> Int
gcd' a b
  | b `rem` a == 0 = a
  | otherwise = gcd' (b `rem` a) a

relativelyPrimes26 :: [Int]
relativelyPrimes26 = filter ((== 1) . gcd' 26) [1..25]

abForNEqualsN :: Int -> [(Int, Int)]
abForNEqualsN n = [(a, b) | a <- relativelyPrimes26, b <- [0..26], (a*n + b) `rem` 26 == n `rem` 26]

inverseAB :: Int -> Int -> (Int, Int)
inverseAB a b = (inverseMod26 a, 26 - b)

-- This is brute, also assumes invertible number is passed
inverseMod26 :: Int -> Int
inverseMod26 n = head [x | x <- relativelyPrimes26, x*n `rem` 26 == 1]

decryptAffine :: Int -> Int -> (Int -> Int)
decryptAffine a b p = (inverseMod26 a * p + (26 - b) * inverseMod26 a + 26) `rem` 26


message :: String
message = "APHUO EGEHP PEXOV FKEUH CKVUE CHKVE APHUO \
         \ EGEHU EXOVL EXDKT VGEFT EHFKE UHCKF TZEXO \
         \ VEZDT TVKUE XOVKV ENOHK ZFTEH TEHKQ LEROF \
         \ PVEHP PEXOV ERYKP GERYT GVKEG XDRTE RGAGA"

alphaMessage :: String
alphaMessage = filter (/= ' ') message

intMessage :: [Int]
intMessage = alphaMessage ==> strToAlphaInts

(==>) :: a -> (a -> b) -> b
x ==> f = f x

ordA :: Int
ordA = ord 'A'

getFrequency :: String -> M.Map Char Int
getFrequency string = M.fromListWith (+) [(x, 1) | x <- string, x /= ' ']

strToAlphaInts :: String -> [Int]
strToAlphaInts = map (\x -> ord x - ordA)

intsToAlphaStr :: [Int] -> String
intsToAlphaStr = map (\x -> chr (x + ordA))

shiftMod26 :: Int -> (Int -> Int)
shiftMod26 n = (`rem` 26) . (n+)

bruteDecryptWithAffine :: [String]
bruteDecryptWithAffine = [intMessage ==> map (decryptAffine a b) ==> intsToAlphaStr | (a, b) <- abForNEqualsN 4]

main :: IO ()
main = do 
    print $ getFrequency message
    printList bruteDecryptWithAffine
    -- printList [alphaMessage ==>(abForNEqualsN 4)
