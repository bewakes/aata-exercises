-- Write a progam to implement a (15, 11)-linear code. Your program should be
-- able to encode and decode messages using coset decoding. Once your program
-- is written, write a program to simulate a binary symmetric channel with
-- transmission noise. Compare the results of your simulation with the
-- theoretically predicted error probability
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Z2 = Zero
        | One
        deriving (Eq, Ord)

instance Show Z2 where
    show Zero = "0"
    show One = "1"

type Information = V.Vector Z2
type CodeWord = V.Vector Z2

instance Num Z2 where
    One + One = Zero
    Zero + Zero = Zero
    _ + _ = One

    abs a = a
    signum _ = 1

    (-) = (+)

    One * One = One
    _ * _ = Zero

    fromInteger 0 = Zero
    fromInteger 1 = One
    -- TODO: handle negatives
    fromInteger n = fromInteger $ n `rem` 2

data BinaryMatrix a =
    BinaryMatrix { size :: (Int, Int)
                 , rows :: V.Vector (V.Vector a)
                 }

instance Functor BinaryMatrix where
    fmap f (BinaryMatrix s rws) =
        BinaryMatrix { size = s
                     , rows = V.map (V.map f) rws
                     }

instance (Show a) => Show (BinaryMatrix a) where
    show (BinaryMatrix (r, c) rws) = (unlines . V.toList . V.map show) rws ++ show r ++ "x" ++ show c ++ "\n"

dot :: (Num a) => V.Vector a -> V.Vector a -> a
dot a b = sum (V.zipWith (*) a b)

getColumn :: Int -> BinaryMatrix a -> V.Vector a
getColumn index (BinaryMatrix _ rws) = V.map (V.! index) rws

transpose :: BinaryMatrix a -> BinaryMatrix a
transpose (BinaryMatrix (r, c) rws) =
    BinaryMatrix {
                   size = (c, r)
                 , rows = V.foldl createCols (V.replicate c V.empty) rws
                 }
                     where createCols = V.zipWith (\l e -> l V.++ V.singleton e)

multiply :: (Num a) => BinaryMatrix a -> BinaryMatrix a -> BinaryMatrix a
multiply a b
  | c1 == r2 = BinaryMatrix
      { size = (r1, c2)
      , rows = V.map (V.zipWith dot bCols . V.replicate c2) $ rows a
      }
  | otherwise = error "Invalid size of matrices"
  where (r1, c1) = size a
        (r2, c2) = size b
        bCols = V.map (`getColumn` b) (V.fromList [0..(c2-1)])

validateSize :: [[a]] -> Maybe (Int, Int)
validateSize [] = Nothing
validateSize (x:xs)
  | all ((== colsize) . length) xs = Just (rowsize, colsize)
  | otherwise = Nothing
    where colsize = length x
          rowsize = length xs + 1

createMatrix :: [[a]] -> BinaryMatrix a
createMatrix arr = case validateSize arr of
                     Just sz -> BinaryMatrix { size=sz, rows=V.fromList (map V.fromList arr) }
                     _ -> error "The rows should be present and have equal lengths"


augmentVertically :: BinaryMatrix a -> BinaryMatrix a -> BinaryMatrix a
-- TODO check sizes
augmentVertically mat1 mat2 = BinaryMatrix
    {
      size = (r1+r2, c1)
    , rows = rows mat1 V.++ rows mat2
    }
        where (r1, c1) = size mat1
              (r2, c2) = size mat2

augmentHorizontally :: BinaryMatrix a -> BinaryMatrix a -> BinaryMatrix a
-- TODO check sizes
augmentHorizontally mat1 mat2 = BinaryMatrix
    {
      size = (r1, c1 + c2)
      , rows = V.zipWith (V.++) (rows mat1) (rows mat2)
    }
        where (r1, c1) = size mat1
              (r2, c2) = size mat2


createIdentity :: Int -> a -> a -> BinaryMatrix a
createIdentity n oneVal zeroVal =
    BinaryMatrix { size = (n, n)
                 , rows = V.fromList $ map createIdentityRow [1..n]
                 }
                     where createIdentityRow k = V.fromList $ replicate (k - 1 ) zeroVal ++ [oneVal] ++ replicate (n - k) zeroVal


hammingBitsMatrix :: BinaryMatrix Z2
hammingBitsMatrix = transpose $ fromInteger <$> createMatrix
    [
      [0, 0, 1, 1]
    , [0, 1, 0, 1]
    , [0, 1, 1, 0]
    , [0, 1, 1, 1]
        {- , [1, 0, 0, 1]
    , [1, 0, 1, 0]
    , [1, 0, 1, 1]
    , [1, 1, 0, 0]
    , [1, 1, 0, 1]
    , [1, 1, 1, 0]
    , [1, 1, 1, 1]
        -}
    ]

blockSize = 8

identityMatrix = createIdentity (blockSize - (snd . size) hammingBitsMatrix) One Zero

hammingMatrix :: BinaryMatrix Z2
hammingMatrix = augmentHorizontally hammingBitsMatrix identityMatrix

generatorMatrix :: BinaryMatrix Z2
generatorMatrix = augmentVertically (createIdentity (blockSize - (fst . size) identityMatrix) One Zero) hammingBitsMatrix

generateNBits :: Int -> V.Vector (V.Vector Z2)
generateNBits bitsLen = V.mapM (const (V.fromList [Zero, One])) $ V.fromList $ replicate bitsLen Zero

toColumnMatrix :: V.Vector a -> BinaryMatrix a
-- TODO: empty vector raise error
toColumnMatrix vec = transpose $ BinaryMatrix (1, V.length vec) (V.singleton vec)

toColumnVector :: BinaryMatrix a -> V.Vector a
toColumnVector mat = V.map (V.! 0) (rows mat)

weight :: V.Vector Z2 -> Int
weight vec = V.length (V.filter (== One) vec)

vectorSum :: V.Vector Z2 -> V.Vector Z2 -> V.Vector Z2
vectorSum = V.zipWith (+)


generateCodewords :: BinaryMatrix Z2 -> Int -> V.Vector CodeWord
generateCodewords genMat infoSize = V.map (toColumnVector. multiply genMat . toColumnMatrix) (generateNBits infoSize)

cosetLeaders :: V.Vector CodeWord -> S.Set CodeWord
cosetLeaders codeGroup = V.foldl updateCosetLeaderSet S.empty possibleBits
    where codeSize = V.length $ codeGroup V.! 0  -- length of first codeword
          possibleBits = generateNBits codeSize
          updateCosetLeaderSet set nTuple = S.insert cosetLeader set
              where cosetElems = V.zipWith vectorSum (V.fromList $ replicate (length codeGroup) nTuple) codeGroup
                    cosetLeader = V.foldl (\minm elem -> if weight elem < weight minm then elem else minm) (V.head cosetElems) (V.tail cosetElems)

syndromeCosetLeaderMap :: BinaryMatrix Z2 -> S.Set CodeWord -> M.Map (V.Vector Z2) CodeWord
syndromeCosetLeaderMap hamMatrix cosetLeaders = M.fromList (zip syndromes leadersList)
    where leadersList = S.toList cosetLeaders
          syndromes = map (toColumnVector. multiply hamMatrix . toColumnMatrix) leadersList


decode :: CodeWord -> BinaryMatrix Z2 -> M.Map (V.Vector Z2) CodeWord -> CodeWord
decode word hamMatrix syndromeCosetMap =
    case M.lookup syndrome syndromeCosetMap of
      Just x -> vectorSum x word
      _ -> error "Oh My god!!"
    where syndrome = (toColumnVector . multiply hamMatrix . toColumnMatrix) word


cws = [
      [0,0,0,0,0,0,0,0]
    , [0,0,0,1,0,1,1,1]
    , [0,0,1,0,0,1,1,0]
    , [0,0,1,1,0,0,0,1]
    , [0,1,0,0,0,1,0,1]
    , [0,1,0,1,0,0,1,0]
    , [0,1,1,0,0,0,1,1]
    , [0,1,1,1,0,1,0,0]
    , [1,0,0,0,0,0,1,1]
    , [1,0,0,1,0,1,0,0]
    , [1,0,1,0,0,1,0,1]
    , [1,0,1,1,0,0,1,0]
    , [1,1,0,0,0,1,1,0]
    , [1,1,0,1,0,0,0,1]
    , [1,1,1,0,0,0,0,0]
    , [1,1,1,1,0,1,1,1]
    ]

cw = fromInteger <$> V.fromList ([1,1,1,0,0,0,0,0] :: [Integer])
modifiedcw = fromInteger <$> V.fromList ([1,1,1,1,0,0,0,1] :: [Integer])

main :: IO ()
main = do
    let codewords = generateCodewords generatorMatrix 4
    let leadersSet = cosetLeaders codewords
    let syndromeMap = syndromeCosetLeaderMap hammingMatrix leadersSet
    print syndromeMap
    print $ decode modifiedcw hammingMatrix syndromeMap
