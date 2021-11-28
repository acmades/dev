module Learn.Function.Func  where
import Data.List
import Data.Map.Strict ((!))
import Data.List.Split (splitOn)

-- https://www.codewars.com/kata/5541f58a944b85ce6d00006a//haskell
productFib :: Integer -> (Integer, Integer, Bool)
productFib n = (a, b, a*b==n) where
    (a, b) = head $ dropWhile (\(a,b) -> a*b < n) $ iterate' (\(a, b) -> (b, a + b)) (0, 1)
--productFib' :: Integer -> (Integer, Integer, Bool)
--productFib' n = go 0 1 n where
--    go a b c
--        | a*b >= c = (a, b, a*b==n)
--        | otherwise = go b (a+b) c

-- --------------------------------------------------

-- https://www.codewars.com/kata/5592e3bd57b64d00f3000047/haskell
findNb :: Integer -> Integer
findNb = go 0 1 where
    go a b c
        | a > c = -1
        | a == c = b-1
        | otherwise  = go (a+b^3) (b+1) c

-- --------------------------------------------------

-- https://www.codewars.com/kata/55c04b4cc56a697bb0000048/haskell
scramble :: [Char] -> [Char] -> Bool
scramble s1 s2 = null ([x | x <- nub s2, length (elemIndices x s2) > length (elemIndices x s1)])

-- --------------------------------------------------

-- https://www.codewars.com/kata/5839edaa6754d6fec10000a2/haskell
findMissingLetter :: [Char] -> Char
findMissingLetter cs = head [succ x | x<-cs, succ x `notElem` cs]
--findMissingLetter cs = head $ [head cs .. last cs] \\ cs

-- --------------------------------------------------

-- https://www.codewars.com/kata/52f787eb172a8b4ae1000a34/haskell
zeros :: Int -> Int
zeros x = sum $ takeWhile (>0) $ zipWith (\a b -> x`div`a^b) [5,5..] [1..]
--zeros' 0 = 0
--zeros' n = d + zeros (n `div` 5)
--  where d = n `div` 5

-- --------------------------------------------------

-- https://www.codewars.com/kata/54521e9ec8e60bc4de000d6c/haskell
maxSequence :: [Int] -> Int
--maxSequence [] = 0
--maxSequence n
--    | all (<0) n = 0
--    | otherwise = res n
--    where res all@(x:xs) = snd $ foldr (\a (b,c) -> (max (a + b) 0, max c (a + b))) (0, x) all
maxSequence = maximum . scanl (\acc b -> max 0 (acc + b)) 0

-- --------------------------------------------------

-- https://www.codewars.com/kata/54d496788776e49e6b00052f/haskell
sumOfDivided :: [Int] -> [(Int, Int)]
sumOfDivided = summ . sumOfDivided'
  where 
    fact n p acc
      | n == 1 = acc
      | n `mod` p == 0 = fact (n `div` p) p (p:acc)
      | p * p > n = fact n n acc
      | otherwise = fact n (p+1) acc
    sumOfDivided' x = sortOn fst [ (y, a) | a <- x, y <- nub $ fact (abs a) 2 []]
    summ [] = []
    summ [(a, b)] = [(a,b)]
    summ ((a,b):(c,d):s) = if a==c then summ ((a, b+d):s) else (a,b): summ ((c,d):s)

-- --------------------------------------------------

-- https://www.codewars.com/kata/5277c8a221e209d3f6000b56/haskell
validBraces :: String -> Bool
validBraces = (==) "." . foldl (\(a:strA) b -> if f a b then strA else b:a:strA) ['.']
  where 
    f a b = [a,b] == "()" || [a,b] == "{}" || [a,b] == "[]"

-- --------------------------------------------------

-- https://www.codewars.com/kata/54b724efac3d5402db00065e/haskell
-- Decode the Morse code - 1/3 - It's work!
-- Not imported morseCodes !!!
{-
decodeMorse :: String -> String
decodeMorse = unwords . filter (/="") . map (concatMap (morseCodes !) . words) . splitOn "   "
-}

-- --------------------------------------------------

-- https://www.codewars.com/kata/54b72c16cd7f5154e9000457/haskell
-- Decode the Morse code - 2/3 - It's work!
{- Fist iteration

data ZF = Zero {runZF::Int} | One {runZF::Int} deriving (Show, Read, Eq,Ord)

add::ZF -> ZF
add (Zero a) = Zero (a+1)
add (One a)  = One (a+1)

mapZF b = fromList [(Zero b,""),(Zero (3*b)," "),(Zero (7*b),"   "),(One b, "."),(One (3*b), "-")]

revZF::ZF->ZF
revZF x = case x of (Zero a) -> One 1; One a  -> Zero 1

dropLast'0 :: [Char] -> [Char]
dropLast'0 str = case last str of 
                      '0' -> dropLast'0 (init str)
                      _      -> str

decZF :: [Char] -> ([ZF], Int)
decZF = go 1000 (One 1) [] where
    go minVal z ac []      = (ac,minVal)
    go minVal z ac [a]     = go (min minVal (runZF z)) z (ac++[z]) []
    go minVal z ac (a:b:c) 
        | a == b    = go minVal (add z) ac (b:c)
        | otherwise = go (min minVal (runZF z)) (revZF z) (ac++[z]) (b:c)

comparZF :: Foldable t => (t ZF, Int) -> [Char]
comparZF (a,b) = concatMap (mapZF b !) a

decodeBits :: String -> String
decodeBits = comparZF . decZF . dropLast'0 . dropWhile (/='1')
-}
{- Second iteration
dropLast'0 :: [Char] -> [Char]
dropLast'0 str = case last str of 
                      '0' -> dropLast'0 (init str)
                      _      -> str

decodeBits :: String -> String
decodeBits str = concatMap (mapZF !) . group $ dropstr where
    b = minimum . map length . group $ dropstr
    dropstr = dropLast'0 . dropWhile (/='1') $ str
    mapZF = fromList [(replicate b '0',""),(replicate (b*3) '0'," "),
                        (replicate (b*7) '0',"   "),(replicate b '1', "."),
                        (replicate (b*3) '1', "-")]
--}
--decode = decodeMorse . decodeBits

-- --------------------------------------------------

-- 


-- --------------------------------------------------