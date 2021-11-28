module Learn.Function.Func  where
import Data.List
import Data.List.Split (splitOn)
import Data.Char ( toLower, toUpper )

{- 1
    Implement a function that accepts 3 integer values a, b, c. The function should 
    return true if a triangle can be built with the sides of given length and false 
    in any other case.
    (In this case, all triangles must have surface greater than 0 to be accepted).
-}
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = x + y > z
    where [x, y, z] = sort [a,b,c]

{- 2
    Trolls are attacking your comment section!
    A common way to deal with this situation is to remove all of the vowels from the 
    trolls' comments, neutralizing the threat.
    Your task is to write a function that takes a string and return a new string with 
    all vowels removed.
    For example, the string "This website is for losers LOL!" 
    would become "Ths wbst s fr lsrs LL!".
-}
disemvowel :: String -> String
disemvowel str = [x | x <- str, toLower x `notElem` "aeiou"]

{- 3
    In this little assignment you are given a string of space 
    separated numbers, and have to return the highest and lowest number.
-}
highAndLow :: String -> String
highAndLow input = show (maximum numInput) ++ " " ++ show (minimum numInput)
    where numInput = map read $ words input :: [Int]

{- 4
    An isogram is a word that has no repeating letters, consecutive or non-consecutive. 
    Implement a function that determines whether a string that contains only letters 
    is an isogram. Assume the empty string is an isogram. Ignore letter case.
-}
isIsogram :: String -> Bool
isIsogram [] = True
isIsogram xs = head f `notElem` tail f && isIsogram (tail xs)
    where f = map toLower xs
-- isIsogram xs = map toLower xs == nub (map toLower xs)
-- isIsogram xs = length xs == length (nub (map toLower xs))

{- 5
    Complete the solution so that it splits the string into pairs of two characters. 
    If the string contains an odd number of characters then it should replace the 
    missing second character of the final pair with an underscore ('_').
-}
solution :: String -> [String]
solution [] = []
solution [x] = [x : "_"]
--solution x = take 2 x : solution (drop 2 x)
solution (x:s:xs) = [x, s] : solution xs

{- 6
    Well met with Fibonacci bigger brother, AKA Tribonacci.
    As the name may already reveal, it works basically like a Fibonacci, but summing 
    the last 3 (instead of 2) numbers of the sequence to generate the next. And, worse 
    part of it, regrettably I won't get to hear non-native Italian speakers trying to 
    pronounce it :(
    So, if we are to start our Tribonacci sequence with [1, 1, 1] as a starting input 
    (AKA signature), we have this sequence:
-}
--tribonacci :: Num a => (a, a, a) -> Int -> [a]
--tribonacci (a, b, c) n = take n (fibs (a, b, c))
--    fibs :: Num a => (a, a, a) -> [a]
--    fibs r@(i,j,k) = i:j:k:next (fibs r)
--    where
--        next (a : t@(b : c : _)) = (a+b+c) : next 
tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (x, y, z) n = take n $ map sel1 $ iterate (\(a,b,c) -> (b,c,a+b+c)) (x, y, z)
    where sel1 (a,_,_ ) = a
--tribonacci (a, b, c) n = a:tribonacci (b,c,a+b+c) (n-1)

{- 7
    A string is considered to be in title case if each word in the string is either 
    (a) capitalised (that is, only the first letter of the word is in upper case) or 
    (b) considered to be an exception and put entirely into lower case unless it is 
    the first word, which is always capitalised.
    First argument: space-delimited list of minor words that must always be lowercase 
    except for the first word in the string.
    Second argument: the original string to be converted.
-}
titleCase :: String -> String -> String
titleCase minor title = toUpperHead (unwords ([passOrCapitalize x | x <- titleLow]))
    where minorLow           = words $ map toLower minor
          titleLow           = words $ map toLower title
          toUpperHead (a:b)  = toUpper a : b
          toUpperHead []     = []
          passOrCapitalize y = if y `elem` minorLow  then y else toUpperHead y

-------------------------------------------------------------------------------------

{- 1
    Let's assume that a song consists of some number of words (that don't contain WUB).
    For example, a song with words "I AM X" can transform into a dubstep remix as 
    "WUBWUBIWUBAMWUBWUBX" and cannot transform into "WUBWUBIAMWUBX".
    Recently, Jonny has heard Polycarpus's new dubstep track, but since he isn't into 
    modern music, he decided to find out what was the initial song that Polycarpus remixed. 
    Help Jonny restore the original song.
-}
songDecoder :: String -> String
songDecoder str = unwords [x | x<-splitOn "WUB" str, x /=""]

{- 2
    Write a function that takes a string of braces, and determines if the order of the 
    braces is valid. It should return true if the string is valid, and false if it's invalid.
    A string of braces is considered valid if all braces are matched with the correct brace.
-}
