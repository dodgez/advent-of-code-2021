module Lib where

import Data.Char
import Data.Functor

import Debug.Trace

getInput :: String -> IO [String]
getInput file = readFile ("./src/inputs/" ++ file) <&> lines

getDay1Input :: IO [Int]
getDay1Input = getInput "day1.txt" <&> map read

day1 :: IO Int
day1 = do
  depths <- getDay1Input
  let foldFunc = \(acc, l) b -> if b > l then (acc + 1, b) else (acc, b)
  let res = foldl foldFunc (0, 800) depths
  return $ fst res

-- >>> day1

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

slidingWindow :: Int -> [Int] -> [[Int]]
slidingWindow n xs = fst $ foldl (\(acc, i) _ -> if i > (length xs - n) then (acc, i + 1) else (acc ++ [slice i (i + n - 1) xs], i + 1)) ([], 0) xs

sumSlidingWindow :: Int -> [Int] -> [Int]
sumSlidingWindow n xs = map sum $ slidingWindow n xs

day1Part2 :: IO Int
day1Part2 = do
  depths <- getDay1Input
  let slideDepths = sumSlidingWindow 3 depths
  let foldFunc = \(acc, l) b -> if b > l then (acc + 1, b) else (acc, b)
  let res = foldl foldFunc (0, 800) slideDepths
  return (fst res)

-- >>> day1Part2

getDay2Input :: IO [(String, Int)]
getDay2Input = getInput "day2.txt" <&> map words <&> map (\[dir, n] -> (dir, read n :: Int))

command :: (Int, Int) -> (String, Int) -> (Int, Int)
command (h, v) ("forward", n) = (h + n, v)
command (h, v) ("down", n) = (h, v + n)
command (h, v) ("up", n) = (h, v - n)

day2 :: IO Int
day2 = do
  commands <- getDay2Input
  let (h, v) = foldl command (0, 0) commands
  return $ h * v

-- >>> day2

commandPart2 :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
commandPart2 (h, v, a) ("forward", n) = (h + n, v + a * n, a)
commandPart2 (h, v, a) ("down", n) = (h, v, a + n)
commandPart2 (h, v, a) ("up", n) = (h, v, a - n)

day2Part2 :: IO Int
day2Part2 = do
  commands <- getDay2Input
  let (h, v, _) = foldl commandPart2 (0, 0, 0) commands
  return $ h * v

-- >>> day2Part2

getDay3Input :: IO [[Int]]
getDay3Input = getInput "day3.txt" <&> map (map digitToInt)

fromBinary :: [Int] -> Int
fromBinary [] = 0
fromBinary (x:xs) = x * (2 ^ length xs) + fromBinary xs

notBinary :: [Int] -> [Int]
notBinary [] = []
notBinary (x:xs) = (1 - x) : notBinary xs

range :: Int -> [Int]
range n = take n [0..]

sumLists :: [[Int]] -> [Int]
sumLists xs = map sum $ map (\i -> map (!! i) xs) $ range $ length $ xs !! 0

findMostDigit :: Int -> [Int] -> [Int]
findMostDigit _ [] = []
findMostDigit n (x:xs) = (if x <= n `div` 2 then 0 else 1) : findMostDigit n xs

day3 :: IO Int
day3 = do
  diagnostics <- getDay3Input
  let n = length diagnostics
  let gamma = findMostDigit n $ sumLists diagnostics
  return $ fromBinary gamma * (fromBinary $ notBinary gamma)

-- >>> day3

findMostDigitNPlace :: Int -> [[Int]] -> Int
findMostDigitNPlace i xs = if 2*x >= length xs then 1 else 0 where x = sum $ map (!! i) xs

findLeastDigitNPlace :: Int -> [[Int]] -> Int
findLeastDigitNPlace i xs = if 2*x < length xs then 1 else 0 where x = sum $ map (!! i) xs

foldListBits :: [[Int]] -> Bool -> [Int]
foldListBits xs most = head $ foldl (\acc n -> if length acc == 1 then acc else filter (\l -> (l !! n) == ((if most then findMostDigitNPlace else findLeastDigitNPlace) n acc)) acc) xs $ range $ length $ head xs

day3Part2 :: IO Int
day3Part2 = do
  diagnostics <- getDay3Input
  let oxygen = fromBinary $ foldListBits diagnostics True
  let carbon = fromBinary $ foldListBits diagnostics False
  return $ oxygen * carbon

-- >>> day3Part2

main :: IO ()
main = do
  mapM_ (>>= print) [day1, day1Part2, day2, day2Part2, day3, day3Part2]

-- >>> main
