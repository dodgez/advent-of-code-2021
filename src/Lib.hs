module Lib where

getInput :: String -> IO [String]
getInput file = readFile ("./src/inputs/" ++ file) >>= return.lines

getDay1Input :: IO [Int]
getDay1Input = getInput "day1.txt" >>= return.(map read)

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
getDay2Input = getInput "day2.txt" >>= return.(map words) >>= return.(map (\[dir, n] -> (dir, read n :: Int)))

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

main :: IO ()
main = do
  mapM_ (\f -> f >>= print) [day1, day1Part2, day2, day2Part2]

-- >>> main
