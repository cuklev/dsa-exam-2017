main = do
    testCount <- fmap read getLine
    sequence_ $ replicate testCount $ do
        [n, g] <- readNumbers
        numbers <- readNumbers
        print $ solveSequence g numbers

solveSequence :: Int -> [Int] -> Int
solveSequence groups numbers = let left = maximum numbers
                                   right = sum numbers + 1
                                   f s = minGroups s numbers <= groups
                               in binarySearch f left right

minGroups :: Int -> [Int] -> Int
minGroups = g' 0 1
    where g' currentSum groups maxSum [] = groups
          g' currentSum groups maxSum (x:xs)
                | nextSum > maxSum = g' x (groups + 1) maxSum xs
                | otherwise        = g' nextSum groups maxSum xs
                where nextSum = currentSum + x

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch f l r
    | l >= r     = l
    | f middle   = binarySearch f l middle
    | otherwise  = binarySearch f (middle + 1) r
    where middle = (l + r) `div` 2

readNumbers :: IO [Int]
readNumbers = fmap parseNumbers getLine
    where parseNumbers = map read . words
