main :: IO ()
main = do
    n <- fmap read getLine
    print $ if odd n
        then 0
        else expressions !! (n `div` 2)

bracketCount = 4

expressions :: [Integer]
expressions = 1 : map e' [1..]
    where e' n = sum $ flip map [1..n] $ \i -> (whole !! i) * (expressions !! (n - i))

whole :: [Integer]
whole = 0 : map (bracketCount*) expressions
