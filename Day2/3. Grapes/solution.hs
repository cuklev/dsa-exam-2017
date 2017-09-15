main = do
    n <- fmap read getLine
    print $ grapes !! n

grapes = 1 : map g' [1..]
    where g' n = sum $ map (\i -> grapes !! i * grapes !! (n - 1 - i)) [0..n-1]
