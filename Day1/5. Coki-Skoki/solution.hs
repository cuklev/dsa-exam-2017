main = do
    _ <- getLine
    line <- getLine
    let nums = map read $ words line
        (maxResult, results, _) = foldr foldFunc (-1, [-1], []) nums
    print maxResult
    putStrLn $ unwords $ map show $ init results

type FoldResults = (Int, [Int], [Int])

foldFunc :: Int -> FoldResults -> FoldResults
foldFunc n (m, cs, s) = let (dropped, newStack') = countDrop (>n) s
                            newStack = n : newStack'
                            newCount = head cs - dropped + 1
                            newMax = max m newCount
                        in (newMax, newCount:cs, newStack)

countDrop :: (a -> Bool) -> [a] -> (Int, [a])
countDrop _ [] = (0, [])
countDrop f all@(x:xs)
    | f x       = (0, all)
    | otherwise = let (c, ys) = countDrop f xs
                  in (c + 1, ys)
