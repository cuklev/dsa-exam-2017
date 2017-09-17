import Data.List (sort)
import Data.Map (singleton, alter, update, Map, findMax)

parseCarpet :: String -> (Int, Int, Int)
parseCarpet str = let [l, r, h] = map read $ words str
                  in (l, r, h)

main = do
    n <- fmap read getLine
    carpets <- sequence $ replicate n $ fmap parseCarpet getLine

    let sweep = sort $ concatMap (\(l, r, h) -> [(l, h, True), (r, h, False)]) carpets
        heights = singleton 0 1

        result = removeSameY $ removeSameX $ solve sweep heights

    putStr $ unlines $ map (\(x, y) -> show x ++ " " ++ show y) result

solve :: [(Int, Int, Bool)] -> Map Int Int -> [(Int, Int)]
solve [] _ = []
solve ((x, h, isLeft) : xs) heights = let heights' = if isLeft
                                            then alter (Just . maybe 1 (+1)) h heights
                                            else update (\x -> if x == 1 then Nothing else Just $ x - 1) h heights

                                          y = fst $ findMax heights'
                                      in (x, y) : solve xs heights'

removeSameX :: [(Int, Int)] -> [(Int, Int)]
removeSameX [] = []
removeSameX [p] = [p]
removeSameX (p:p2:ps)
    | fst p == fst p2 = removeSameX (p2:ps)
    | otherwise       = p : removeSameX (p2:ps)

removeSameY :: [(Int, Int)] -> [(Int, Int)]
removeSameY [] = []
removeSameY [p] = [p]
removeSameY (p:p2:ps)
    | snd p == snd p2 = removeSameY (p:ps)
    | otherwise       = p : removeSameY (p2:ps)
