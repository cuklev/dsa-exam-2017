main = do
    digits <- getLine
    expected <- fmap read getLine

    let exps = expressions digits
        valid = [ exp | (result, exp) <- exps, result == expected ]

    print $ length valid
    putStr $ unlines valid

expressions :: String -> [(Int, String)]
expressions (d:ds) = append [d] $ exp' ds False (readDigit d) 0 1
    where
        readDigit :: Char -> Int
        readDigit d = fromEnum d - fromEnum '0'

        append :: String -> [(Int, String)] -> [(Int, String)]
        append prefix = map (\(r, e) -> (r, prefix ++ e))

        exp' :: String -> Bool -> Int -> Int -> Int -> [(Int, String)]
        exp' [] negative currentNumber currentSum currentProduct =
            let result = if negative
                            then currentSum - currentProduct * currentNumber
                            else currentSum + currentProduct * currentNumber
            in [(result, "")]

        exp' (d:ds) negative currentNumber currentSum currentProduct =
            let digitValue = readDigit d

                newProduct = currentProduct * currentNumber
                newSum = if negative
                            then currentSum - newProduct
                            else currentSum + newProduct

                noSign = if currentNumber == 0
                            then []
                            else let newNumber = currentNumber * 10 + digitValue
                                 in append [d] $ exp' ds negative newNumber currentSum currentProduct

                [addSign, subSign] = [ append [sign, d] $ exp' ds neg digitValue newSum 1
                                       | (sign, neg) <- [ ('+', False)
                                                        , ('-', True)
                                                        ]
                                     ]

                mulSign = append ['*', d] $ exp' ds negative digitValue currentSum newProduct

            in addSign ++ subSign ++ mulSign ++ noSign
