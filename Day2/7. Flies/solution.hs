import Text.Printf

newtype Vector = Vector (Double, Double)

xOf :: Vector -> Double
xOf (Vector (x, _)) = x

yOf :: Vector -> Double
yOf (Vector (_, y)) = y

(@+) :: Vector -> Vector -> Vector
v1 @+ v2 = Vector (xOf v1 + xOf v2, yOf v1 + yOf v2)

(@-) :: Vector -> Vector -> Vector
v1 @- v2 = Vector (xOf v1 - xOf v2, yOf v1 - yOf v2)

(@/) :: Vector -> Double -> Vector
v @/ d = Vector (xOf v / d, yOf v / d)

(@.) :: Vector -> Vector -> Double
v1 @. v2 = xOf v1 * xOf v2 + yOf v1 * yOf v2

fromString :: String -> Vector
fromString str = let parts = words str
                     pp = map read parts
                     [x, y] = pp
                 in Vector (x, y)

instance Show Vector where
    show v = printf "%.4f %.4f" (xOf v) (yOf v)

main = do
    [pointA, pointB, pointC] <- fmap (map fromString) $ sequence $ replicate 3 getLine

    let ab  = pointB @- pointA
        ab2 = ab @. ab / 2
        ac  = pointC @- pointA
        ac2 = ac @. ac / 2

        at = Vector
            ( ac2 * yOf ab - ab2 * yOf ac
            , ab2 * xOf ac - ac2 * xOf ab
            )
            @/ (yOf ab * xOf ac - xOf ab * yOf ac)

        pointT = pointA @+ at

    print pointT
