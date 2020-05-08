module BasicPractice exposing (..)

-- data Point = Point {x :: Float, y :: Float}
-- data Shape =
--   Circle Float Point |
--   Triangle Point Point Point
--
-- calculateArea :: Shape -> Float
-- calculateArea (Circle radius _) = pi * radius * radius
-- calculateArea (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) =
--   abs $ (first + second + third) / 2.0
--   where
--     first = x1 * (y2 - y3)
--     second = x2 * (y3 - y1)
--     third = x3 * (y1 - y2)


-- TODO
type alias Point =
  { x: Float
  , y: Float
  }

type Shape
    = Circle {radius: Float, center: Point}
    | Triangle {p1: Point, p2: Point, p3: Point}

calculateArea : Shape -> Float
calculateArea shape =
    case shape of
        Triangle {p1, p2, p3} ->
            let
                first = p1.x * (p2.y - p3.y)
                second = p2.x * (p3.y - p1.y)
                third = p3.x * (p1.y - p2.y)
            in
                abs ((first + second + third) / 2.0)
        Circle {radius, center} -> pi * radius * radius


-- sumsAndProducts :: [Int] -> Int -> Int -> [Int]
-- sumsAndProducts is j k = do
--   i <- is
--   l <- [i + j, i * j]
--   return $ k - l
--
-- Examples:
-- sumsAndProducts [] 3 5 = []
-- sumsAndProducts [1, 2, 3] 3 10 = [6, 7, 5, 4, 4, 1]
-- sumsAndProducts [7, 3, 2, 9, 8] 2 9 = [0, -5, 4, 3, 5, 5, -2, -9, -1, -7]

listAndThen : (a -> List a) -> List a -> List a
listAndThen listFunc listElems =
    case List.isEmpty listElems of
        True -> []
        False -> List.concatMap listFunc listElems

sumsAndProducts : List Int -> Int -> Int -> List Int
sumsAndProducts is j k =
    listAndThen (\i ->
                     listAndThen (\l -> [k - l]) [i + j, i * j]
                ) is
