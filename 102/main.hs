import Data.List.Split (splitOn)

data Point = Point (Int, Int)
    deriving Show

origin :: Point
origin = Point (0, 0)

x (Point (x, _)) = x
y (Point (_, y)) = y

data Triangle = Triangle (Point, Point, Point)
    deriving Show

-- area = 1/2 * abs(xA*yB + xB*yC + xC*yA - xC*yB - xB*yA - xA*yC)
area :: Triangle -> Int
area (Triangle (a, b, c)) = abs $ x a * y b + x b * y c + x c * y a - x c * y b - x b * y a - x a * y c


inside :: Point -> Triangle -> Bool
inside p (Triangle (a, b, c)) = area (Triangle (a, b, p)) + area (Triangle (a, p, c)) + area (Triangle (p, b, c)) == area (Triangle (a, b, c))

main = do
    lines <- getContents
    let xs :: [[Int]]
        xs = map (map read) $ map (splitOn ",") $ words lines
    let triangles = map (\x -> Triangle (Point (x!!0, x!!1), Point (x!!2, x!!3), Point (x!!4, x!!5))) xs

    print $ length $ filter (inside origin) triangles
