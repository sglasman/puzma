module Generate where

import           Data

generate :: Puzzle -> String
generate puzzle = "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"> " ++ -- svg boilerplate
                  "<rect x=\"" ++ show d ++ "\" y=\"" ++ show d ++ "\" width=\"" ++ show (d * n) ++ "\" height=\"" ++ show (d * m) ++ "\" stroke=\"black\" stroke-width=\"4\" fill=\"none\"/>" ++ -- outer rectangle
                  ([1..(m-1)] >>= (\i -> line (0, i) (n, i) 1 d)) ++ -- horizontal grid lines
                  ([1..(n-1)] >>= (\j -> line (j, 0) (j, m) 1 d)) ++ -- vertical grid lines
                  "</svg>"
                  where n = width $ grid puzzle
                        m = height $ grid puzzle
                        d = gridsize $ grid puzzle

line :: Coord -> Coord -> Int -> Int -> String -- draw a line from grid coordinates (x1, y1) to (x2, y2) of width w, supply the grid size d
line (x1, y1) (x2, y2) w d = "<line x1=\"" ++ show (d * (x1 + 1)) ++ "\" x2=\"" ++ show (d * (x2 + 1)) ++
                             "\" y1=\"" ++ show (d * (y1 + 1)) ++ "\" y2 = \"" ++ show (d * (y2 + 1)) ++
                             "\" stroke=\"black\" stroke-width=\"" ++ show w ++ "\"/>"
                             where d' = fromIntegral d
