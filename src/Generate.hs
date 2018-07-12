module Generate where

import Data

genPuzzle :: Puzzle -> String
genPuzzle puzzle = "<svg xmlns=\"http://www.w3.org/2000/svg\" " ++ -- svg boilerplate
                   "viewBox=\"" ++ genViewBox puzzle ++ "\">" ++
                   (genGrid $ mGrid puzzle) ++
                   (mObjects puzzle >>= genClue (gridsize $ mGrid puzzle)) ++
                   "</svg>"


genGrid :: Grid -> String
genGrid grid = "<rect x=\"0\" y=\"0\" width=\"" ++ show (d * n) ++ "\" height=\"" ++ show (d * m) ++ "\" stroke=\"black\" stroke-width=\"4\" fill=\"none\"/>" ++ -- outer rectangle
               ([1..(m-1)] >>= (\i -> gridLine (0, i) (n, i) 1 d)) ++ -- horizontal grid lines
               ([1..(n-1)] >>= (\j -> gridLine (j, 0) (j, m) 1 d)) -- vertical grid lines
               where n = width grid
                     m = height  grid
                     d = gridsize grid

genClue :: Int -> Object -> String
genClue d clue = let (x, y) = coordTransform (location clue) d
                     e = quot d 2
                 in  "<text x=\"" ++ show (x - e) ++ "\" y=\"" ++ show (y - e) ++
                     "\" text-anchor=\"middle\" dy=\"" ++ show (quot d 4) ++ "\" style=\"font: " ++
                     show (2 * (quot d 3)) ++ "px helvetica;\">" ++ content clue ++ "</text>"
                     -- font size is 2/3 of grid size


gridLine :: GridCoord -> GridCoord -> Int -> Int -> String -- draw a line from grid coordinates (x1, y1) to (x2, y2) of width w, supply the grid size d
gridLine start end w d = pixLine (coordTransform start d) (coordTransform end d) w

pixLine :: PixelCoord -> PixelCoord -> Int -> String -- draw a line from pixel coordinates (x1, y1) to (x2, y2) of width w
pixLine (x1, y1) (x2, y2) w = "<line x1=\"" ++ show x1 ++ "\" x2=\"" ++ show x2 ++
                              "\" y1=\"" ++ show y1 ++ "\" y2 = \"" ++ show y2 ++
                              "\" stroke=\"black\" stroke-width=\"" ++ show w ++ "\"/>"

coordTransform :: GridCoord -> Int -> PixelCoord
coordTransform (x, y) d = (d * x, d * y)

genViewBox :: Puzzle -> String
genViewBox puzzle = let coords = map location $ mObjects puzzle
                        minX = minimum $ map fst coords
                        minY = minimum $ map snd coords
                        maxX = maximum $ map fst coords
                        maxY = maximum $ map snd coords
                        m = height $ mGrid puzzle
                        n = width $ mGrid puzzle
                        d = gridsize $ mGrid puzzle
                    in show ((min minX (-1)) * d) ++ " " ++
                       show ((min minY (-1)) * d) ++ " " ++
                       show ((max (maxX - minX + 2) (n + 2)) * d) ++ " " ++
                       show ((max (maxY - minY + 2) (m + 2)) * d)
