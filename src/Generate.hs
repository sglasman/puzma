module Generate where

import Data

genPuzzle :: Puzzle -> String
genPuzzle puzzle = "<svg xmlns=\"http://www.w3.org/2000/svg\" " ++ -- svg boilerplate
                   "viewBox=\"" ++ genViewBox puzzle ++ "\">" ++
                   (genGrid $ puzzleGrid puzzle) ++
                   (puzzleObjects puzzle >>= genClue (gridsize $ puzzleGrid puzzle)) ++
                   "</svg>"


genGrid :: Grid -> String
genGrid grid = "<rect x=\"0\" y=\"0\" width=\"" ++ show (d * n) ++ "\" height=\"" ++ show (d * m) ++ "\" stroke=\"black\" stroke-width=\"4\" fill=\"none\"/>" ++ -- outer rectangle
               ([1..(m-1)] >>= (\i -> gridLine (0, 2 * i) (2 * n, 2 * i) 1 d)) ++ -- horizontal grid lines
               ([1..(n-1)] >>= (\j -> gridLine (2 * j, 0) (2 * j, 2 * m) 1 d)) -- vertical grid lines
               where n = width grid
                     m = height  grid
                     d = gridsize grid

genClue :: Int -> Object -> String
genClue d clue = let (x, y) = coordTransform (location clue) d
                 in  "<text x=\"" ++ show x ++ "\" y=\"" ++ show y ++
                     "\" text-anchor=\"middle\" dy=\"" ++ show (quot d 4) ++ "\" style=\"font: " ++ -- positioning the clue 1/4 of the way down the cell seems to look nice
                     show (2 * (quot d 3)) ++ "px helvetica;\">" ++ content clue ++ "</text>" -- font size is 2/3 of grid size


gridLine :: GridCoord -> GridCoord -> Int -> Int -> String -- draw a line from grid coordinates (x1, y1) to (x2, y2) of width w, supply the grid size d
gridLine start end w d = pixLine (coordTransform start d) (coordTransform end d) w

pixLine :: PixelCoord -> PixelCoord -> Int -> String -- draw a line from pixel coordinates (x1, y1) to (x2, y2) of width w
pixLine (x1, y1) (x2, y2) w = "<line x1=\"" ++ show x1 ++ "\" x2=\"" ++ show x2 ++
                              "\" y1=\"" ++ show y1 ++ "\" y2 = \"" ++ show y2 ++
                              "\" stroke=\"black\" stroke-width=\"" ++ show w ++ "\"/>"

coordTransform :: GridCoord -> Int -> PixelCoord
coordTransform (x, y) d = ((d * x) `quot` 2, (d * y) `quot` 2)

genViewBox :: Puzzle -> String
genViewBox puzzle = let coords = map location $ puzzleObjects puzzle
                        minX = minimum $ map fst coords
                        minY = minimum $ map snd coords
                        maxX = maximum $ map fst coords
                        maxY = maximum $ map snd coords
                        m = height $ puzzleGrid puzzle
                        n = width $ puzzleGrid puzzle
                        d = gridsize $ puzzleGrid puzzle
                    in show (quot ((min (minX - 3) (-2)) * d) 2) ++ " " ++ -- using -2 here enforces a 1-cell margin
                       show (quot ((min (minY - 3) (-2)) * d) 2) ++ " " ++
                       show (quot (maximum [
                                            maxX - minX + 6, 2 * n + 4, 2 * n - minX + 4, maxX + 6
                                           ] * d) 2) ++ " " ++
                       show (quot (maximum [
                                            maxY - minY + 6, 2 * n + 4, 2 * n - minY + 4, maxY + 6
                                           ] * d) 2)
