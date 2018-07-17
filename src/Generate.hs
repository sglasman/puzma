module Generate where

import Data

genPuzzle :: Puzzle -> String
genPuzzle puzzle = "<svg xmlns=\"http://www.w3.org/2000/svg\" " ++ -- svg boilerplate
                   "viewBox=\"" ++ genViewBox puzzle ++ "\">" ++
                   (genGrid $ puzzleGrid puzzle) ++
                   (puzzleObjects puzzle >>= genObject (gridsize $ puzzleGrid puzzle)) ++
                   "</svg>"

genGrid :: Grid -> String
genGrid (Rectangle m n d) = "<rect x=\"0\" y=\"0\" width=\"" ++ show (d * n) ++ "\" height=\"" ++ show (d * m) ++ "\" stroke=\"black\" stroke-width=\"4\" fill=\"none\"/>" ++ -- outer rectangle
                            ([1..(m-1)] >>= (\i -> gridLine (0, 2 * i) (2 * n, 2 * i) 1 d)) ++ -- horizontal grid lines
                            ([1..(n-1)] >>= (\j -> gridLine (2 * j, 0) (2 * j, 2 * m) 1 d)) -- vertical grid lines
genGrid (Sudoku d) = genGrid (Rectangle 9 9 d) ++
                     gridLine (0, 6) (18, 6) 4 d ++ gridLine (0, 12) (18, 12) 4 d ++
                     gridLine (6, 0) (6, 18) 4 d ++ gridLine (12, 0) (12,18) 4 d

genObject :: Int -> Object -> String
genObject d (LocatedClueObject locatedClueObject) = genLocatedClue d locatedClueObject
genObject d (LineObject (Line (LineEndpoints start end) thickness)) = gridLine start end thickness d
genObject _ _ = ""

genLocatedClue :: Int -> LocatedClue -> String
genLocatedClue d (LocatedClue (BasicClue content) location) = let (x, y) = coordTransform location d
                                                              in  "<text x=\"" ++ show x ++ "\" y=\"" ++ show y ++
                                                              "\" text-anchor=\"middle\" dy=\"" ++ show (quot d 4) ++ "\" style=\"font: " ++ -- positioning the clue 1/4 of the way down the cell seems to look nice
                                                              show (2 * (quot d 3)) ++ "px helvetica;\">" ++ content ++ "</text>" -- font size is 2/3 of grid size
genLocatedClue d (LocatedClue ShadedCell location) = let (x, y) = coordTransform location d
                                                         e = quot d 2
                                                     in "<rect x=\"" ++ show (x - e) ++ "\" y=\"" ++ show (y - e) ++ "\" width=\"" ++ show d ++ "\" height=\"" ++ show d ++ "\"/>"

gridLine :: GridCoord -> GridCoord -> Int -> Int -> String -- draw a line from grid coordinates (x1, y1) to (x2, y2) of width w, supply the grid size d
gridLine start end w d = pixLine (coordTransform start d) (coordTransform end d) w

pixLine :: PixelCoord -> PixelCoord -> Int -> String -- draw a line from pixel coordinates (x1, y1) to (x2, y2) of width w
pixLine (x1, y1) (x2, y2) w = "<line x1=\"" ++ show x1 ++ "\" x2=\"" ++ show x2 ++
                              "\" y1=\"" ++ show y1 ++ "\" y2 = \"" ++ show y2 ++
                              "\" stroke=\"black\" stroke-linecap=\"square\" stroke-width=\"" ++ show w ++ "\"/>"

coordTransform :: GridCoord -> Int -> PixelCoord
coordTransform (x, y) d = ((d * x) `quot` 2, (d * y) `quot` 2)

genViewBox :: Puzzle -> String
genViewBox puzzle = let coords = puzzleObjects puzzle >>= objectCoords
                        minX = safeMinimum $ map fst coords
                        minY = safeMinimum $ map snd coords
                        maxX = safeMaximum $ map fst coords
                        maxY = safeMaximum $ map snd coords
                        m = gridHeight $ puzzleGrid puzzle
                        n = gridWidth $ puzzleGrid puzzle
                        d = gridsize $ puzzleGrid puzzle
                    in show (quot ((min (minX - 3) (-2)) * d) 2) ++ " " ++ -- using -3 here enforces a 1-cell margin
                       show (quot ((min (minY - 3) (-2)) * d) 2) ++ " " ++
                       show (quot (maximum [
                                            maxX - minX + 6, 2 * n + 4, 2 * n - minX + 4, maxX + 6
                                           ] * d) 2) ++ " " ++
                       show (quot (maximum [
                                            maxY - minY + 6, 2 * n + 4, 2 * n - minY + 4, maxY + 6
                                           ] * d) 2)

objectCoords :: Object -> [GridCoord]
objectCoords (LocatedClueObject (LocatedClue _ location)) = [location]
objectCoords (LineObject (Line (LineEndpoints start end) _)) = [start, end]

gridHeight :: Grid -> Int
gridHeight (Rectangle m _ _) = m
gridHeight (Sudoku _) = 9 -- other kinds of grids might go here eventually, so this function isn't as silly as it looks

gridWidth :: Grid -> Int
gridWidth (Rectangle _ n _) = n
gridWidth (Sudoku _) = 9

safeMinimum :: (Ord a, Num a) => [a] -> a
safeMinimum [] = 1
safeMinimum l = minimum l

safeMaximum :: (Ord a, Num a) => [a] -> a
safeMaximum [] = 1
safeMaximum l = minimum l

