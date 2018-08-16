module Generate where

import Data
import Utility

genPuzzle :: Puzzle -> String
genPuzzle puzzle = "<svg xmlns=\"http://www.w3.org/2000/svg\" " ++ -- svg boilerplate
                   "viewBox=\"" ++ genViewBox puzzle ++ "\"><g stroke=\"black\">" ++
                   (genGrid $ puzzleGrid puzzle) ++
                   (puzzleObjects puzzle >>= genObject (gridsize $ puzzleGrid puzzle)) ++
                   "</g></svg>"

genGrid :: Grid -> String
genGrid (Rectangle m n d gridstyle borderstyle) = setProperty "fill" "none"
                                                  (if borderstyle == NormalLinestyle then setProperty "stroke-width" "4" $
                                                                                          genRect (-2, -2) (d * n + 4) (d * m + 4)
                                                                                     else setProperty "stroke-width" "1" .
                                                                                          setProperty "stroke-dasharray" "1, 2" $
                                                                                          genRect (0, 0) (d * n) (d * m)
                                                  ) ++
                                                  ([1..(m-1)] >>= (\i -> genGridLine (0, 2 * i) (2 * n, 2 * i) 1 gridstyle d)) ++ -- horizontal grid lines
                                                  ([1..(n-1)] >>= (\j -> genGridLine (2 * j, 0) (2 * j, 2 * m) 1 gridstyle d)) -- vertical grid lines
genGrid (Sudoku d) = genGrid (Rectangle 9 9 d NormalLinestyle NormalLinestyle) ++
                     genGridLine (0, 6) (18, 6) 4 NormalLinestyle d ++ genGridLine (0, 12) (18, 12) 4 NormalLinestyle d ++
                     genGridLine (6, 0) (6, 18) 4 NormalLinestyle d ++ genGridLine (12, 0) (12, 18) 4 NormalLinestyle d
genGrid (Slitherlink m n d) = concat (coordArrayInterstitial m n) >>=
                              \coord -> let (x, y) = coordTransform coord d
                                        in genRect (x - 2, y - 2) 4 4

genObject :: Int -> Object -> String
genObject d (LocatedClueObject locatedClueObject) = genLocatedClue d locatedClueObject
genObject d (LineObject (Line (LineEndpoints start end) thickness linestyle)) = genGridLine start end thickness linestyle d
genObject _ _ = ""

-- lots of fussy symbol positioning here
genLocatedClue :: Int -> LocatedClue -> String
genLocatedClue d (LocatedClue (BasicClue content) location) = let (x, y) = coordTransform location d
                                                              in placeString x y 0 (quot d 4) (quot (2 * d) 3) content
genLocatedClue d (LocatedClue (SmallClue content) location) = let (x, y) = coordTransform location d
                                                              in placeString x y (quot (-3 * d) 8) (quot (-5 * d) 18) (quot d 6) content
genLocatedClue d (LocatedClue (Tapa2Clue n1 n2) location) = let (x, y) = coordTransform location d
                                                            in placeString x y (quot (-1 * d) 4) 0 (quot d 2) n1 ++
                                                               placeString x y (quot d 4) (quot (3 * d) 8) (quot d 2) n2
genLocatedClue d (LocatedClue (Tapa3Clue n1 n2 n3) location) = let (x, y) = coordTransform location d
                                                               in placeString x y (quot (-1 * d) 4) 0 (quot d 2) n1 ++
                                                                  placeString x y (quot d 4) 0 (quot d 2) n2 ++
                                                                  placeString x y 0 (quot (3 * d) 8) (quot d 2) n3
genLocatedClue d (LocatedClue (Tapa4Clue n1 n2 n3 n4) location) = let (x, y) = coordTransform location d
                                                                  in placeString x y 0 (quot (-1 * d) 16) (quot d 2) n1 ++
                                                                     placeString x y (quot (-1 * d) 4) (quot (3 * d) 16) (quot d 2) n2 ++
                                                                     placeString x y (quot d 4) (quot (3 * d) 16) (quot d 2) n3 ++
                                                                     placeString x y 0 (quot (7 * d) 16) (quot d 2) n4
genLocatedClue d (LocatedClue Battleship1 location) = makeGrey $ genCircle d (coordTransform location d) True
genLocatedClue d (LocatedClue BattleshipMiddle location) = let (x, y) = coordTransform location d
                                                           in (makeGrey . setProperty "stroke-width" "2") $
                                                              genRect (x - quot d 3, y - quot d 3) (quot (2 * d) 3) (quot (2 * d) 3)
genLocatedClue d (LocatedClue BattleshipLeftEnd location) = let (x, y) = coordTransform location d
                                                            in (makeGrey . setProperty "stroke-width" "2")
                                                               (genCircle d (x, y) True ++ genRect (x, y - quot d 3) (quot d 3) (quot (2 * d) 3))
genLocatedClue d (LocatedClue BattleshipRightEnd location) = let (x, y) = coordTransform location d
                                                             in (makeGrey . setProperty "stroke-width" "2")
                                                                (genCircle d (x, y) True ++ genRect (x - quot d 3, y - quot d 3) (quot d 3) (quot (2 * d) 3))
genLocatedClue d (LocatedClue BattleshipTopEnd location) = let (x, y) = coordTransform location d
                                                           in (makeGrey . setProperty "stroke-width" "2")
                                                                 (genCircle d (x, y) True ++  genRect (x - quot d 3, y) (quot (2 * d) 3) (quot d 3))
genLocatedClue d (LocatedClue BattleshipBottomEnd location) = let (x, y) = coordTransform location d
                                                              in (makeGrey . setProperty "stroke-width" "2")
                                                                 (genCircle d (x, y) True ++ genRect (x - quot d 3, y - quot d 3) (quot (2 * d) 3) (quot d 3))
genLocatedClue d (LocatedClue ShadedCell location) = let (x, y) = coordTransform location d
                                                         e = quot d 2
                                                     in genRect (x - e, y - e) d d
genLocatedClue d (LocatedClue (ShadedClue clue) location) = genLocatedClue d (LocatedClue ShadedCell location) ++
                                                            (makeWhite $ genLocatedClue d (LocatedClue clue location))
genLocatedClue d (LocatedClue UnshadedCircle location) = genCircle d (coordTransform location d) False
genLocatedClue d (LocatedClue ShadedCircle location) = genCircle d (coordTransform location d) True
genLocatedClue _ (LocatedClue EmptyCell _) = ""

placeString :: Int -> Int -> Int -> Int -> Int -> String -> String
placeString x y dx dy fontsize content = "<text x=\"" ++ show x ++ "\" y=\"" ++ show y ++
                                         "\" text-anchor=\"middle\" stroke=\"transparent\" dx=\"" ++ show dx ++
                                         "\" dy=\"" ++ show dy ++ "\" style=\"font: " ++
                                         show fontsize ++ "px helvetica;\">" ++ content ++ "</text>"

genCircle :: Int -> GridCoord -> Bool -> String
genCircle d (x, y) shaded = (if shaded then id else (setProperty "fill" "none")) $
                            "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"" ++ show (quot d 3) ++ "\" stroke-width=\"2\"/>"

genRect :: PixelCoord -> Int -> Int -> String
genRect (x, y) w h = "<rect x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\" stroke-linecap=\"square\"/>"

genGridLine :: GridCoord -> GridCoord -> Int -> Linestyle -> Int -> String -- draw a line from grid coordinates (x1, y1) to (x2, y2) of width w and style linestyle, supply the grid size d
genGridLine start end w linestyle d = genPixLine (coordTransform start d) (coordTransform end d) w linestyle

genPixLine :: PixelCoord -> PixelCoord -> Int -> Linestyle -> String -- draw a line from pixel coordinates (x1, y1) to (x2, y2) of width w and style linestyle
genPixLine (x1, y1) (x2, y2) w linestyle = "<line x1=\"" ++ show x1 ++ "\" x2=\"" ++ show x2 ++
                                           "\" y1=\"" ++ show y1 ++ "\" y2 = \"" ++ show y2 ++
                                           "\" stroke-linecap=\"square\" stroke-width=\"" ++ show w ++ "\"" ++
                                           (if linestyle == NormalLinestyle then "" else " stroke-dasharray = \"1, 2\"") ++ "/>"

makeGrey :: String -> String
makeGrey = (setProperty "fill" "grey") . (setProperty "stroke" "grey")

makeWhite :: String -> String
makeWhite = (setProperty "fill" "white") . (setProperty "stroke" "white")

setProperty :: String -> String -> String -> String
setProperty property value input = "<g " ++ property ++ "=\"" ++ value ++ "\">" ++ input ++ "</g>"

coordTransform :: GridCoord -> Int -> PixelCoord
coordTransform (x, y) d = ((d * x) `quot` 2, (d * y) `quot` 2)

-- All functions below this point are for calculating and generating the viewbox

genViewBox :: Puzzle -> String
genViewBox puzzle = let coords = puzzleObjects puzzle >>= objectCoords
                        minX = safeMinimum $ map fst coords
                        minY = safeMinimum $ map snd coords
                        maxX = safeMaximum $ map fst coords
                        maxY = safeMaximum $ map snd coords
                        m = gridHeight $ puzzleGrid puzzle
                        n = gridWidth $ puzzleGrid puzzle
                        d = gridsize $ puzzleGrid puzzle
                    in show (quot ((min (minX - 3) (-2)) * d) 2) ++ " " ++ -- using these constants enforces a 1-cell margin
                       show (quot ((min (minY - 3) (-2)) * d) 2) ++ " " ++
                       show (quot (maximum [
                                            maxX - minX + 6, 2 * n + 4, 2 * n - minX + 5, maxX + 3
                                           ] * d) 2) ++ " " ++
                       show (quot (maximum [
                                            maxY - minY + 6, 2 * m + 4, 2 * m - minY + 5, maxY + 3
                                           ] * d) 2)

objectCoords :: Object -> [GridCoord]
objectCoords (LocatedClueObject (LocatedClue _ location))      = [location]
objectCoords (LineObject (Line (LineEndpoints start end) _ _)) = [start, end]

gridHeight :: Grid -> Int
gridHeight (Rectangle m _ _ _ _) = m
gridHeight (Sudoku _)            = 9
gridHeight (Slitherlink m _ _) = m -- other kinds of grids might go here eventually, so this function isn't as silly as it looks

gridWidth :: Grid -> Int
gridWidth (Rectangle _ n _ _ _) = n
gridWidth (Sudoku _)            = 9
gridWidth (Slitherlink _ n _) = n

safeMinimum :: (Ord a, Num a) => [a] -> a
safeMinimum [] = 1
safeMinimum l  = minimum l

safeMaximum :: (Ord a, Num a) => [a] -> a
safeMaximum [] = 1
safeMaximum l  = maximum l