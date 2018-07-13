module Data where

data Puzzle = Puzzle { puzzleGrid :: Grid,
                       puzzleObjects :: [Object] } deriving (Show)
    
data Grid = Rectangle { height :: Int,
                        width :: Int,
                        gridsize :: Int,
                        thickLines :: [Line]} deriving (Show)
                        
data Object = Object { content :: String,
                     location :: GridCoord } deriving (Show)

data Line = Line { start :: GridCoord,
                   end :: GridCoord } deriving (Show)


type GridCoord = (Int, Int) -- IMPORTANT: Grid coords allow interstitial placement of objects, and thus differ from what you might expect by roughly a factor of 2.
-- A cell is 2 units in this system of measurements!
type PixelCoord = (Int, Int)