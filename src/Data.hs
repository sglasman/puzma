module Data where

data Puzzle = Puzzle { puzzleGrid :: Grid,
                       puzzleObjects :: [Object] } deriving (Show)
    
data Grid = Rectangle { height :: Int,
                        width :: Int,
                        gridsize :: Int } |
            Sudoku { gridsize :: Int } deriving (Show)

data Object = LineObject Line | LocatedClueObject LocatedClue deriving (Show)

data LocatedClue = LocatedClue { locatedClueClue :: Clue,
                                 locatedClueLocation :: GridCoord } deriving (Show)

data Clue = BasicClue String |
            ShadedClue Clue |
            ShadedCell |
            EmptyCell  deriving (Show)

data Line = Line { lineEndpoints :: LineEndpoints,
                   lineThickness :: Int } deriving (Show)

data LineEndpoints = LineEndpoints { lineStart :: GridCoord,
                                     lineEnd :: GridCoord } deriving (Show)

type GridCoord = (Int, Int) -- IMPORTANT: Grid coords allow interstitial placement of objects, and thus differ from what you might expect by roughly a factor of 2. A cell is 2 units in this system of measurement!
type PixelCoord = (Int, Int)