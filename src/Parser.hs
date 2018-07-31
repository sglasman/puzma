module Parser where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.Char

import Data

type Parser a = Parsec String () a

puzma :: Parser Puzzle
puzma = do
        grid <- gridP
        layoutObjects <- concat <$> (many $ try layoutP)
        otherObjects <- many objectP
        return Puzzle { puzzleGrid = grid,
                        puzzleObjects = layoutObjects ++ otherObjects }

-- Grid parsers

gridP :: Parser Grid
gridP = (try rectangleGridP) <|> sudokuGridP

rectangleGridP :: Parser Grid
rectangleGridP = do
                 spaces >> string "RectangleGrid" >> spaces >> char '{'
                 gridProperties <- sepBy propertyP (try $ spaces >> char ',')
                 spaces >> char '}'
                 return Rectangle { height = maybe (error "Error: grid has undefined height") read $ lookup "height" gridProperties,
                                   width = maybe (error "Error: grid has undefined width") read $ lookup "width" gridProperties,
                                   gridsize = maybe 36 read $ lookup "gridsize" gridProperties,
                                   gridstyle = maybe NormalLinestyle stringToLinestyle $ lookup "gridstyle" gridProperties,
                                   borderstyle = maybe NormalLinestyle stringToLinestyle $ lookup "borderstyle" gridProperties }

sudokuGridP :: Parser Grid
sudokuGridP = do
              spaces >> string "SudokuGrid" >> spaces >> char '{'
              gridProperties <- sepBy propertyP (try $ spaces >> char ',')
              spaces >> char '}'
              return Sudoku { gridsize = maybe 36 read $ lookup "gridsize" gridProperties }

propertyP :: Parser (String, String)
propertyP = do
           spaces
           key <- many1 letter
           spaces >> char ':' >> spaces
           value <- many1 $ noneOf [',', '}', ' ']
           return (key, value)

stringToLinestyle :: String -> Linestyle
stringToLinestyle "normal" = NormalLinestyle
stringToLinestyle "dotted" = DottedLinestyle
stringToLinestyle _ = error "Invalid line style in grid declaration"

-- Layout parsers

layoutP :: Parser [Object]
layoutP = (try rowColLayoutP) <|> gridLayoutP

rowColLayoutP :: Parser [Object]
rowColLayoutP = do
                spaces
                whichLayout <- (string "RowLayout") <|> (string "ColumnLayout")
                rowCoord <- (try centralLeftCoordP) <|> interstitialLeftCoordP
                spaces >> char ',' >> spaces >> char '['
                layoutRow <- layoutListP
                spaces >> char ']' >> spaces
                rightBracket <- (char ')') <|> (char '>')
                let rowCoords = map (\x -> (x, rowCoord))
                                    (map (if rightBracket == ')' then normalizeCentral else normalizeInterstitial)
                                         [1..(length layoutRow)])
                let rowColCoords = map (if whichLayout == "RowLayout" then id else (\(a, b) -> (b, a)))
                                       rowCoords
                return $ zipWith buildLocatedClue rowColCoords layoutRow

gridLayoutP :: Parser [Object]
gridLayoutP = do
              spaces >> string "GridLayout" >> spaces >> char '['
              layoutRows <- sepBy layoutListP (try $ spaces >> char '|')
              spaces >> char ']'
              if (not . allEqual $ map length layoutRows) then (error "Error: layout not rectangular")
                                                          else return $ zipWith buildLocatedClue
                                                                                (concat $ coordArray (length layoutRows) (length $ layoutRows !! 0))
                                                                                (concat layoutRows)

layoutListP :: Parser [Clue]
layoutListP = sepBy layoutClueP (try (spaces >> optional (char ',')))

layoutClueP :: Parser Clue
layoutClueP = (try (spaces >> char '#' >> return ShadedCell)) <|>
              (try (spaces >> char '_' >> return EmptyCell)) <|>
              (try spaces >> (BasicClue . return <$> noneOf ['|', ',', '#', '{', '}', '[', ']', '_'])) <|>
              (do
               spaces >> char '{'
               clue <- (try clueP <|> (BasicClue <$> (many $ satisfy (/= '}'))))
               spaces >> char '}'
               return clue)

buildLocatedClue :: GridCoord -> Clue -> Object
buildLocatedClue coord clue = LocatedClueObject $ LocatedClue clue coord

-- Object parsers

objectP :: Parser Object
objectP = (try $ LocatedClueObject <$> locatedClueP) <|> (LineObject <$> lineP)

locatedClueP :: Parser LocatedClue
locatedClueP = do
               spaces >> string "at"
               clueLocation <- coordinateP
               clue <- clueP
               return LocatedClue { locatedClueClue = clue, locatedClueLocation = clueLocation }

clueP :: Parser Clue
clueP = (try basicClueP) <|> (try shadedClueP) <|> (try unshadedCircleP) <|> (try shadedCircleP) <|> shadedCellP

shadedClueP :: Parser Clue
shadedClueP = do
              spaces >> (string "Shaded" <|> string "#") >> spaces >> char '{'
              clue <- clueP
              spaces >> char '}'
              return $ ShadedClue clue

basicClueP :: Parser Clue
basicClueP = do
             spaces >> optional (string "Clue") >> spaces >> char '{'
             content <- many $ satisfy (/= '}')
             char '}'
             return $ BasicClue content

unshadedCircleP :: Parser Clue
unshadedCircleP = spaces >> string "UnshadedCircle" >> return UnshadedCircle

shadedCircleP :: Parser Clue
shadedCircleP = spaces >> string "ShadedCircle" >> return ShadedCircle

shadedCellP :: Parser Clue
shadedCellP = spaces >> (string "ShadedCell" <|> string "#") >> return ShadedCell

lineP :: Parser Data.Line
lineP = thickLineP

coordinateP :: Parser GridCoord
coordinateP = do
              xValue <- (try centralLeftCoordP <|> interstitialLeftCoordP)
              spaces >> char ','
              yValue <- (try centralRightCoordP <|> interstitialRightCoordP)
              return (xValue, yValue)

centralLeftCoordP :: Parser Int
centralLeftCoordP = do
                    spaces >> char '('
                    normalizeCentral <$> int

interstitialLeftCoordP :: Parser Int
interstitialLeftCoordP = do
                         spaces >> char '<'
                         normalizeInterstitial <$> int

centralRightCoordP :: Parser Int
centralRightCoordP = do
                     spaces
                     val <- normalizeCentral <$> int
                     spaces >> char ')'
                     return val

interstitialRightCoordP :: Parser Int
interstitialRightCoordP = do
                          spaces
                          val <- normalizeInterstitial <$> int
                          spaces >> char '>'
                          return val

thickLineP :: Parser Data.Line
thickLineP = do
             spaces >> string "ThickLine"
             endpoints <- lineEndpointsP
             return $ Line endpoints 4 NormalLinestyle

lineEndpointsP :: Parser LineEndpoints
lineEndpointsP = do
                 spaces >> char '['
                 start <- coordinateP
                 spaces >> char ','
                 end <- coordinateP
                 spaces >> char ']'
                 return $ LineEndpoints start end

normalizeCentral :: Int -> Int
normalizeCentral n = 2 * n - 1

normalizeInterstitial :: Int -> Int
normalizeInterstitial n = 2 * n
               
notSpace :: Parser Char
notSpace = satisfy (not . isSpace)

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:xs) = (x == head xs) && allEqual xs

coordArray :: Int -> Int -> [[GridCoord]]
coordArray height width = map (\x -> [(y, x) | y <- map normalizeCentral [1..width]]) $ map normalizeCentral [1..height]