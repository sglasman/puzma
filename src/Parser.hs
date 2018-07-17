module Parser where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.Char

import Data

type Parser a = Parsec String () a

puzma :: Parser Puzzle
puzma = do
        grid <- gridP
        gridObjects <- many objectP
        return Puzzle { puzzleGrid = grid,
                        puzzleObjects = gridObjects }

gridP :: Parser Grid
gridP = (try rectangleGridP) <|> sudokuGridP

rectangleGridP :: Parser Grid
rectangleGridP = do
                 spaces >> string "RectangleGrid" >> spaces >> char '{'
                 gridProperties <- sepBy propertyP (try $ spaces >> char ',')
                 spaces >> char '}'
                 return Rectangle { height = maybe (error "Error: grid has undefined height") id $ lookup "height" gridProperties,
                                   width = maybe (error "Error: grid has undefined width") id $ lookup "width" gridProperties,
                                   gridsize = maybe 36 id $ lookup "gridsize" gridProperties }

sudokuGridP :: Parser Grid
sudokuGridP = do
              spaces >> string "SudokuGrid" >> spaces >> char '{'
              gridProperties <- sepBy propertyP (try $ spaces >> char ',')
              spaces >> char '}'
              return Sudoku { gridsize = maybe 36 id $ lookup "gridsize" gridProperties }

propertyP :: Parser (String, Int)
propertyP = do
           spaces
           key <- many1 letter
           spaces >> char ':' >> spaces
           value <- read <$> many1 digit
           return (key, value)

objectP :: Parser Object
objectP = (try $ LocatedClueObject <$> locatedClueP) <|> (LineObject <$> lineP)

locatedClueP :: Parser LocatedClue
locatedClueP = do
               spaces >> string "at"
               clueLocation <- coordinateP
               clue <- clueP
               return LocatedClue { locatedClueClue = clue, locatedClueLocation = clueLocation }

clueP :: Parser Clue
clueP = (try basicClueP) <|> shadedSquareP

basicClueP :: Parser Clue
basicClueP = do
             spaces >> string "Clue" >> spaces >> char '{'
             content <- many $ satisfy (/= '}')
             char '}'
             return $ BasicClue content

shadedSquareP :: Parser Clue
shadedSquareP = spaces >> (string "shadedCell" <|> string "#") >> return ShadedCell

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
             spaces >> string "thickLine"
             endpoints <- lineEndpointsP
             return $ Line endpoints 4

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