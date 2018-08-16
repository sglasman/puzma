module Parser where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.Char

import Data
import Utility

type Parser a = Parsec String () a

puzma :: Parser Puzzle
puzma = do
        grid <- gridP
        layoutObjects <- concat <$> (many $ try layoutP)
        otherObjects <- many $ try objectP
        spaces >> eof
        return Puzzle { puzzleGrid = grid,
                        puzzleObjects = layoutObjects ++ otherObjects }

-- Grid parsers

gridP :: Parser Grid
gridP = (try rectangleGridP) <|> (try sudokuGridP) <|> slitherlinkGridP

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

slitherlinkGridP :: Parser Grid
slitherlinkGridP = do
                   spaces >> string "SlitherlinkGrid" >> spaces >> char '{'
                   gridProperties <- sepBy propertyP (try $ spaces >> char ',')
                   spaces >> char '}'
                   return Slitherlink { height = maybe (error "Error: grid has undefined height") read $ lookup "height" gridProperties,
                                        width = maybe (error "Error: grid has undefined width") read $ lookup "width" gridProperties,
                                        gridsize = maybe 36 read $ lookup "gridsize" gridProperties }

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
layoutP = try rowColLayoutP <|> try gridLayoutP <|> thickLineLayoutP

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
              return . concat $ zipWith (zipWith buildLocatedClue)
                                (coordArray (length layoutRows) (maximum $ map length layoutRows))
                                layoutRows

thickLineLayoutP :: Parser [Object]
thickLineLayoutP = do
                   spaces >> string "ThickLineLayout" >> spaces >> char '['
                   rows <- sepBy lineLayoutRowP (try $ spaces >> char '|')
                   spaces >> char ']'
                   let zippedRows = zip
                                    (interleave (map normalizeInterstitial [0..]) (map normalizeInterstitial [1..]))
                                    rows -- :: [(Int, String)]
                   return (zippedRows >>= thickLineLayoutObjectProducer) -- This doesn't check for enough syntactic correctness of the ThickLineLayout, but it'll do for now

thickLineLayoutObjectProducer :: (Int, String) -> [Object]
thickLineLayoutObjectProducer (rownumber, s) = zip (map normalizeInterstitial [0..]) s >>=
                                               (\c -> case c of
                                                           (colnumber, '_') -> []
                                                           (colnumber, 'i') -> [LineObject $ Line {
                                                                                             lineEndpoints = LineEndpoints (colnumber + 2, rownumber)
                                                                                                                           (colnumber + 2, rownumber + 2),
                                                                                             lineThickness = 4,
                                                                                             linestyle = NormalLinestyle
                                                                                             }
                                                                               ]
                                                           (colnumber, '-') -> [LineObject $ Line {
                                                                                             lineEndpoints = LineEndpoints (colnumber, rownumber)
                                                                                                                           (colnumber + 2, rownumber),
                                                                                             lineThickness = 4,
                                                                                             linestyle = NormalLinestyle
                                                                                             }
                                                                               ]
                                               )

lineLayoutRowP :: Parser String
lineLayoutRowP = spaces >> endBy (oneOf ['_', 'i', '-'])
                                      (try (spaces >> optional (char ',')))

layoutListP :: Parser [Clue]
layoutListP = (try (endBy layoutClueP (try (spaces >> optional (char ','))))) <|> (spaces >> return []) -- the last option allows for empty rows in a layout

layoutClueP :: Parser Clue
layoutClueP = (try (spaces >> char '#' >> return ShadedCell)) <|>
              (try (spaces >> char '_' >> return EmptyCell)) <|>
              (try (spaces >> (BasicClue . return <$> noneOf ['|', ',', '#', '{', '}', '[', ']', '_']))) <|>
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
clueP = (try unshadedCircleP) <|> (try shadedCircleP) <|> (try tapaClueP) <|> (try battleship1P) <|> (try battleshipMiddleP) <|>
        (try battleshipLeftEndP) <|> (try battleshipRightEndP) <|> (try battleshipBottomEndP) <|> (try battleshipTopEndP) <|>
        (try shadedClueP) <|> (try shadedCellP) <|> (try smallClueP) <|> basicClueP

tapaClueP :: Parser Clue
tapaClueP = do
            spaces >> ((try $ string "TapaClue") <|> string "TC")>> spaces >> char '{' >> spaces
            tapaClueList <- endBy (many1 $ satisfy (\c -> not (elem c "}," || isSpace c)))
                                  (try $ spaces >> optional (char ',') >> spaces)
            char '}'
            return $ case tapaClueList of
                                       [n] -> BasicClue n
                                       [n1, n2] -> Tapa2Clue n1 n2
                                       [n1, n2, n3] -> Tapa3Clue n1 n2 n3
                                       [n1, n2, n3, n4] -> Tapa4Clue n1 n2 n3 n4
                                       _ -> error "Empty or overfull Tapa clue"

battleship1P :: Parser Clue
battleship1P = spaces >> ((try $ string "Battleship1") <|> string "B1") >> return Battleship1

battleshipMiddleP :: Parser Clue
battleshipMiddleP = spaces >> ((try $ string "BattleshipMiddle") <|> string "BM") >> return BattleshipMiddle

battleshipLeftEndP :: Parser Clue
battleshipLeftEndP = spaces >> ((try $ string "BattleshipLeftEnd") <|> string "BLE") >> return BattleshipLeftEnd

battleshipRightEndP :: Parser Clue
battleshipRightEndP = spaces >> ((try $ string "BattleshipRightEnd") <|> string "BRE") >> return BattleshipRightEnd

battleshipBottomEndP :: Parser Clue
battleshipBottomEndP = spaces >> ((try $ string "BattleshipBottomEnd") <|> string "BBE") >> return BattleshipBottomEnd

battleshipTopEndP :: Parser Clue
battleshipTopEndP = spaces >> ((try $ string "BattleshipTopEnd") <|> string "BTE") >> return BattleshipTopEnd

shadedClueP :: Parser Clue
shadedClueP = do
              spaces >> (string "Shaded" <|> string "#")
              clue <- (try (spaces >> char '{' >> clueP <* spaces <* char '}')) <|> basicClueP
              return $ ShadedClue clue

smallClueP :: Parser Clue
smallClueP = do
             spaces >> (string "Small") >> spaces >> char '{'
             content <- many $ noneOf ['}']
             char '}'
             return $ SmallClue content

basicClueP :: Parser Clue
basicClueP = do
             spaces >> optional (string "Clue") >> spaces >> char '{'
             content <- many $ noneOf ['}']
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
               
notSpace :: Parser Char
notSpace = satisfy (not . isSpace)

