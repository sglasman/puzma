module Main where

import Control.Monad
import Test.HUnit
import Text.Parsec
import System.Directory

import Parser
import Generate

main :: IO Counts
main = runTestTT =<< getTests

getTests :: IO Test
getTests = do
           files <- getDirectoryContents "./examples/"
           let names = map (reverse . drop 4 . reverse) $
                       filter ((== "mzp") . take 3 . reverse) files
           test <$> (sequence $ map genTestFromExample names)

genTestFromExample :: String -> IO Test
genTestFromExample name = do
                          eitherActual <- parse puzma "" <$> readFile ("./examples/" ++ name ++ ".pzm")
                          let actual = either (error $ "Parse error on example " ++ name) genPuzzle eitherActual
                          expected <- readFile ("./test/" ++ name ++ "-expected.svg")
                          return $ name ~: (actual ~?= expected)