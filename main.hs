{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Tokens
import Grammar
import Evaluator1
import Data.List
import Control.Monad
import System.IO
import System.Environment
import Control.Exception
import System.Exit
import System.IO.Error
import Text.Read
import Control.DeepSeq

main = do
        -- handle ((_) -> putStrLn "Error reading number" )
        input :: String <- readFile "input1.txt"
         -- <- hGetContents handle2
        let inputLists ::[[Int]] = readInputFile input

        contents :: String <- readFile "test.txt"
         -- <- hGetContents handle1
        -- let eVal = (alexScanTokens contents)
        -- catch (do putStrLn (show eVal))
        --       (\err -> error "Could not parse input")
        -- eVal <- (alexScanTokens contents)
        -- case eVal of
        --    Left e -> error "Couldn't parse input"
        --    Right n -> print "Could parse input successfuly"
        let tokens :: [Token] = alexScanTokens contents
        print tokens
        let exps :: Lines = parseCalc tokens 
        print exps
        let kostis :: String = startEvaluationWithInput exps (transpose inputLists)
        print kostis
        -- print $ transpose inputLists 

readInputFile :: String -> [[Int]]
readInputFile input = map f (map words (lines input))

f :: [String] -> [Int]
f = map read