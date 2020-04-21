{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Tokens
import Grammar
import Evaluator
import Data.List
import Control.Monad
import System.IO
import System.Environment
import Control.Exception
import System.Exit
import System.IO.Error
import Text.Read
import Control.DeepSeq

main :: IO()
main = catch main' noParse

main' = do (filename : _ ) <- getArgs
           -- Read code file and generate Tokens and Grammar
           sourceCode :: String <- readFile filename
           let tokens :: [Token] = alexScanTokens sourceCode
           let exps :: Lines = parseCalc tokens 
           
           -- Read input.txt
           input :: String <- getContents
           let inputLists ::[[Int]] = readInputFile input
           
           -- TypeList [ints], TypeLists [[ints]] 
           let runProgram :: Types = startEvaluationWithInput exps (transpose inputLists)

           sequence_ $ (prettyPrint runProgram) >>= (\x -> [putStrLn x] )  

readInputFile :: String -> [[Int]]
readInputFile input = map f (map words (lines input))

f :: [String] -> [Int]
f = map read

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

prettyPrint :: Types -> [String]
prettyPrint (TypeList xs) = pretty where
  pretty = prettyPrint (TypeLists [xs])
prettyPrint (TypeInt x) = pretty where
  pretty = prettyPrint (TypeLists [[x]])
prettyPrint (TypeLists xss) = map (\x -> init $ concatStrings x ) pretty where
  pretty = map (map (show)) $ transpose xss
prettyPrint _ = error "lmao noob"

concatStrings :: [String] -> String
concatStrings (x:xs) = x ++ " " ++ concatStrings xs
concatStrings [] = ""