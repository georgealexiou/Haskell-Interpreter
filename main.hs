{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Tokens
import Grammar
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
        handle1 <- openFile "test.txt" ReadMode
        contents :: String <- hGetContents handle1
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
        hClose handle1