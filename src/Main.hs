module Main where

import           BNFC.AbsLatte
import           BNFC.ErrM
import           BNFC.LexLatte
import           BNFC.ParLatte
import           BNFC.PrintLatte
import           BNFC.SkelLatte
import           Data.List
import           SemanticAnalysis
import           System.Environment
import           System.Exit

parse :: String -> Either String Program
parse s =
    case pProgram $ myLexer s of
        Bad e -> Left e
        Ok t -> Right t

main :: IO ()
main = do
    [srcPath, astPath] <- getArgs
    src <-readFile srcPath
    case parse src of
        Left e -> die e
        Right ast -> do
            writeFile astPath (show ast)
            case runSemanticAnalysis ast of
                Left xs -> putStrLn $ unlines $ map show (sort xs)
                Right () -> do
                    putStrLn "OK"
                    --print $ runSemanticAnalysis' ast

