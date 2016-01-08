module Main where

import           BNFC.AbsLatte
import           BNFC.ErrM
import           BNFC.LexLatte
import           BNFC.ParLatte
import           BNFC.PrintLatte
import           BNFC.SkelLatte
import           Data.List
import           SemanticAnalysis
import CodeGeneration
import           System.Environment
import           System.Exit

parse :: String -> Either String Program
parse s =
    case pProgram $ myLexer s of
        Bad e -> Left e
        Ok t -> Right t

main :: IO ()
main = do
    [srcPath, astPath, llPath] <- getArgs
    src <-readFile srcPath
    case parse src of
        Left e -> do
            putStrLn "ERROR"
            putStrLn e
        Right ast -> do
            writeFile astPath (show ast)
            case runSemanticAnalysis ast of
                Left xs -> do
                    putStrLn "ERROR"
                    putStrLn $ unlines $ map show (sort xs)
                Right () ->
                    case runCodeGeneration2 ast of
                        Left m -> do
                            putStrLn "ERROR"
                            putStrLn m
                        Right xs -> do
                            putStrLn "OK"
                            putStrLn $ unlines $ map show xs
                            writeFile llPath (unlines $ map show xs)
                            --print $ runCodeGeneration2' ast

