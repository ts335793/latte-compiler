module Main where

import           BNFC.AbsLatte
import           BNFC.ErrM
import           BNFC.LexLatte
import           BNFC.ParLatte
import           BNFC.PrintLatte
import           BNFC.SkelLatte
import           CodeGeneration
import           Data.List
import           SemanticAnalysis
import           System.Environment
import           System.Exit
import           System.IO

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
            hPutStrLn stderr "ERROR"
            die e
        Right ast -> do
            writeFile astPath (show ast)
            case runSemanticAnalysis ast of
                Left xs -> do
                    hPutStrLn stderr "ERROR"
                    die $ unlines $ map show (sort xs)
                Right () ->
                    case runCodeGeneration ast of
                        Left m -> do
                            hPutStrLn stderr "ERROR"
                            die m
                        Right xs -> do
                            hPutStrLn stderr "OK"
                            writeFile llPath (unlines $ map show xs)
                            --print $ runCodeGeneration' ast

