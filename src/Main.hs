{-# LANGUAGE LambdaCase #-}

module Main(main) where

import           AST
import           Data.List          (intercalate)
import           Eval               (evalAST)
import           Parse              (parseAST)
import           System.Environment (getArgs)
import           Text.Parsec        (ParseError)


main :: IO ()
main = dealWithArgs =<< getArgs

dealWithArgs :: [String] -> IO ()
dealWithArgs []            = putStrLn "Usage: ./whenever [--ast] input_file.we"
dealWithArgs ("--ast":x:_) = parseAST x >>= \case
    (Right ast) -> putStrLn . (intercalate "\n\n") . map show $ ast
    (Left  err) -> print   err
dealWithArgs (x:_)         = parseAST x >>= \case
    (Right ast) -> evalAST ast
    (Left  err) -> print   err
