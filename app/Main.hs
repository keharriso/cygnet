{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.List (isPrefixOf, partition)
import Data.Text.IO qualified as Text.IO

import System.Environment (getArgs)

import Cygnet.Compiler (CompilerOptions (CompilerOptions, includeDirs), compile)
import Cygnet.Parser (parseCygnet)

main :: IO ()
main = do
    args <- getArgs

    let (includeArgs, fileArgs) = partition (isPrefixOf "-I") args

    let file = case fileArgs of
            [path] -> path
            _ -> error "Expected a single input file argument"

    let opts =
            CompilerOptions
                { includeDirs = map (drop 2) includeArgs
                }

    code <- readFile file

    let unit = case parseCygnet file "main" code of
            Right parsed -> parsed
            Left err -> error $ show err

    compiled <- compile opts unit
    Text.IO.putStr compiled