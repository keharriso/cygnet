{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.List (isPrefixOf, partition)
import Data.Text.IO qualified as Text.IO

import System.Environment (getArgs)

import Cygnet.AST (Module (..))
import Cygnet.Compiler (CompilerOptions (CompilerOptions, includeDirs), compile)
import Cygnet.Parser (parseCygnet)
import Data.Foldable (traverse_)
import Data.Map qualified as Map

main :: IO ()
main = do
    args <- getArgs

    let (flags, fileArgs) = partition (isPrefixOf "-") args

    let file = case fileArgs of
            [path] -> path
            _ -> error "Expected a single input file argument"

    let opts =
            CompilerOptions
                { includeDirs = map (drop 2) (filter (isPrefixOf "-I") flags)
                }

    code <- readFile file

    let unit = case parseCygnet file "main" code of
            Right parsed -> parsed
            Left err -> error $ show err

    if "-P" `elem` flags
        then let symbols = Map.elems $ moduleSymbols unit
              in putStrLn "" >> traverse_ (\symbol -> print symbol >> putStrLn "") symbols
        else compile opts unit >>= Text.IO.putStr
