{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Monad (when)

import Data.Foldable (traverse_)
import Data.List (isPrefixOf, partition)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Text.IO qualified as Text.IO

import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)

import Cygnet.AST (Module (..))
import Cygnet.Compiler (CompilerOptions (..), compile)
import Cygnet.Parser (parseCygnet)

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
                , verbose = "-v" `elem` flags
                }

    code <- readFile file

    let unit = case parseCygnet file "main" code of
            Right parsed -> parsed
            Left err -> error $ show err

    if "-P" `elem` flags
        then
            let symbols = Map.elems $ moduleSymbols unit
             in putStrLn "" >> traverse_ (\symbol -> print symbol >> putStrLn "") symbols
        else doCompile opts unit

doCompile :: CompilerOptions -> Module -> IO ()
doCompile opts unit = do
    (_, info) <- readCommand "clang" ["-v"]
    when (verbose opts) (putStrLn info)
    sysIncludeDirs <- getSystemIncludeDirs opts
    when (verbose opts) (traverse_ putStrLn sysIncludeDirs)
    let opts' = opts{includeDirs = sysIncludeDirs ++ includeDirs opts}
    compile opts' unit >>= Text.IO.putStr

readCommand :: FilePath -> [String] -> IO (String, String)
readCommand cmd args = do
    (ret, out, err) <- readProcessWithExitCode cmd args ""
    if ret /= ExitSuccess
        then error $ "Failed to read command: \"" ++ cmd ++ "\" " ++ show args
        else return (out, err)

getSystemIncludeDirs :: CompilerOptions -> IO [FilePath]
getSystemIncludeDirs _ = do
    (_, flags) <- withSystemTempFile "cygtmp.c" (\path _ -> readCommand "clang" ["-###", path])
    let flags' = splitOn "\" \"" flags
    return $ parseIncludeDirs flags'
  where
    parseIncludeDirs args =
        case args of
            ("-internal-isystem" : dir : args') -> cleanDir dir : parseIncludeDirs args'
            (_ : args') -> parseIncludeDirs args'
            [] -> []
    cleanDir dir =
        case dir of
            ('\\' : escaped : dir') -> escaped : cleanDir dir'
            (c : dir') -> c : cleanDir dir'
            [] -> []
