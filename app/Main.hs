{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad (when)

import Data.Foldable (traverse_)
import Data.List (intercalate, isPrefixOf, partition)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Text.IO qualified as Text.IO

import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)

import Cygnet.AST (Module (..))
import Cygnet.CC (capture)
import Cygnet.Compiler (CompilerOptions (..), NumericLimits (..), compile)
import Cygnet.Parser (parseCygnet)

main :: IO ()
main = do
    limits <- getNumericLimits

    args <- getArgs

    let (flags, fileArgs) = partition (isPrefixOf "-") args

    let file = case fileArgs of
            [path] -> path
            _ -> error "Expected a single input file argument"

    let opts =
            CompilerOptions
                { includeDirs = map (drop 2) (filter (isPrefixOf "-I") flags)
                , verbose = "-v" `elem` flags
                , numericLimits = limits
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

getNumericLimits :: IO NumericLimits
getNumericLimits = do
    let source =
            intercalate
                "\n"
                [ "#include <limits.h>"
                , "#include <stdio.h>"
                , ""
                , "int main(void)"
                , "{"
                , "    printf(\"%lld\\n\", (long long)CHAR_BIT);"
                , "    printf(\"%lld\\n\", (long long)SCHAR_MIN);"
                , "    printf(\"%lld\\n\", (long long)SCHAR_MAX);"
                , "    printf(\"%lld\\n\", (long long)UCHAR_MAX);"
                , "    printf(\"%lld\\n\", (long long)CHAR_MIN);"
                , "    printf(\"%lld\\n\", (long long)CHAR_MAX);"
                , "    printf(\"%lld\\n\", (long long)MB_LEN_MAX);"
                , "    printf(\"%lld\\n\", (long long)SHRT_MIN);"
                , "    printf(\"%lld\\n\", (long long)SHRT_MAX);"
                , "    printf(\"%lld\\n\", (long long)USHRT_MAX);"
                , "    printf(\"%lld\\n\", (long long)INT_MIN);"
                , "    printf(\"%lld\\n\", (long long)INT_MAX);"
                , "    printf(\"%lld\\n\", (long long)UINT_MAX);"
                , "    printf(\"%lld\\n\", (long long)LONG_MIN);"
                , "    printf(\"%lld\\n\", (long long)LONG_MAX);"
                , "    printf(\"%lld\\n\", (long long)ULONG_MAX);"
                , "    printf(\"%lld\\n\", (long long)LLONG_MIN);"
                , "    printf(\"%lld\\n\", (long long)LLONG_MAX);"
                , "    printf(\"%llu\\n\", (unsigned long long)ULLONG_MAX);"
                , ""
                , "    return 0;"
                , "}"
                ]
    capturedOutput <- capture source
    let [ charBit'
            , scharMin'
            , scharMax'
            , ucharMax'
            , charMin'
            , charMax'
            , mbLenMax'
            , shrtMin'
            , shrtMax'
            , ushrtMax'
            , intMin'
            , intMax'
            , uintMax'
            , longMin'
            , longMax'
            , ulongMax'
            , llongMin'
            , llongMax'
            , ullongMax'
            ] = map read $ lines capturedOutput
    return $
        NumericLimits
            { charBit = charBit'
            , scharMin = scharMin'
            , scharMax = scharMax'
            , ucharMax = ucharMax'
            , charMin = charMin'
            , charMax = charMax'
            , mbLenMax = mbLenMax'
            , shrtMin = shrtMin'
            , shrtMax = shrtMax'
            , ushrtMax = ushrtMax'
            , intMin = intMin'
            , intMax = intMax'
            , uintMax = uintMax'
            , longMin = longMin'
            , longMax = longMax'
            , ulongMax = ulongMax'
            , llongMin = llongMin'
            , llongMax = llongMax'
            , ullongMax = ullongMax'
            }

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
