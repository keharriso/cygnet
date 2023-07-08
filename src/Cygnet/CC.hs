module Cygnet.CC (
    Flags,
    cc,
    capture,
) where

import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcess)

type Flags = [String]

cc :: String -> Flags -> FilePath -> [FilePath] -> IO ()
cc compiler flags dest sources = callProcess compiler $ flags ++ ["-o", dest] ++ sources

capture :: String -> IO String
capture source =
    withSystemTempFile "cygnet.c" $ \sourcePath sourceFile ->
        withSystemTempFile "cygnet.exe" $ \exePath exeFile -> do
            hPutStr sourceFile source
            hClose sourceFile
            hClose exeFile
            cc "clang" [] exePath [sourcePath]
            readProcess exePath [] []
