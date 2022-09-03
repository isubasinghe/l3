module Main where

import CLI
import qualified Data.Text.IO as TIO
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))

version :: String
version = "0.1.0"

main :: IO ()
main = runCompiler =<< execParser opts
  where
    opts =
      info
        (cliOptions <**> helper)
        ( fullDesc
            <> progDesc ("Version: " ++ version)
            <> header "A compiler for a LISP like language (L3)"
        )

runCompiler :: CLIOptions -> IO ()
runCompiler c = do
  let f = file c
  inp <- TIO.readFile f
  TIO.putStrLn inp
