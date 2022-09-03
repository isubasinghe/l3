module CLI where 
import Options.Applicative 

data CLIOptions = CLIOptions {file :: String }
  deriving (Show, Eq)

cliOptions :: Parser CLIOptions 
cliOptions = 
  CLIOptions 
    <$> argument str (metavar "FILE")

