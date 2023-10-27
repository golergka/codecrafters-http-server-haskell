module CLIOptions
  ( CLIOptions (..),
    cliOptions,
  )
where

import Options.Applicative

data CLIOptions = CLIOptions {directory :: Maybe String}

cliOptions :: Parser CLIOptions
cliOptions =
  CLIOptions
    <$> optional
      ( strOption
          ( long "directory"
              <> short 'd'
              <> metavar "DIRECTORY"
              <> help "Directory to serve at file endpoint"
          )
      )