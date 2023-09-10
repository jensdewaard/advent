module Parser (ProgramArgs (ProgramArgs), args) where

import Options.Applicative

data ProgramArgs = ProgramArgs 
    { test :: Bool
    , year :: Int
    , day  :: Int
    }

args :: Parser ProgramArgs
args = ProgramArgs
    <$> switch
        ( long "test" <> short 't' <> help "Run on a test input")
    <*> argument auto
        ( help "The year to run a challenge of" 
        <> metavar "YEAR"
        )
    <*> argument auto
        ( help "The day to run the challenge of"
        <> metavar "DAY"
        )
