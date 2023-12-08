module Parser (ProgramArgs (ProgramArgs), args) where

import Options.Applicative

data ProgramArgs = ProgramArgs 
    { command :: String
    , year :: Integer
    , day  :: Integer
    }

args :: Parser ProgramArgs
args = ProgramArgs
    <$> argument str
        ( help "The command to perform on the challenge"
        <> metavar "CMD"
        )
    <*> argument auto
        ( help "The year to run a challenge of" 
        <> metavar "YEAR"
        )
    <*> argument auto
        ( help "The day to run the challenge of"
        <> metavar "DAY"
        )
