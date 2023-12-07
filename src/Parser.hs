module Parser (ProgramArgs (ProgramArgs), args) where

import Options.Applicative

data ProgramArgs = ProgramArgs 
    { year :: Int
    , day  :: Int
    }

args :: Parser ProgramArgs
args = ProgramArgs
    <$> argument auto
        ( help "The year to run a challenge of" 
        <> metavar "YEAR"
        )
    <*> argument auto
        ( help "The day to run the challenge of"
        <> metavar "DAY"
        )
