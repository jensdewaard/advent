module Parser (Sample (Sample), sample) where

import Options.Applicative

data Sample = Sample 
    { test :: Bool
    , year :: Int
    , day  :: Int
    }

sample :: Parser Sample
sample = Sample
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
