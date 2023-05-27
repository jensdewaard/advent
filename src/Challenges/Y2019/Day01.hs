module Challenges.Y2019.Day01 where

type Module = Int
type Mass = Int

fuelRequired :: Module -> Mass
fuelRequired m = (m `div` 3) - 2