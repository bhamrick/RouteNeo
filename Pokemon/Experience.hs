module Pokemon.Experience where

data ExpCurve = Slow | MediumSlow | Medium | Fast
    deriving (Eq, Show, Ord)

lowestExpForLevel :: ExpCurve -> Integer -> Integer
lowestExpForLevel curve n =
    case curve of
        Slow -> 5 * n^3 `div` 4
        MediumSlow -> 6 * n^3 `div` 5 - 15 * n^2 + 100 * n - 140
        Medium -> n^3
        Fast -> 4 * n^3 `div` 5
