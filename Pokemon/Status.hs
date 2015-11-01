module Pokemon.Status where

data Status
    = Healthy
    | SLP Integer
    | PSN
    | BRN
    | FRZ
    | PAR
    deriving (Eq, Show, Ord)
