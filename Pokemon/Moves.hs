{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Moves where

import Control.Lens

import Pokemon.Type

data Move =
    Move
        { _moveName :: String
        , _moveType :: Type
        , _pp :: Integer
        , _power :: Integer
        , _accuracy :: Integer
        -- TODO: Effect
        }
    deriving (Eq, Show, Ord)

makeLenses ''Move

-- TODO: Make all moves
tackle :: Move
tackle =
    Move
        { _moveName = "Tackle"
        , _moveType = Normal
        , _pp = 35
        , _power = 35
        , _accuracy = 243
        }
