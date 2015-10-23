{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Battle where

import Control.Lens
import Data.Maybe
import Data.Monoid

import Pokemon.Moves
import Pokemon.Species
import Pokemon.Stats
import Pokemon.Type

data Participant =
    Participant
        { _species :: Species
        , _level :: Integer
        , _partyStats :: Stats
        , _battleStats :: Stats
        }
    deriving (Eq, Show, Ord)

makeLenses ''Participant

minRange = 217
maxRange = 255

damage :: Participant -> Participant -> Move -> Bool -> Integer -> Integer
damage attacker defender move crit roll =
    let
    attack =
        attacker ^.
        (if crit then partyStats else battleStats) .
        (case physicality (move^.moveType) of
            Physical -> atkStat
            Special -> spcStat)
    defense =
        defender ^.
        (if crit then partyStats else battleStats) .
        (case physicality (move^.moveType) of
            Physical -> defStat
            Special -> spcStat)
    stab =
        move^.moveType == attacker^.species.type1 ||
        Just (move^.moveType) == attacker^.species.type2
    effective =
        effectiveness (move^.moveType) (defender^.species.type1) <>
        fromMaybe mempty (effectiveness (move^.moveType) <$> (defender^.species.type2))
    in
    floor (fromInteger (floor (fromInteger ((floor (fromInteger (attacker^.level) * (2/5) * (if crit then 2 else 1) + 2) * attack * (move^.power) `div` 50 `div` defense) + 2) * (if stab then 3/2 else 1))) * attackRatio effective) * roll `div` 255
