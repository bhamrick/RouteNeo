module Test.Damage where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Test.HUnit

import Pokemon.Battle
import Pokemon.Species
import Pokemon.Moves
import Pokemon.Stats
import Pokemon.Type

-- Values taken from Silph rival fight
nidoking :: Participant
nidoking =
    Participant
        { _species = speciesByName ! "Nidoking"
        , _level = 35
        , _partyStats = Stats
            { _hpStat = 107
            , _atkStat = 81
            , _defStat = 70
            , _spdStat = 78
            , _spcStat = 66
            }
        , _battleStats = Stats
            { _hpStat = 107
            , _atkStat = 102
            , _defStat = 87
            , _spdStat = 117
            , _spcStat = 99
            }
        }

pidgeot :: Participant
pidgeot =
    Participant
        { _species = speciesByName ! "Pidgeot"
        , _level = 37
        , _partyStats = Stats
            { _hpStat = 114
            , _atkStat = 70
            , _defStat = 66
            , _spdStat = 78
            , _spcStat = 62
            }
        , _battleStats = Stats
            { _hpStat = 114
            , _atkStat = 70
            , _defStat = 66
            , _spdStat = 78
            , _spcStat = 62
            }
        }

thunderbolt :: Move
thunderbolt =
    Move
        { _moveName = "Thunderbolt"
        , _moveType = Electric
        , _pp = 15
        , _power = 95
        , _accuracy = 255
        }

pidgeotRange :: Test
pidgeotRange =
    TestList
        [ TestCase $ assertEqual "Min range should be 85." (damage nidoking pidgeot thunderbolt False minRange) 85
        , TestCase $ assertEqual "Max range should be 100." (damage nidoking pidgeot thunderbolt False maxRange) 100
        , TestCase $ assertEqual "Min crit range should be 105." (damage nidoking pidgeot thunderbolt True minRange) 105
        , TestCase $ assertEqual "Max crit range should be 124." (damage nidoking pidgeot thunderbolt True maxRange) 124
        ]

main :: IO Counts
main = runTestTT $ TestList
    [ pidgeotRange
    ]
