module Main where

import Control.Lens
import Control.Monad
import Data.Foldable

import qualified Data.Map as Map
import Pokemon.Party
import Pokemon.Route
import Pokemon.Species
import Pokemon.Stats
import Pokemon.Trainer

route :: RouteT IO ()
route = do
    let 
        nidoDVs = DVs
            { _atkDV = 12
            , _defDV = 12
            , _spdDV = 12
            , _spcDV = 12
            }
        squirtleDVs = DVs
            { _atkDV = 12
            , _defDV = 12
            , _spdDV = 12
            , _spcDV = 12
            }
    party .=
        [ partyPokemon (speciesByName Map.! "NidoranM") 3 nidoDVs
        , partyPokemon (speciesByName Map.! "Squirtle") 8 squirtleDVs
        ]

    learnMove "Leer"
    learnMove "Tackle"

    printStats
    printRanges

    -- Defeat Brock
    for_ ((trainersByOffset Map.! 0x3A3B5) ^. tParty) $ \enemy -> do
        party . each %= defeatPokemon' (enemy^.tpSpecies) (enemy^.tpLevel) True 2
        preuse (party . _head) >>= maybe (return ()) printRanges'

    badges . boulderBadge .= True
    learnMove "Horn Attack"

    -- Route 3
    defeatTrainer 0x39DDA
    defeatTrainer 0x39D99
    defeatTrainer 0x39DDF
    defeatTrainer 0x39DE5

    learnMove "Poison Sting"

    -- Mt Moon
    defeatTrainer 0x39F26
    defeatTrainer 0x39E1C

    party . _head %= defeatPokemon' (speciesByName Map.! "Zubat") 8 False 1
    party . _head %= defeatPokemon' (speciesByName Map.! "Zubat") 8 False 1
    party . _head %= defeatPokemon' (speciesByName Map.! "Zubat") 8 False 1

    evolveTo "Nidorino"
    evolveTo "Nidoking"
    
    defeatTrainer 0x3A29C
    defeatTrainer 0x39F2A

    -- Nugget Bridge
    defeatTrainer 0x3A209
    defeatTrainer 0x39DF2
    defeatTrainer 0x39E27
    defeatTrainer 0x39DA5
    defeatTrainer 0x39E23
    party . _head %= rarecandy
    party . _head %= rarecandy
    learnMove "Thrash"
    unlearnMove "Leer"
    defeatTrainer 0x3A2B0

    -- Route 25
    defeatTrainer 0x39F63
    defeatTrainer 0x39E2b
    defeatTrainer 0x39E7C
    defeatTrainer 0x39E2F

    -- Cerulean Gym
    defeatTrainer 0x39E9D
    defeatTrainerWithRanges 0x3A3BB

    -- Route 6
    defeatTrainer 0x3A2AC
    defeatTrainer 0x39EA4
    defeatTrainer 0x39E86

    learnMove "Bubblebeam"
    unlearnMove "Tackle"

    defeatTrainer 0x3A40B

    -- Vermillion Gym
    defeatTrainer 0x3A3C1
    badges . thunderBadge .= True

    learnMove "Thunderbolt"
    unlearnMove "Horn Attack"

    -- Route 9
    defeatTrainer 0x39EAC
    defeatTrainer 0x39E07

    -- Rock Tunnel
    defeatTrainer 0x39F22
    defeatTrainer 0x39F1A
    defeatTrainer 0x39EC2
    defeatTrainer 0x39F81
    defeatTrainer 0x39EE8

    -- Route 8
    defeatTrainer 0x3A0CD

    learnMove "Horn Drill"
    unlearnMove "Bubblebeam"

    learnMove "Rock Slide"
    unlearnMove "Poison Sting"

    -- Lavender Tower
    defeatTrainer 0x3A42B
    defeatTrainer 0x3A4E3
    defeatTrainer 0x3A507
    defeatTrainer 0x3A504

    defeatTrainer 0x3A2ED
    defeatTrainer 0x3A2F2
    defeatTrainer 0x3A2F6

    learnMove "Ice Beam"
    unlearnMove "Rock Slide"

    -- Silph
    defeatTrainer 0x3A34B
    defeatTrainer 0x3A319
    defeatTrainer 0x3A44F
    defeatTrainer 0x3A355
    defeatTrainer 0x3A286

    -- Fuchsia Gym
    defeatTrainer 0x3A13A
    party . _head %= rarecandy
    defeatTrainer 0x3A140
    defeatTrainer 0x3A3D1
    badges . soulBadge .= True
    party . _head %= rarecandy
    party . _head %= rarecandy

    party . _head %= rarecandy
    party . _head %= rarecandy

    -- Celadon Gym
    defeatTrainer 0x3A0DB
    defeatTrainer 0x3A3C9

    -- Cinnabar Gym
    defeatTrainer 0x3A3DB
    badges . volcanoBadge .= True

    -- Saffron Gym
    defeatTrainer 0x3A3E5

    -- Viridian Gym
    defeatTrainer 0x3A382
    defeatTrainer 0x3A1DA
    defeatTrainer 0x3A290

    -- Route 22
    defeatTrainer 0x3A475

    -- Elite Four
    defeatTrainer 0x3A4BB
    defeatTrainer 0x3A3A9
    defeatTrainer 0x3A516
    defeatTrainer 0x3A522
    defeatTrainer 0x3A49F

    printStats
    printRanges

main :: IO ()
main = void $ runRouteT route emptyParty
