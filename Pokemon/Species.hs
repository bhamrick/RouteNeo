{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Species where

import Control.Lens
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Pokemon.Experience
import Pokemon.Moves
import Pokemon.Type

data Species =
    Species
        { _name :: String
        , _expCurve :: ExpCurve
        , _type1 :: Type
        , _type2 :: Maybe Type
        , _baseHP :: Integer
        , _baseAtk :: Integer
        , _baseDef :: Integer
        , _baseSpd :: Integer
        , _baseSpc :: Integer
        , _killExp :: Integer
        , _learnset :: [(Integer, Move)]
        }
    deriving (Eq, Show, Ord)

makeLenses ''Species

mkLearnset :: [(Integer, String)] -> [(Integer, Move)]
mkLearnset = each . _2 %~ (movesByName Map.!)

speciesByName :: Map String Species
speciesByName = Map.fromList (map (\s -> (s^.name, s)) allSpecies)

defaultMoves :: Species -> Integer -> [Move]
defaultMoves s lvl =
    map snd . reverse . take 4 . reverse . nub . filter (\(l, _) -> l <= lvl) $ s^.learnset

allSpecies :: [Species]
allSpecies =
    [ Species
        { _name     = "Bulbasaur"
        , _expCurve = MediumSlow
        , _type1    = Grass
        , _type2    = Just Poison
        , _baseHP   = 45
        , _baseAtk  = 49
        , _baseDef  = 49
        , _baseSpd  = 45
        , _baseSpc  = 65
        , _killExp  = 64
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Growl")
            , (7, "Leech Seed")
            , (13, "Vine Whip")
            , (20, "Poisonpowder")
            , (27, "Razor Leaf")
            , (34, "Growth")
            , (41, "Sleep Powder")
            , (48, "Solarbeam")
            ]
        }
    , Species
        { _name     = "Ivysaur"
        , _expCurve = MediumSlow
        , _type1    = Grass
        , _type2    = Just Poison
        , _baseHP   = 60
        , _baseAtk  = 62
        , _baseDef  = 63
        , _baseSpd  = 60
        , _baseSpc  = 80
        , _killExp  = 141
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Growl")
            , (0, "Leech Seed")
            , (7, "Leech Seed")
            , (13, "Vine Whip")
            , (22, "Poisonpowder")
            , (30, "Razor Leaf")
            , (38, "Growth")
            , (46, "Sleep Powder")
            , (54, "Solarbeam")
            ]
        }
    , Species
        { _name     = "Venusaur"
        , _expCurve = MediumSlow
        , _type1    = Grass
        , _type2    = Just Poison
        , _baseHP   = 80
        , _baseAtk  = 82
        , _baseDef  = 83
        , _baseSpd  = 80
        , _baseSpc  = 100
        , _killExp  = 208
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Growl")
            , (0, "Leech Seed")
            , (0, "Vine Whip")
            , (7, "Leech Seed")
            , (13, "Vine Whip")
            , (22, "Poisonpowder")
            , (30, "Razor Leaf")
            , (43, "Growth")
            , (55, "Sleep Powder")
            , (65, "Solarbeam")
            ]
        }
    , Species
        { _name     = "Charmander"
        , _expCurve = MediumSlow
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 39
        , _baseAtk  = 52
        , _baseDef  = 43
        , _baseSpd  = 65
        , _baseSpc  = 50
        , _killExp  = 65
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Growl")
            , (9, "Ember")
            , (15, "Leer")
            , (22, "Rage")
            , (30, "Slash")
            , (38, "Flamethrower")
            , (46, "Fire Spin")
            ]
        }
    , Species
        { _name     = "Charmeleon"
        , _expCurve = MediumSlow
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 58
        , _baseAtk  = 64
        , _baseDef  = 58
        , _baseSpd  = 80
        , _baseSpc  = 65
        , _killExp  = 142
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Growl")
            , (0, "Ember")
            , (9, "Ember")
            , (15, "Leer")
            , (24, "Rage")
            , (33, "Slash")
            , (42, "Flamethrower")
            , (56, "Fire Spin")
            ]
        }
    , Species
        { _name     = "Charizard"
        , _expCurve = MediumSlow
        , _type1    = Fire
        , _type2    = Just Flying
        , _baseHP   = 78
        , _baseAtk  = 84
        , _baseDef  = 78
        , _baseSpd  = 100
        , _baseSpc  = 85
        , _killExp  = 209
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Growl")
            , (0, "Ember")
            , (0, "Leer")
            , (9, "Ember")
            , (15, "Leer")
            , (24, "Rage")
            , (36, "Slash")
            , (46, "Flamethrower")
            , (55, "Fire Spin")
            ]
        }
    , Species
        { _name     = "Squirtle"
        , _expCurve = MediumSlow
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 44
        , _baseAtk  = 48
        , _baseDef  = 65
        , _baseSpd  = 43
        , _baseSpc  = 50
        , _killExp  = 66
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Tail Whip")
            , (8, "Bubble")
            , (15, "Water Gun")
            , (22, "Bite")
            , (28, "Withdraw")
            , (35, "Skull Bash")
            , (42, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Wartortle"
        , _expCurve = MediumSlow
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 59
        , _baseAtk  = 63
        , _baseDef  = 80
        , _baseSpd  = 58
        , _baseSpc  = 65
        , _killExp  = 143
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Tail Whip")
            , (0, "Bubble")
            , (8, "Bubble")
            , (15, "Water Gun")
            , (24, "Bite")
            , (31, "Withdraw")
            , (39, "Skull Bash")
            , (47, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Blastoise"
        , _expCurve = MediumSlow
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 79
        , _baseAtk  = 83
        , _baseDef  = 100
        , _baseSpd  = 78
        , _baseSpc  = 85
        , _killExp  = 210
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Tail Whip")
            , (0, "Bubble")
            , (0, "Water Gun")
            , (8, "Bubble")
            , (15, "Water Gun")
            , (24, "Bite")
            , (31, "Withdraw")
            , (42, "Skull Bash")
            , (52, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Caterpie"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Nothing
        , _baseHP   = 45
        , _baseAtk  = 30
        , _baseDef  = 35
        , _baseSpd  = 45
        , _baseSpc  = 20
        , _killExp  = 53
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "String Shot")
            ]
        }
    , Species
        { _name     = "Metapod"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Nothing
        , _baseHP   = 50
        , _baseAtk  = 20
        , _baseDef  = 55
        , _baseSpd  = 30
        , _baseSpc  = 25
        , _killExp  = 72
        , _learnset = mkLearnset
            [ (0, "Harden")
            ]
        }
    , Species
        { _name     = "Butterfree"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Just Flying
        , _baseHP   = 60
        , _baseAtk  = 45
        , _baseDef  = 50
        , _baseSpd  = 70
        , _baseSpc  = 80
        , _killExp  = 160
        , _learnset = mkLearnset
            [ (0, "Confusion")
            , (12, "Confusion")
            , (15, "Poisonpowder")
            , (16, "Stun Spore")
            , (17, "Sleep Powder")
            , (21, "Supersonic")
            , (26, "Whirlwind")
            , (32, "Psybeam")
            ]
        }
    , Species
        { _name     = "Weedle"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Just Poison
        , _baseHP   = 40
        , _baseAtk  = 35
        , _baseDef  = 30
        , _baseSpd  = 50
        , _baseSpc  = 20
        , _killExp  = 52
        , _learnset = mkLearnset
            [ (0, "Poison Sting")
            , (0, "String Shot")
            ]
        }
    , Species
        { _name     = "Kakuna"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Just Poison
        , _baseHP   = 45
        , _baseAtk  = 25
        , _baseDef  = 50
        , _baseSpd  = 35
        , _baseSpc  = 25
        , _killExp  = 71
        , _learnset = mkLearnset
            [ (0, "Harden")
            ]
        }
    , Species
        { _name     = "Beedrill"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Just Poison
        , _baseHP   = 65
        , _baseAtk  = 80
        , _baseDef  = 40
        , _baseSpd  = 75
        , _baseSpc  = 45
        , _killExp  = 159
        , _learnset = mkLearnset
            [ (0, "Fury Attack")
            , (12, "Fury Attack")
            , (16, "Focus Energy")
            , (20, "Twineedle")
            , (25, "Rage")
            , (30, "Pin Missile")
            , (35, "Agility")
            ]
        }
    , Species
        { _name     = "Pidgey"
        , _expCurve = MediumSlow
        , _type1    = Normal
        , _type2    = Just Flying
        , _baseHP   = 40
        , _baseAtk  = 45
        , _baseDef  = 40
        , _baseSpd  = 56
        , _baseSpc  = 35
        , _killExp  = 55
        , _learnset = mkLearnset
            [ (0, "Gust")
            , (5, "Sand Attack")
            , (12, "Quick Attack")
            , (19, "Whirlwind")
            , (28, "Wing Attack")
            , (36, "Agility")
            , (44, "Mirror Move")
            ]
        }
    , Species
        { _name     = "Pidgeotto"
        , _expCurve = MediumSlow
        , _type1    = Normal
        , _type2    = Just Flying
        , _baseHP   = 63
        , _baseAtk  = 60
        , _baseDef  = 55
        , _baseSpd  = 71
        , _baseSpc  = 50
        , _killExp  = 113
        , _learnset = mkLearnset
            [ (0, "Gust")
            , (0, "Sand Attack")
            , (5, "Sand Attack")
            , (12, "Quick Attack")
            , (21, "Whirlwind")
            , (31, "Wing Attack")
            , (40, "Agility")
            , (49, "Mirror Move")
            ]
        }
    , Species
        { _name     = "Pidgeot"
        , _expCurve = MediumSlow
        , _type1    = Normal
        , _type2    = Just Flying
        , _baseHP   = 83
        , _baseAtk  = 80
        , _baseDef  = 75
        , _baseSpd  = 91
        , _baseSpc  = 70
        , _killExp  = 172
        , _learnset = mkLearnset
            [ (0, "Gust")
            , (0, "Sand Attack")
            , (0, "Quick Attack")
            , (5, "Sand Attack")
            , (12, "Quick Attack")
            , (21, "Whirlwind")
            , (31, "Wing Attack")
            , (44, "Agility")
            , (54, "Mirror Move")
            ]
        }
    , Species
        { _name     = "Rattata"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 30
        , _baseAtk  = 56
        , _baseDef  = 35
        , _baseSpd  = 72
        , _baseSpc  = 25
        , _killExp  = 57
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Tail Whip")
            , (7, "Quick Attack")
            , (14, "Hyper Fang")
            , (23, "Focus Energy")
            , (34, "Super Fang")
            ]
        }
    , Species
        { _name     = "Raticate"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 55
        , _baseAtk  = 81
        , _baseDef  = 60
        , _baseSpd  = 97
        , _baseSpc  = 50
        , _killExp  = 116
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Tail Whip")
            , (0, "Quick Attack")
            , (7, "Quick Attack")
            , (14, "Hyper Fang")
            , (27, "Focus Energy")
            , (41, "Super Fang")]
        }
    , Species
        { _name     = "Spearow"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Just Flying
        , _baseHP   = 40
        , _baseAtk  = 60
        , _baseDef  = 30
        , _baseSpd  = 70
        , _baseSpc  = 31
        , _killExp  = 58
        , _learnset = mkLearnset
            [ (0, "Peck")
            , (0, "Growl")
            , (9, "Leer")
            , (15, "Fury Attack")
            , (22, "Mirror Move")
            , (29, "Drill Peck")
            , (36, "Agility")
            ]
        }
    , Species
        { _name     = "Fearow"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Just Flying
        , _baseHP   = 65
        , _baseAtk  = 90
        , _baseDef  = 65
        , _baseSpd  = 100
        , _baseSpc  = 61
        , _killExp  = 162
        , _learnset = mkLearnset
            [ (0, "Peck")
            , (0, "Growl")
            , (0, "Leer")
            , (9, "Leer")
            , (15, "Fury Attack")
            , (25, "Mirror Move")
            , (34, "Drill Peck")
            , (43, "Agility")
            ]
        }
    , Species
        { _name     = "Ekans"
        , _expCurve = Medium
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 35
        , _baseAtk  = 60
        , _baseDef  = 44
        , _baseSpd  = 55
        , _baseSpc  = 40
        , _killExp  = 62
        , _learnset = mkLearnset
            [ (0, "Wrap")
            , (0, "Leer")
            , (10, "Poison Sting")
            , (17, "Bite")
            , (24, "Glare")
            , (31, "Screech")
            , (38, "Acid")
            ]
        }
    , Species
        { _name     = "Arbok"
        , _expCurve = Medium
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 60
        , _baseAtk  = 85
        , _baseDef  = 69
        , _baseSpd  = 80
        , _baseSpc  = 65
        , _killExp  = 147
        , _learnset = mkLearnset
            [ (0, "Wrap")
            , (0, "Leer")
            , (0, "Poison Sting")
            , (10, "Poison Sting")
            , (17, "Bite")
            , (27, "Glare")
            , (36, "Screech")
            , (47, "Acid")
            ]
        }
    , Species
        { _name     = "Pikachu"
        , _expCurve = Medium
        , _type1    = Electric
        , _type2    = Nothing
        , _baseHP   = 35
        , _baseAtk  = 55
        , _baseDef  = 30
        , _baseSpd  = 90
        , _baseSpc  = 50
        , _killExp  = 82
        , _learnset = mkLearnset
            [ (0, "Thundershock")
            , (0, "Growl")
            , (9, "Thunder Wave")
            , (16, "Quick Attack")
            , (26, "Swift")
            , (33, "Agility")
            , (43, "Thunder")
            ]
        }
    , Species
        { _name     = "Raichu"
        , _expCurve = Medium
        , _type1    = Electric
        , _type2    = Nothing
        , _baseHP   = 60
        , _baseAtk  = 90
        , _baseDef  = 55
        , _baseSpd  = 100
        , _baseSpc  = 90
        , _killExp  = 122
        , _learnset = mkLearnset
            [ (0, "Thundershock")
            , (0, "Growl")
            , (0, "Thunder Wave")
            ]
        }
    , Species
        { _name     = "Sandshrew"
        , _expCurve = Medium
        , _type1    = Ground
        , _type2    = Nothing
        , _baseHP   = 50
        , _baseAtk  = 75
        , _baseDef  = 85
        , _baseSpd  = 40
        , _baseSpc  = 30
        , _killExp  = 93
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (10, "Sand Attack")
            , (17, "Slash")
            , (24, "Poison Sting")
            , (31, "Swift")
            , (38, "Fury Swipes")
            ]
        }
    , Species
        { _name     = "Sandslash"
        , _expCurve = Medium
        , _type1    = Ground
        , _type2    = Nothing
        , _baseHP   = 75
        , _baseAtk  = 100
        , _baseDef  = 110
        , _baseSpd  = 65
        , _baseSpc  = 55
        , _killExp  = 163
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Sand Attack")
            , (10, "Sand Attack")
            , (17, "Slash")
            , (27, "Poison Sting")
            , (36, "Swift")
            , (47, "Fury Swipes")
            ]
        }
    , Species
        { _name     = "NidoranF"
        , _expCurve = MediumSlow
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 55
        , _baseAtk  = 47
        , _baseDef  = 52
        , _baseSpd  = 41
        , _baseSpc  = 40
        , _killExp  = 59
        , _learnset = mkLearnset
            [ (0, "Growl")
            , (0, "Tackle")
            , (8, "Scratch")
            , (14, "Poison Sting")
            , (21, "Tail Whip")
            , (29, "Bite")
            , (36, "Fury Swipes")
            , (43, "Double Kick")
            ]
        }
    , Species
        { _name     = "Nidorina"
        , _expCurve = MediumSlow
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 70
        , _baseAtk  = 62
        , _baseDef  = 67
        , _baseSpd  = 56
        , _baseSpc  = 55
        , _killExp  = 117
        , _learnset = mkLearnset
            [ (0, "Growl")
            , (0, "Tackle")
            , (0, "Scratch")
            , (8, "Scratch")
            , (14, "Poison Sting")
            , (23, "Tail Whip")
            , (32, "Bite")
            , (41, "Fury Swipes")
            , (50, "Double Kick")
            ]
        }
    , Species
        { _name     = "Nidoqueen"
        , _expCurve = MediumSlow
        , _type1    = Poison
        , _type2    = Just Ground
        , _baseHP   = 90
        , _baseAtk  = 82
        , _baseDef  = 87
        , _baseSpd  = 76
        , _baseSpc  = 75
        , _killExp  = 194
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Scratch")
            , (0, "Tail Whip")
            , (0, "Body Slam")
            , (8, "Scratch")
            , (14, "Poison Sting")
            , (23, "Body Slam")
            ]
        }
    , Species
        { _name     = "NidoranM"
        , _expCurve = MediumSlow
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 46
        , _baseAtk  = 57
        , _baseDef  = 40
        , _baseSpd  = 50
        , _baseSpc  = 40
        , _killExp  = 60
        , _learnset = mkLearnset
            [ (0, "Leer")
            , (0, "Tackle")
            , (8, "Horn Attack")
            , (14, "Poison Sting")
            , (21, "Focus Energy")
            , (29, "Fury Attack")
            , (36, "Horn Drill")
            , (43, "Double Kick")
            ]
        }
    , Species
        { _name     = "Nidorino"
        , _expCurve = MediumSlow
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 61
        , _baseAtk  = 72
        , _baseDef  = 57
        , _baseSpd  = 65
        , _baseSpc  = 55
        , _killExp  = 118
        , _learnset = mkLearnset
            [ (0, "Leer")
            , (0, "Tackle")
            , (0, "Horn Attack")
            , (8, "Horn Attack")
            , (14, "Poison Sting")
            , (23, "Focus Energy")
            , (32, "Fury Attack")
            , (41, "Horn Drill")
            , (50, "Double Kick")
            ]
        }
    , Species
        { _name     = "Nidoking"
        , _expCurve = MediumSlow
        , _type1    = Poison
        , _type2    = Just Ground
        , _baseHP   = 81
        , _baseAtk  = 92
        , _baseDef  = 77
        , _baseSpd  = 85
        , _baseSpc  = 75
        , _killExp  = 195
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Horn Attack")
            , (0, "Poison Sting")
            , (0, "Thrash")
            , (8, "Horn Attack")
            , (14, "Poison Sting")
            , (23, "Thrash")
            ]
        }
    , Species
        { _name     = "Clefairy"
        , _expCurve = Fast
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 70
        , _baseAtk  = 45
        , _baseDef  = 48
        , _baseSpd  = 35
        , _baseSpc  = 60
        , _killExp  = 68
        , _learnset = mkLearnset
            [ (0, "Pound")
            , (0, "Growl")
            , (13, "Sing")
            , (18, "Doubleslap")
            , (24, "Minimize")
            , (31, "Metronome")
            , (39, "Defense Curl")
            , (48, "Light Screen")
            ]
        }
    , Species
        { _name     = "Clefable"
        , _expCurve = Fast
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 95
        , _baseAtk  = 70
        , _baseDef  = 73
        , _baseSpd  = 60
        , _baseSpc  = 85
        , _killExp  = 129
        , _learnset = mkLearnset
            [ (0, "Sing")
            , (0, "Doubleslap")
            , (0, "Minimize")
            , (0, "Metronome")
            ]
        }
    , Species
        { _name     = "Vulpix"
        , _expCurve = Medium
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 38
        , _baseAtk  = 41
        , _baseDef  = 40
        , _baseSpd  = 65
        , _baseSpc  = 65
        , _killExp  = 63
        , _learnset = mkLearnset
            [ (0, "Ember")
            , (0, "Tail Whip")
            , (16, "Quick Attack")
            , (21, "Roar")
            , (28, "Confuse Ray")
            , (35, "Flamethrower")
            , (42, "Fire Spin")
            ]
        }
    , Species
        { _name     = "Ninetales"
        , _expCurve = Medium
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 73
        , _baseAtk  = 76
        , _baseDef  = 75
        , _baseSpd  = 100
        , _baseSpc  = 100
        , _killExp  = 178
        , _learnset = mkLearnset
            [ (0, "Ember")
            , (0, "Tail Whip")
            , (0, "Quick Attack")
            , (0, "Roar")
            ]
        }
    , Species
        { _name     = "Jigglypuff"
        , _expCurve = Fast
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 115
        , _baseAtk  = 45
        , _baseDef  = 20
        , _baseSpd  = 20
        , _baseSpc  = 25
        , _killExp  = 76
        , _learnset = mkLearnset
            [ (0, "Sing")
            , (9, "Pound")
            , (14, "Disable")
            , (19, "Defense Curl")
            , (24, "Doubleslap")
            , (29, "Rest")
            , (34, "Body Slam")
            , (39, "Double Edge")
            ]
        }
    , Species
        { _name     = "Wigglytuff"
        , _expCurve = Fast
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 140
        , _baseAtk  = 70
        , _baseDef  = 45
        , _baseSpd  = 45
        , _baseSpc  = 50
        , _killExp  = 109
        , _learnset = mkLearnset
            [ (0, "Sing")
            , (0, "Disable")
            , (0, "Defense Curl")
            , (0, "Doubleslap")
            ]
        }
    , Species
        { _name     = "Zubat"
        , _expCurve = Medium
        , _type1    = Poison
        , _type2    = Just Flying
        , _baseHP   = 40
        , _baseAtk  = 45
        , _baseDef  = 35
        , _baseSpd  = 55
        , _baseSpc  = 40
        , _killExp  = 54
        , _learnset = mkLearnset
            [ (0, "Leech Life")
            , (10, "Supersonic")
            , (15, "Bite")
            , (21, "Confuse Ray")
            , (28, "Wing Attack")
            , (36, "Haze")
            ]
        }
    , Species
        { _name     = "Golbat"
        , _expCurve = Medium
        , _type1    = Poison
        , _type2    = Just Flying
        , _baseHP   = 75
        , _baseAtk  = 80
        , _baseDef  = 70
        , _baseSpd  = 90
        , _baseSpc  = 75
        , _killExp  = 171
        , _learnset = mkLearnset
            [ (0, "Leech Life")
            , (0, "Screech")
            , (0, "Bite")
            , (10, "Supersonic")
            , (15, "Bite")
            , (21, "Confuse Ray")
            , (32, "Wing Attack")
            , (43, "Haze")
            ]
        }
    , Species
        { _name     = "Oddish"
        , _expCurve = MediumSlow
        , _type1    = Grass
        , _type2    = Just Poison
        , _baseHP   = 45
        , _baseAtk  = 50
        , _baseDef  = 55
        , _baseSpd  = 30
        , _baseSpc  = 75
        , _killExp  = 78
        , _learnset = mkLearnset
            [ (0, "Absorb")
            , (15, "Poisonpowder")
            , (17, "Stun Spore")
            , (19, "Sleep Powder")
            , (24, "Acid")
            , (33, "Petal Dance")
            , (46, "Solarbeam")
            ]
        }
    , Species
        { _name     = "Gloom"
        , _expCurve = MediumSlow
        , _type1    = Grass
        , _type2    = Just Poison
        , _baseHP   = 60
        , _baseAtk  = 65
        , _baseDef  = 70
        , _baseSpd  = 40
        , _baseSpc  = 85
        , _killExp  = 132
        , _learnset = mkLearnset
            [ (0, "Absorb")
            , (0, "Poisonpowder")
            , (0, "Stun Spore")
            , (15, "Poisonpowder")
            , (17, "Stun Spore")
            , (19, "Sleep Powder")
            , (28, "Acid")
            , (38, "Petal Dance")
            , (52, "Solarbeam")
            ]
        }
    , Species
        { _name     = "Vileplume"
        , _expCurve = MediumSlow
        , _type1    = Grass
        , _type2    = Just Poison
        , _baseHP   = 75
        , _baseAtk  = 80
        , _baseDef  = 85
        , _baseSpd  = 50
        , _baseSpc  = 100
        , _killExp  = 184
        , _learnset = mkLearnset
            [ (0, "Stun Spore")
            , (0, "Sleep Powder")
            , (0, "Acid")
            , (0, "Petal Dance")
            , (15, "Poisonpowder")
            , (17, "Stun Spore")
            , (19, "Sleep Powder")
            ]
        }
    , Species
        { _name     = "Paras"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Just Grass
        , _baseHP   = 35
        , _baseAtk  = 70
        , _baseDef  = 55
        , _baseSpd  = 25
        , _baseSpc  = 55
        , _killExp  = 70
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (13, "Stun Spore")
            , (20, "Leech Life")
            , (27, "Spore")
            , (34, "Slash")
            , (41, "Growth")
            ]
        }
    , Species
        { _name     = "Parasect"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Just Grass
        , _baseHP   = 60
        , _baseAtk  = 95
        , _baseDef  = 80
        , _baseSpd  = 30
        , _baseSpc  = 80
        , _killExp  = 128
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Stun Spore")
            , (0, "Leech Life")
            , (13, "Stun Spore")
            , (20, "Leech Life")
            , (30, "Spore")
            , (39, "Slash")
            , (48, "Growth")
            ]
        }
    , Species
        { _name     = "Venonat"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Just Poison
        , _baseHP   = 60
        , _baseAtk  = 55
        , _baseDef  = 50
        , _baseSpd  = 45
        , _baseSpc  = 40
        , _killExp  = 75
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Disable")
            , (24, "Poisonpowder")
            , (27, "Leech Life")
            , (30, "Stun Spore")
            , (35, "Psybeam")
            , (38, "Sleep Powder")
            , (43, "Psychic")
            ]
        }
    , Species
        { _name     = "Venomoth"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Just Poison
        , _baseHP   = 70
        , _baseAtk  = 65
        , _baseDef  = 60
        , _baseSpd  = 90
        , _baseSpc  = 90
        , _killExp  = 138
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Disable")
            , (0, "Poisonpowder")
            , (0, "Leech Life")
            , (24, "Poisonpowder")
            , (27, "Leech Life")
            , (30, "Stun Spore")
            , (38, "Psybeam")
            , (43, "Sleep Powder")
            , (50, "Psychic")
            ]
        }
    , Species
        { _name     = "Diglett"
        , _expCurve = Medium
        , _type1    = Ground
        , _type2    = Nothing
        , _baseHP   = 10
        , _baseAtk  = 55
        , _baseDef  = 25
        , _baseSpd  = 95
        , _baseSpc  = 45
        , _killExp  = 81
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (15, "Growl")
            , (19, "Dig")
            , (24, "Sand Attack")
            , (31, "Slash")
            , (40, "Earthquake")
            ]
        }
    , Species
        { _name     = "Dugtrio"
        , _expCurve = Medium
        , _type1    = Ground
        , _type2    = Nothing
        , _baseHP   = 35
        , _baseAtk  = 80
        , _baseDef  = 50
        , _baseSpd  = 120
        , _baseSpc  = 70
        , _killExp  = 153
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Growl")
            , (0, "Dig")
            , (15, "Growl")
            , (19, "Dig")
            , (24, "Sand Attack")
            , (35, "Slash")
            , (47, "Earthquake")
            ]
        }
    , Species
        { _name     = "Meowth"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 40
        , _baseAtk  = 45
        , _baseDef  = 35
        , _baseSpd  = 90
        , _baseSpc  = 45
        , _killExp  = 69
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Growl")
            , (12, "Bite")
            , (17, "Pay Day")
            , (24, "Screech")
            , (33, "Fury Swipes")
            , (44, "Slash")
            ]
        }
    , Species
        { _name     = "Persian"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 70
        , _baseDef  = 60
        , _baseSpd  = 115
        , _baseSpc  = 65
        , _killExp  = 148
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Growl")
            , (0, "Bite")
            , (0, "Screech")
            , (12, "Bite")
            , (17, "Pay Day")
            , (24, "Screech")
            , (37, "Fury Swipes")
            , (51, "Slash")
            ]
        }
    , Species
        { _name     = "Psyduck"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 50
        , _baseAtk  = 52
        , _baseDef  = 48
        , _baseSpd  = 55
        , _baseSpc  = 50
        , _killExp  = 80
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (28, "Tail Whip")
            , (31, "Disable")
            , (36, "Confusion")
            , (43, "Fury Swipes")
            , (52, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Golduck"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 80
        , _baseAtk  = 82
        , _baseDef  = 78
        , _baseSpd  = 85
        , _baseSpc  = 80
        , _killExp  = 174
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Tail Whip")
            , (0, "Disable")
            , (28, "Tail Whip")
            , (31, "Disable")
            , (39, "Confusion")
            , (48, "Fury Swipes")
            , (59, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Mankey"
        , _expCurve = Medium
        , _type1    = Fighting
        , _type2    = Nothing
        , _baseHP   = 40
        , _baseAtk  = 80
        , _baseDef  = 35
        , _baseSpd  = 70
        , _baseSpc  = 35
        , _killExp  = 74
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Leer")
            , (15, "Karate Chop")
            , (21, "Fury Swipes")
            , (27, "Focus Energy")
            , (33, "Seismic Toss")
            , (39, "Thrash")
            ]
        }
    , Species
        { _name     = "Primeape"
        , _expCurve = Medium
        , _type1    = Fighting
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 105
        , _baseDef  = 60
        , _baseSpd  = 95
        , _baseSpc  = 60
        , _killExp  = 149
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Leer")
            , (0, "Karate Chop")
            , (0, "Fury Swipes")
            , (15, "Karate Chop")
            , (21, "Fury Swipes")
            , (27, "Focus Energy")
            , (37, "Seismic Toss")
            , (46, "Thrash")
            ]
        }
    , Species
        { _name     = "Growlithe"
        , _expCurve = Slow
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 55
        , _baseAtk  = 70
        , _baseDef  = 45
        , _baseSpd  = 60
        , _baseSpc  = 50
        , _killExp  = 91
        , _learnset = mkLearnset
            [ (0, "Bite")
            , (0, "Roar")
            , (18, "Ember")
            , (23, "Leer")
            , (30, "Take Down")
            , (39, "Agility")
            , (50, "Flamethrower")
            ]
        }
    , Species
        { _name     = "Arcanine"
        , _expCurve = Slow
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 90
        , _baseAtk  = 110
        , _baseDef  = 80
        , _baseSpd  = 95
        , _baseSpc  = 80
        , _killExp  = 213
        , _learnset = mkLearnset
            [ (0, "Roar")
            , (0, "Ember")
            , (0, "Leer")
            , (0, "Take Down")
            ]
        }
    , Species
        { _name     = "Poliwag"
        , _expCurve = MediumSlow
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 40
        , _baseAtk  = 50
        , _baseDef  = 40
        , _baseSpd  = 90
        , _baseSpc  = 40
        , _killExp  = 77
        , _learnset = mkLearnset
            [ (0, "Bubble")
            , (16, "Hypnosis")
            , (19, "Water Gun")
            , (25, "Doubleslap")
            , (31, "Body Slam")
            , (38, "Amnesia")
            , (45, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Poliwhirl"
        , _expCurve = MediumSlow
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 65
        , _baseDef  = 65
        , _baseSpd  = 90
        , _baseSpc  = 50
        , _killExp  = 131
        , _learnset = mkLearnset
            [ (0, "Bubble")
            , (0, "Hypnosis")
            , (0, "Water Gun")
            , (16, "Hypnosis")
            , (19, "Water Gun")
            , (26, "Doubleslap")
            , (33, "Body Slam")
            , (41, "Amnesia")
            , (49, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Poliwrath"
        , _expCurve = MediumSlow
        , _type1    = Water
        , _type2    = Just Fighting
        , _baseHP   = 90
        , _baseAtk  = 85
        , _baseDef  = 95
        , _baseSpd  = 70
        , _baseSpc  = 70
        , _killExp  = 185
        , _learnset = mkLearnset
            [ (0, "Hypnosis")
            , (0, "Water Gun")
            , (0, "Doubleslap")
            , (0, "Body Slam")
            , (16, "Hypnosis")
            , (19, "Water Gun")
            ]
        }
    , Species
        { _name     = "Abra"
        , _expCurve = MediumSlow
        , _type1    = Psychic
        , _type2    = Nothing
        , _baseHP   = 25
        , _baseAtk  = 20
        , _baseDef  = 15
        , _baseSpd  = 90
        , _baseSpc  = 105
        , _killExp  = 73
        , _learnset = mkLearnset
            [ (0, "Teleport")
            ]
        }
    , Species
        { _name     = "Kadabra"
        , _expCurve = MediumSlow
        , _type1    = Psychic
        , _type2    = Nothing
        , _baseHP   = 40
        , _baseAtk  = 35
        , _baseDef  = 30
        , _baseSpd  = 105
        , _baseSpc  = 120
        , _killExp  = 145
        , _learnset = mkLearnset
            [ (0, "Teleport")
            , (0, "Confusion")
            , (0, "Disable")
            , (16, "Confusion")
            , (20, "Disable")
            , (27, "Psybeam")
            , (31, "Recover")
            , (38, "Psychic")
            , (42, "Reflect")
            ]
        }
    , Species
        { _name     = "Alakazam"
        , _expCurve = MediumSlow
        , _type1    = Psychic
        , _type2    = Nothing
        , _baseHP   = 55
        , _baseAtk  = 50
        , _baseDef  = 45
        , _baseSpd  = 120
        , _baseSpc  = 135
        , _killExp  = 186
        , _learnset = mkLearnset
            [ (0, "Teleport")
            , (0, "Confusion")
            , (0, "Disable")
            , (16, "Confusion")
            , (20, "Disable")
            , (27, "Psybeam")
            , (31, "Recover")
            , (38, "Psychic")
            , (42, "Reflect")
            ]
        }
    , Species
        { _name     = "Machop"
        , _expCurve = MediumSlow
        , _type1    = Fighting
        , _type2    = Nothing
        , _baseHP   = 70
        , _baseAtk  = 80
        , _baseDef  = 50
        , _baseSpd  = 35
        , _baseSpc  = 35
        , _killExp  = 88
        , _learnset = mkLearnset
            [ (0, "Karate Chop")
            , (20, "Low Kick")
            , (25, "Leer")
            , (32, "Focus Energy")
            , (39, "Seismic Toss")
            , (46, "Submission")
            ]
        }
    , Species
        { _name     = "Machoke"
        , _expCurve = MediumSlow
        , _type1    = Fighting
        , _type2    = Nothing
        , _baseHP   = 80
        , _baseAtk  = 100
        , _baseDef  = 70
        , _baseSpd  = 45
        , _baseSpc  = 50
        , _killExp  = 146
        , _learnset = mkLearnset
            [ (0, "Karate Chop")
            , (0, "Low Kick")
            , (0, "Leer")
            , (20, "Low Kick")
            , (25, "Leer")
            , (36, "Focus Energy")
            , (44, "Seismic Toss")
            , (52, "Submission")
            ]
        }
    , Species
        { _name     = "Machamp"
        , _expCurve = MediumSlow
        , _type1    = Fighting
        , _type2    = Nothing
        , _baseHP   = 90
        , _baseAtk  = 130
        , _baseDef  = 80
        , _baseSpd  = 55
        , _baseSpc  = 65
        , _killExp  = 193
        , _learnset = mkLearnset
            [ (0, "Karate Chop")
            , (0, "Low Kick")
            , (0, "Leer")
            , (20, "Low Kick")
            , (25, "Leer")
            , (36, "Focus Energy")
            , (44, "Seismic Toss")
            , (52, "Submission")
            ]
        }
    , Species
        { _name     = "Bellsprout"
        , _expCurve = MediumSlow
        , _type1    = Grass
        , _type2    = Just Poison
        , _baseHP   = 50
        , _baseAtk  = 75
        , _baseDef  = 35
        , _baseSpd  = 40
        , _baseSpc  = 70
        , _killExp  = 84
        , _learnset = mkLearnset
            [ (0, "Vine Whip")
            , (0, "Growth")
            , (13, "Wrap")
            , (15, "Poisonpowder")
            , (18, "Sleep Powder")
            , (21, "Stun Spore")
            , (26, "Acid")
            , (33, "Razor Leaf")
            , (42, "Slam")
            ]
        }
    , Species
        { _name     = "Weepinbell"
        , _expCurve = MediumSlow
        , _type1    = Grass
        , _type2    = Just Poison
        , _baseHP   = 65
        , _baseAtk  = 90
        , _baseDef  = 50
        , _baseSpd  = 55
        , _baseSpc  = 85
        , _killExp  = 151
        , _learnset = mkLearnset
            [ (0, "Vine Whip")
            , (0, "Growth")
            , (0, "Wrap")
            , (13, "Wrap")
            , (15, "Poisonpowder")
            , (18, "Sleep Powder")
            , (23, "Stun Spore")
            , (29, "Acid")
            , (38, "Razor Leaf")
            , (49, "Slam")
            ]
        }
    , Species
        { _name     = "Victreebel"
        , _expCurve = MediumSlow
        , _type1    = Grass
        , _type2    = Just Poison
        , _baseHP   = 80
        , _baseAtk  = 105
        , _baseDef  = 65
        , _baseSpd  = 70
        , _baseSpc  = 100
        , _killExp  = 191
        , _learnset = mkLearnset
            [ (0, "Sleep Powder")
            , (0, "Stun Spore")
            , (0, "Acid")
            , (0, "Razor Leaf")
            , (13, "Wrap")
            , (15, "Poisonpowder")
            , (18, "Sleep Powder")
            ]
        }
    , Species
        { _name     = "Tentacool"
        , _expCurve = Slow
        , _type1    = Water
        , _type2    = Just Poison
        , _baseHP   = 40
        , _baseAtk  = 80
        , _baseDef  = 100
        , _baseSpd  = 20
        , _baseSpc  = 30
        , _killExp  = 105
        , _learnset = mkLearnset
            [ (0, "Acid")
            , (7, "Supersonic")
            , (13, "Wrap")
            , (18, "Poison Sting")
            , (22, "Water Gun")
            , (27, "Constrict")
            , (33, "Barrier")
            , (40, "Screech")
            , (48, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Tentacruel"
        , _expCurve = Slow
        , _type1    = Water
        , _type2    = Just Poison
        , _baseHP   = 80
        , _baseAtk  = 70
        , _baseDef  = 65
        , _baseSpd  = 100
        , _baseSpc  = 120
        , _killExp  = 205
        , _learnset = mkLearnset
            [ (0, "Acid")
            , (0, "Supersonic")
            , (0, "Wrap")
            , (7, "Supersonic")
            , (13, "Wrap")
            , (18, "Poison Sting")
            , (22, "Water Gun")
            , (27, "Constrict")
            , (35, "Barrier")
            , (43, "Screech")
            , (50, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Geodude"
        , _expCurve = MediumSlow
        , _type1    = Rock
        , _type2    = Just Ground
        , _baseHP   = 40
        , _baseAtk  = 80
        , _baseDef  = 100
        , _baseSpd  = 20
        , _baseSpc  = 30
        , _killExp  = 86
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (11, "Defense Curl")
            , (16, "Rock Throw")
            , (21, "Selfdestruct")
            , (26, "Harden")
            , (31, "Earthquake")
            , (36, "Explosion")
            ]
        }
    , Species
        { _name     = "Graveler"
        , _expCurve = MediumSlow
        , _type1    = Rock
        , _type2    = Just Ground
        , _baseHP   = 55
        , _baseAtk  = 95
        , _baseDef  = 115
        , _baseSpd  = 35
        , _baseSpc  = 45
        , _killExp  = 134
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Defense Curl")
            , (11, "Defense Curl")
            , (16, "Rock Throw")
            , (21, "Selfdestruct")
            , (29, "Harden")
            , (36, "Earthquake")
            , (43, "Explosion")
            ]
        }
    , Species
        { _name     = "Golem"
        , _expCurve = MediumSlow
        , _type1    = Rock
        , _type2    = Just Ground
        , _baseHP   = 80
        , _baseAtk  = 110
        , _baseDef  = 130
        , _baseSpd  = 45
        , _baseSpc  = 55
        , _killExp  = 177
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Defense Curl")
            , (11, "Defense Curl")
            , (16, "Rock Throw")
            , (21, "Selfdestruct")
            , (29, "Harden")
            , (36, "Earthquake")
            , (43, "Explosion")
            ]
        }
    , Species
        { _name     = "Ponyta"
        , _expCurve = Medium
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 50
        , _baseAtk  = 85
        , _baseDef  = 55
        , _baseSpd  = 90
        , _baseSpc  = 65
        , _killExp  = 152
        , _learnset = mkLearnset
            [ (0, "Ember")
            , (30, "Tail Whip")
            , (32, "Stomp")
            , (35, "Growl")
            , (39, "Fire Spin")
            , (43, "Take Down")
            , (48, "Agility")
            ]
        }
    , Species
        { _name     = "Rapidash"
        , _expCurve = Medium
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 100
        , _baseDef  = 70
        , _baseSpd  = 105
        , _baseSpc  = 80
        , _killExp  = 192
        , _learnset = mkLearnset
            [ (0, "Ember")
            , (0, "Tail Whip")
            , (0, "Stomp")
            , (0, "Growl")
            , (30, "Tail Whip")
            , (32, "Stomp")
            , (35, "Growl")
            , (39, "Fire Spin")
            , (47, "Take Down")
            , (55, "Agility")
            ]
        }
    , Species
        { _name     = "Slowpoke"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Just Psychic
        , _baseHP   = 90
        , _baseAtk  = 65
        , _baseDef  = 65
        , _baseSpd  = 15
        , _baseSpc  = 40
        , _killExp  = 99
        , _learnset = mkLearnset
            [ (0, "Confusion")
            , (18, "Disable")
            , (22, "Headbutt")
            , (27, "Growl")
            , (33, "Water Gun")
            , (40, "Amnesia")
            , (48, "Psychic")
            ]
        }
    , Species
        { _name     = "Slowbro"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Just Psychic
        , _baseHP   = 95
        , _baseAtk  = 75
        , _baseDef  = 110
        , _baseSpd  = 30
        , _baseSpc  = 80
        , _killExp  = 164
        , _learnset = mkLearnset
            [ (0, "Confusion")
            , (0, "Disable")
            , (0, "Headbutt")
            , (18, "Disable")
            , (22, "Headbutt")
            , (27, "Growl")
            , (33, "Water Gun")
            , (37, "Withdraw")
            , (44, "Amnesia")
            , (55, "Psychic")
            ]
        }
    , Species
        { _name     = "Magnemite"
        , _expCurve = Medium
        , _type1    = Electric
        , _type2    = Nothing
        , _baseHP   = 25
        , _baseAtk  = 35
        , _baseDef  = 70
        , _baseSpd  = 45
        , _baseSpc  = 95
        , _killExp  = 89
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (21, "Sonicboom")
            , (25, "Thundershock")
            , (29, "Supersonic")
            , (35, "Thunder Wave")
            , (41, "Swift")
            , (47, "Screech")
            ]
        }
    , Species
        { _name     = "Magneton"
        , _expCurve = Medium
        , _type1    = Electric
        , _type2    = Nothing
        , _baseHP   = 50
        , _baseAtk  = 60
        , _baseDef  = 95
        , _baseSpd  = 70
        , _baseSpc  = 120
        , _killExp  = 169
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Sonicboom")
            , (0, "Thundershock")
            , (21, "Sonicboom")
            , (25, "Thundershock")
            , (29, "Supersonic")
            , (38, "Thunder Wave")
            , (46, "Swift")
            , (54, "Screech")
            ]
        }
    , Species
        { _name     = "Farfetchd"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Just Flying
        , _baseHP   = 52
        , _baseAtk  = 65
        , _baseDef  = 55
        , _baseSpd  = 60
        , _baseSpc  = 58
        , _killExp  = 94
        , _learnset = mkLearnset
            [ (0, "Peck")
            , (0, "Sand Attack")
            , (7, "Leer")
            , (15, "Fury Attack")
            , (23, "Swords Dance")
            , (31, "Agility")
            , (39, "Slash")
            ]
        }
    , Species
        { _name     = "Doduo"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Just Flying
        , _baseHP   = 35
        , _baseAtk  = 85
        , _baseDef  = 45
        , _baseSpd  = 75
        , _baseSpc  = 35
        , _killExp  = 96
        , _learnset = mkLearnset
            [ (0, "Peck")
            , (20, "Growl")
            , (24, "Fury Attack")
            , (30, "Drill Peck")
            , (36, "Rage")
            , (40, "Tri Attack")
            , (44, "Agility")
            ]
        }
    , Species
        { _name     = "Dodrio"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Just Flying
        , _baseHP   = 60
        , _baseAtk  = 110
        , _baseDef  = 70
        , _baseSpd  = 100
        , _baseSpc  = 60
        , _killExp  = 158
        , _learnset = mkLearnset
            [ (0, "Peck")
            , (0, "Growl")
            , (0, "Fury Attack")
            , (20, "Growl")
            , (24, "Fury Attack")
            , (30, "Drill Peck")
            , (39, "Rage")
            , (45, "Tri Attack")
            , (51, "Agility")
            ]
        }
    , Species
        { _name     = "Seel"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 45
        , _baseDef  = 55
        , _baseSpd  = 45
        , _baseSpc  = 70
        , _killExp  = 100
        , _learnset = mkLearnset
            [ (0, "Headbutt")
            , (30, "Growl")
            , (35, "Aurora Beam")
            , (40, "Rest")
            , (45, "Take Down")
            , (50, "Ice Beam")
            ]
        }
    , Species
        { _name     = "Dewgong"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Just Ice
        , _baseHP   = 90
        , _baseAtk  = 70
        , _baseDef  = 80
        , _baseSpd  = 70
        , _baseSpc  = 95
        , _killExp  = 176
        , _learnset = mkLearnset
            [ (0, "Headbutt")
            , (0, "Growl")
            , (0, "Aurora Beam")
            , (30, "Growl")
            , (35, "Aurora Beam")
            , (44, "Rest")
            , (50, "Take Down")
            , (56, "Ice Beam")
            ]
        }
    , Species
        { _name     = "Grimer"
        , _expCurve = Medium
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 80
        , _baseAtk  = 80
        , _baseDef  = 50
        , _baseSpd  = 25
        , _baseSpc  = 40
        , _killExp  = 90
        , _learnset = mkLearnset
            [ (0, "Pound")
            , (0, "Disable")
            , (30, "Poison Gas")
            , (33, "Minimize")
            , (37, "Sludge")
            , (42, "Harden")
            , (48, "Screech")
            , (55, "Acid Armor")
            ]
        }
    , Species
        { _name     = "Muk"
        , _expCurve = Medium
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 105
        , _baseAtk  = 105
        , _baseDef  = 75
        , _baseSpd  = 50
        , _baseSpc  = 65
        , _killExp  = 157
        , _learnset = mkLearnset
            [ (0, "Pound")
            , (0, "Disable")
            , (0, "Poison Gas")
            , (30, "Poison Gas")
            , (33, "Minimize")
            , (37, "Sludge")
            , (45, "Harden")
            , (53, "Screech")
            , (60, "Acid Armor")
            ]
        }
    , Species
        { _name     = "Shellder"
        , _expCurve = Slow
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 30
        , _baseAtk  = 65
        , _baseDef  = 100
        , _baseSpd  = 40
        , _baseSpc  = 45
        , _killExp  = 97
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Withdraw")
            , (18, "Supersonic")
            , (23, "Clamp")
            , (30, "Aurora Beam")
            , (39, "Leer")
            , (50, "Ice Beam")
            ]
        }
    , Species
        { _name     = "Cloyster"
        , _expCurve = Slow
        , _type1    = Water
        , _type2    = Just Ice
        , _baseHP   = 50
        , _baseAtk  = 95
        , _baseDef  = 180
        , _baseSpd  = 70
        , _baseSpc  = 85
        , _killExp  = 203
        , _learnset = mkLearnset
            [ (0, "Withdraw")
            , (0, "Supersonic")
            , (0, "Clamp")
            , (0, "Aurora Beam")
            , (50, "Spike Cannon")
            ]
        }
    , Species
        { _name     = "Gastly"
        , _expCurve = MediumSlow
        , _type1    = Ghost
        , _type2    = Just Poison
        , _baseHP   = 30
        , _baseAtk  = 35
        , _baseDef  = 30
        , _baseSpd  = 80
        , _baseSpc  = 100
        , _killExp  = 95
        , _learnset = mkLearnset
            [ (0, "Lick")
            , (0, "Confuse Ray")
            , (0, "Night Shade")
            , (27, "Hypnosis")
            , (35, "Dream Eater")
            ]
        }
    , Species
        { _name     = "Haunter"
        , _expCurve = MediumSlow
        , _type1    = Ghost
        , _type2    = Just Poison
        , _baseHP   = 45
        , _baseAtk  = 50
        , _baseDef  = 45
        , _baseSpd  = 95
        , _baseSpc  = 115
        , _killExp  = 126
        , _learnset = mkLearnset
            [ (0, "Lick")
            , (0, "Confuse Ray")
            , (0, "Night Shade")
            , (29, "Hypnosis")
            , (38, "Dream Eater")
            ]
        }
    , Species
        { _name     = "Gengar"
        , _expCurve = MediumSlow
        , _type1    = Ghost
        , _type2    = Just Poison
        , _baseHP   = 60
        , _baseAtk  = 65
        , _baseDef  = 60
        , _baseSpd  = 110
        , _baseSpc  = 130
        , _killExp  = 190
        , _learnset = mkLearnset
            [ (0, "Lick")
            , (0, "Confuse Ray")
            , (0, "Night Shade")
            , (29, "Hypnosis")
            , (38, "Dream Eater")
            ]
        }
    , Species
        { _name     = "Onix"
        , _expCurve = Medium
        , _type1    = Rock
        , _type2    = Just Ground
        , _baseHP   = 35
        , _baseAtk  = 45
        , _baseDef  = 160
        , _baseSpd  = 70
        , _baseSpc  = 30
        , _killExp  = 108
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Screech")
            , (15, "Bind")
            , (19, "Rock Throw")
            , (25, "Rage")
            , (33, "Slam")
            , (43, "Harden")
            ]
        }
    , Species
        { _name     = "Drowzee"
        , _expCurve = Medium
        , _type1    = Psychic
        , _type2    = Nothing
        , _baseHP   = 60
        , _baseAtk  = 48
        , _baseDef  = 45
        , _baseSpd  = 42
        , _baseSpc  = 90
        , _killExp  = 102
        , _learnset = mkLearnset
            [ (0, "Pound")
            , (0, "Hypnosis")
            , (12, "Disable")
            , (17, "Confusion")
            , (24, "Headbutt")
            , (29, "Poison Gas")
            , (32, "Psychic")
            , (37, "Meditate")
            ]
        }
    , Species
        { _name     = "Hypno"
        , _expCurve = Medium
        , _type1    = Psychic
        , _type2    = Nothing
        , _baseHP   = 85
        , _baseAtk  = 73
        , _baseDef  = 70
        , _baseSpd  = 67
        , _baseSpc  = 115
        , _killExp  = 165
        , _learnset = mkLearnset
            [ (0, "Pound")
            , (0, "Hypnosis")
            , (0, "Disable")
            , (0, "Confusion")
            , (12, "Disable")
            , (17, "Confusion")
            , (24, "Headbutt")
            , (33, "Poison Gas")
            , (37, "Psychic")
            , (43, "Meditate")
            ]
        }
    , Species
        { _name     = "Krabby"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 30
        , _baseAtk  = 105
        , _baseDef  = 90
        , _baseSpd  = 50
        , _baseSpc  = 25
        , _killExp  = 115
        , _learnset = mkLearnset
            [ (0, "Bubble")
            , (0, "Leer")
            , (20, "Vicegrip")
            , (25, "Guillotine")
            , (30, "Stomp")
            , (35, "Crabhammer")
            , (40, "Harden")
            ]
        }
    , Species
        { _name     = "Kingler"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 55
        , _baseAtk  = 130
        , _baseDef  = 115
        , _baseSpd  = 75
        , _baseSpc  = 50
        , _killExp  = 206
        , _learnset = mkLearnset
            [ (0, "Bubble")
            , (0, "Leer")
            , (0, "Vicegrip")
            , (20, "Vicegrip")
            , (25, "Guillotine")
            , (34, "Stomp")
            , (42, "Crabhammer")
            , (49, "Harden")
            ]
        }
    , Species
        { _name     = "Voltorb"
        , _expCurve = Medium
        , _type1    = Electric
        , _type2    = Nothing
        , _baseHP   = 40
        , _baseAtk  = 30
        , _baseDef  = 50
        , _baseSpd  = 100
        , _baseSpc  = 55
        , _killExp  = 103
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Screech")
            , (17, "Sonicboom")
            , (22, "Selfdestruct")
            , (29, "Light Screen")
            , (36, "Swift")
            , (43, "Explosion")
            ]
        }
    , Species
        { _name     = "Electrode"
        , _expCurve = Medium
        , _type1    = Electric
        , _type2    = Nothing
        , _baseHP   = 60
        , _baseAtk  = 50
        , _baseDef  = 70
        , _baseSpd  = 140
        , _baseSpc  = 80
        , _killExp  = 150
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Screech")
            , (0, "Sonicboom")
            , (17, "Sonicboom")
            , (22, "Selfdestruct")
            , (29, "Light Screen")
            , (40, "Swift")
            , (50, "Explosion")
            ]
        }
    , Species
        { _name     = "Exeggcute"
        , _expCurve = Slow
        , _type1    = Grass
        , _type2    = Just Psychic
        , _baseHP   = 60
        , _baseAtk  = 40
        , _baseDef  = 80
        , _baseSpd  = 40
        , _baseSpc  = 60
        , _killExp  = 98
        , _learnset = mkLearnset
            [ (0, "Barrage")
            , (0, "Hypnosis")
            , (25, "Reflect")
            , (28, "Leech Seed")
            , (32, "Stun Spore")
            , (37, "Poisonpowder")
            , (42, "Solarbeam")
            , (48, "Sleep Powder")
            ]
        }
    , Species
        { _name     = "Exeggutor"
        , _expCurve = Slow
        , _type1    = Grass
        , _type2    = Just Psychic
        , _baseHP   = 95
        , _baseAtk  = 95
        , _baseDef  = 85
        , _baseSpd  = 55
        , _baseSpc  = 125
        , _killExp  = 212
        , _learnset = mkLearnset
            [ (0, "Barrage")
            , (0, "Hypnosis")
            , (28, "Stomp")
            ]
        }
    , Species
        { _name     = "Cubone"
        , _expCurve = Medium
        , _type1    = Ground
        , _type2    = Nothing
        , _baseHP   = 50
        , _baseAtk  = 50
        , _baseDef  = 95
        , _baseSpd  = 35
        , _baseSpc  = 40
        , _killExp  = 87
        , _learnset = mkLearnset
            [ (0, "Bone Club")
            , (0, "Growl")
            , (25, "Leer")
            , (31, "Focus Energy")
            , (38, "Thrash")
            , (43, "Bonemerang")
            , (46, "Rage")
            ]
        }
    , Species
        { _name     = "Marowak"
        , _expCurve = Medium
        , _type1    = Ground
        , _type2    = Nothing
        , _baseHP   = 60
        , _baseAtk  = 80
        , _baseDef  = 110
        , _baseSpd  = 45
        , _baseSpc  = 50
        , _killExp  = 124
        , _learnset = mkLearnset
            [ (0, "Bone Club")
            , (0, "Growl")
            , (0, "Leer")
            , (0, "Focus Energy")
            , (25, "Leer")
            , (33, "Focus Energy")
            , (41, "Thrash")
            , (48, "Bonemerang")
            , (55, "Rage")
            ]
        }
    , Species
        { _name     = "Hitmonlee"
        , _expCurve = Medium
        , _type1    = Fighting
        , _type2    = Nothing
        , _baseHP   = 50
        , _baseAtk  = 120
        , _baseDef  = 53
        , _baseSpd  = 87
        , _baseSpc  = 35
        , _killExp  = 139
        , _learnset = mkLearnset
            [ (0, "Double Kick")
            , (0, "Meditate")
            , (33, "Rolling Kick")
            , (38, "Jump Kick")
            , (43, "Focus Energy")
            , (48, "Hi Jump Kick")
            , (53, "Mega Kick")
            ]
        }
    , Species
        { _name     = "Hitmonchan"
        , _expCurve = Medium
        , _type1    = Fighting
        , _type2    = Nothing
        , _baseHP   = 50
        , _baseAtk  = 105
        , _baseDef  = 79
        , _baseSpd  = 76
        , _baseSpc  = 35
        , _killExp  = 140
        , _learnset = mkLearnset
            [ (0, "Comet Punch")
            , (0, "Agility")
            , (33, "Fire Punch")
            , (38, "Ice Punch")
            , (43, "Thunderpunch")
            , (48, "Mega Punch")
            , (53, "Counter")
            ]
        }
    , Species
        { _name     = "Lickitung"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 90
        , _baseAtk  = 55
        , _baseDef  = 75
        , _baseSpd  = 30
        , _baseSpc  = 60
        , _killExp  = 127
        , _learnset = mkLearnset
            [ (0, "Wrap")
            , (0, "Supersonic")
            , (7, "Stomp")
            , (15, "Disable")
            , (23, "Defense Curl")
            , (31, "Slam")
            , (39, "Screech")
            ]
        }
    , Species
        { _name     = "Koffing"
        , _expCurve = Medium
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 40
        , _baseAtk  = 65
        , _baseDef  = 95
        , _baseSpd  = 35
        , _baseSpc  = 60
        , _killExp  = 114
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Smog")
            , (32, "Sludge")
            , (37, "Smokescreen")
            , (40, "Selfdestruct")
            , (45, "Haze")
            , (48, "Explosion")
            ]
        }
    , Species
        { _name     = "Weezing"
        , _expCurve = Medium
        , _type1    = Poison
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 90
        , _baseDef  = 120
        , _baseSpd  = 60
        , _baseSpc  = 85
        , _killExp  = 173
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Smog")
            , (0, "Sludge")
            , (32, "Sludge")
            , (39, "Smokescreen")
            , (43, "Selfdestruct")
            , (49, "Haze")
            , (51, "Explosion")
            ]
        }
    , Species
        { _name     = "Rhyhorn"
        , _expCurve = Slow
        , _type1    = Ground
        , _type2    = Just Rock
        , _baseHP   = 80
        , _baseAtk  = 85
        , _baseDef  = 95
        , _baseSpd  = 25
        , _baseSpc  = 30
        , _killExp  = 135
        , _learnset = mkLearnset
            [ (0, "Horn Attack")
            , (30, "Stomp")
            , (35, "Tail Whip")
            , (40, "Fury Attack")
            , (45, "Horn Drill")
            , (50, "Leer")
            , (55, "Take Down")
            ]
        }
    , Species
        { _name     = "Rhydon"
        , _expCurve = Slow
        , _type1    = Ground
        , _type2    = Just Rock
        , _baseHP   = 105
        , _baseAtk  = 130
        , _baseDef  = 120
        , _baseSpd  = 40
        , _baseSpc  = 45
        , _killExp  = 204
        , _learnset = mkLearnset
            [ (0, "Horn Attack")
            , (0, "Stomp")
            , (0, "Tail Whip")
            , (0, "Fury Attack")
            , (30, "Stomp")
            , (35, "Tail Whip")
            , (40, "Fury Attack")
            , (48, "Horn Drill")
            , (55, "Leer")
            , (64, "Take Down")
            ]
        }
    , Species
        { _name     = "Chansey"
        , _expCurve = Fast
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 250
        , _baseAtk  = 5
        , _baseDef  = 5
        , _baseSpd  = 50
        , _baseSpc  = 105
        , _killExp  = 255
        , _learnset = mkLearnset
            [ (0, "Pound")
            , (0, "Doubleslap")
            , (24, "Sing")
            , (30, "Growl")
            , (38, "Minimize")
            , (44, "Defense Curl")
            , (48, "Light Screen")
            , (54, "Double Edge")
            ]
        }
    , Species
        { _name     = "Tangela"
        , _expCurve = Medium
        , _type1    = Grass
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 55
        , _baseDef  = 115
        , _baseSpd  = 60
        , _baseSpc  = 100
        , _killExp  = 166
        , _learnset = mkLearnset
            [ (0, "Constrict")
            , (0, "Bind")
            , (29, "Absorb")
            , (32, "Poisonpowder")
            , (36, "Stun Spore")
            , (39, "Sleep Powder")
            , (45, "Slam")
            , (49, "Growth")
            ]
        }
    , Species
        { _name     = "Kangaskhan"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 105
        , _baseAtk  = 95
        , _baseDef  = 80
        , _baseSpd  = 90
        , _baseSpc  = 40
        , _killExp  = 175
        , _learnset = mkLearnset
            [ (0, "Comet Punch")
            , (0, "Rage")
            , (26, "Bite")
            , (31, "Tail Whip")
            , (36, "Mega Punch")
            , (41, "Leer")
            , (46, "Dizzy Punch")
            ]
        }
    , Species
        { _name     = "Horsea"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 30
        , _baseAtk  = 40
        , _baseDef  = 70
        , _baseSpd  = 60
        , _baseSpc  = 70
        , _killExp  = 83
        , _learnset = mkLearnset
            [ (0, "Bubble")
            , (19, "Smokescreen")
            , (24, "Leer")
            , (30, "Water Gun")
            , (37, "Agility")
            , (45, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Seadra"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 55
        , _baseAtk  = 65
        , _baseDef  = 95
        , _baseSpd  = 85
        , _baseSpc  = 95
        , _killExp  = 155
        , _learnset = mkLearnset
            [ (0, "Bubble")
            , (0, "Smokescreen")
            , (19, "Smokescreen")
            , (24, "Leer")
            , (30, "Water Gun")
            , (41, "Agility")
            , (52, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Goldeen"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 45
        , _baseAtk  = 67
        , _baseDef  = 60
        , _baseSpd  = 63
        , _baseSpc  = 50
        , _killExp  = 111
        , _learnset = mkLearnset
            [ (0, "Peck")
            , (0, "Tail Whip")
            , (19, "Supersonic")
            , (24, "Horn Attack")
            , (30, "Fury Attack")
            , (37, "Waterfall")
            , (45, "Horn Drill")
            , (54, "Agility")
            ]
        }
    , Species
        { _name     = "Seaking"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 80
        , _baseAtk  = 92
        , _baseDef  = 65
        , _baseSpd  = 68
        , _baseSpc  = 80
        , _killExp  = 170
        , _learnset = mkLearnset
            [ (0, "Peck")
            , (0, "Tail Whip")
            , (0, "Supersonic")
            , (19, "Supersonic")
            , (24, "Horn Attack")
            , (30, "Fury Attack")
            , (39, "Waterfall")
            , (48, "Horn Drill")
            , (54, "Agility")
            ]
        }
    , Species
        { _name     = "Staryu"
        , _expCurve = Slow
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 30
        , _baseAtk  = 45
        , _baseDef  = 55
        , _baseSpd  = 85
        , _baseSpc  = 70
        , _killExp  = 106
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (17, "Water Gun")
            , (22, "Harden")
            , (27, "Recover")
            , (32, "Swift")
            , (37, "Minimize")
            , (42, "Light Screen")
            , (47, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Starmie"
        , _expCurve = Slow
        , _type1    = Water
        , _type2    = Just Psychic
        , _baseHP   = 60
        , _baseAtk  = 75
        , _baseDef  = 85
        , _baseSpd  = 115
        , _baseSpc  = 100
        , _killExp  = 207
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Water Gun")
            , (0, "Harden")
            ]
        }
    , Species
        { _name     = "MrMime"
        , _expCurve = Medium
        , _type1    = Psychic
        , _type2    = Nothing
        , _baseHP   = 40
        , _baseAtk  = 45
        , _baseDef  = 65
        , _baseSpd  = 90
        , _baseSpc  = 100
        , _killExp  = 136
        , _learnset = mkLearnset
            [ (0, "Confusion")
            , (0, "Barrier")
            , (15, "Confusion")
            , (23, "Light Screen")
            , (31, "Doubleslap")
            , (39, "Meditate")
            , (47, "Substitute")
            ]
        }
    , Species
        { _name     = "Scyther"
        , _expCurve = Medium
        , _type1    = Bug
        , _type2    = Just Flying
        , _baseHP   = 70
        , _baseAtk  = 110
        , _baseDef  = 80
        , _baseSpd  = 105
        , _baseSpc  = 55
        , _killExp  = 187
        , _learnset = mkLearnset
            [ (0, "Quick Attack")
            , (17, "Leer")
            , (20, "Focus Energy")
            , (24, "Double Team")
            , (29, "Slash")
            , (35, "Swords Dance")
            , (42, "Agility")
            ]
        }
    , Species
        { _name     = "Jynx"
        , _expCurve = Medium
        , _type1    = Ice
        , _type2    = Just Psychic
        , _baseHP   = 65
        , _baseAtk  = 50
        , _baseDef  = 35
        , _baseSpd  = 95
        , _baseSpc  = 95
        , _killExp  = 137
        , _learnset = mkLearnset
            [ (0, "Pound")
            , (0, "Lovely Kiss")
            , (18, "Lick")
            , (23, "Doubleslap")
            , (31, "Ice Punch")
            , (39, "Body Slam")
            , (47, "Thrash")
            , (58, "Blizzard")
            ]
        }
    , Species
        { _name     = "Electabuzz"
        , _expCurve = Medium
        , _type1    = Electric
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 83
        , _baseDef  = 57
        , _baseSpd  = 105
        , _baseSpc  = 85
        , _killExp  = 156
        , _learnset = mkLearnset
            [ (0, "Quick Attack")
            , (0, "Leer")
            , (34, "Thundershock")
            , (37, "Screech")
            , (42, "Thunderpunch")
            , (49, "Light Screen")
            , (54, "Thunder")
            ]
        }
    , Species
        { _name     = "Magmar"
        , _expCurve = Medium
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 95
        , _baseDef  = 57
        , _baseSpd  = 93
        , _baseSpc  = 85
        , _killExp  = 167
        , _learnset = mkLearnset
            [ (0, "Ember")
            , (36, "Leer")
            , (39, "Confuse Ray")
            , (43, "Fire Punch")
            , (48, "Smokescreen")
            , (52, "Smog")
            , (55, "Flamethrower")
            ]
        }
    , Species
        { _name     = "Pinsir"
        , _expCurve = Slow
        , _type1    = Bug
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 125
        , _baseDef  = 100
        , _baseSpd  = 85
        , _baseSpc  = 55
        , _killExp  = 200
        , _learnset = mkLearnset
            [ (0, "Vicegrip")
            , (25, "Seismic Toss")
            , (30, "Guillotine")
            , (36, "Focus Energy")
            , (43, "Harden")
            , (49, "Slash")
            , (54, "Swords Dance")
            ]
        }
    , Species
        { _name     = "Tauros"
        , _expCurve = Slow
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 75
        , _baseAtk  = 100
        , _baseDef  = 95
        , _baseSpd  = 110
        , _baseSpc  = 70
        , _killExp  = 211
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (21, "Stomp")
            , (28, "Tail Whip")
            , (35, "Leer")
            , (44, "Rage")
            , (51, "Take Down")
            ]
        }
    , Species
        { _name     = "Magikarp"
        , _expCurve = Slow
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 20
        , _baseAtk  = 10
        , _baseDef  = 55
        , _baseSpd  = 80
        , _baseSpc  = 20
        , _killExp  = 20
        , _learnset = mkLearnset
            [ (0, "Splash")
            , (15, "Tackle")
            ]
        }
    , Species
        { _name     = "Gyarados"
        , _expCurve = Slow
        , _type1    = Water
        , _type2    = Just Flying
        , _baseHP   = 95
        , _baseAtk  = 125
        , _baseDef  = 79
        , _baseSpd  = 81
        , _baseSpc  = 100
        , _killExp  = 214
        , _learnset = mkLearnset
            [ (0, "Bite")
            , (0, "Dragon Rage")
            , (0, "Leer")
            , (0, "Hydro Pump")
            , (20, "Bite")
            , (25, "Dragon Rage")
            , (32, "Leer")
            , (41, "Hydro Pump")
            , (52, "Hyper Beam")
            ]
        }
    , Species
        { _name     = "Lapras"
        , _expCurve = Slow
        , _type1    = Water
        , _type2    = Just Ice
        , _baseHP   = 130
        , _baseAtk  = 85
        , _baseDef  = 80
        , _baseSpd  = 60
        , _baseSpc  = 95
        , _killExp  = 219
        , _learnset = mkLearnset
            [ (0, "Water Gun")
            , (0, "Growl")
            , (16, "Sing")
            , (20, "Mist")
            , (25, "Body Slam")
            , (31, "Confuse Ray")
            , (38, "Ice Beam")
            , (46, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Ditto"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 48
        , _baseAtk  = 48
        , _baseDef  = 48
        , _baseSpd  = 48
        , _baseSpc  = 48
        , _killExp  = 61
        , _learnset = mkLearnset
            [ (0, "Transform")
            ]
        }
    , Species
        { _name     = "Eevee"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 55
        , _baseAtk  = 55
        , _baseDef  = 50
        , _baseSpd  = 55
        , _baseSpc  = 65
        , _killExp  = 92
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Sand Attack")
            , (27, "Quick Attack")
            , (31, "Tail Whip")
            , (37, "Bite")
            , (45, "Take Down")
            ]
        }
    , Species
        { _name     = "Vaporeon"
        , _expCurve = Medium
        , _type1    = Water
        , _type2    = Nothing
        , _baseHP   = 130
        , _baseAtk  = 65
        , _baseDef  = 60
        , _baseSpd  = 65
        , _baseSpc  = 110
        , _killExp  = 196
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Sand Attack")
            , (0, "Quick Attack")
            , (0, "Water Gun")
            , (27, "Quick Attack")
            , (31, "Water Gun")
            , (37, "Tail Whip")
            , (40, "Bite")
            , (42, "Acid Armor")
            , (44, "Haze")
            , (48, "Mist")
            , (54, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Jolteon"
        , _expCurve = Medium
        , _type1    = Electric
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 65
        , _baseDef  = 60
        , _baseSpd  = 130
        , _baseSpc  = 110
        , _killExp  = 197
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Sand Attack")
            , (0, "Quick Attack")
            , (0, "Thundershock")
            , (27, "Quick Attack")
            , (31, "Thundershock")
            , (37, "Tail Whip")
            , (40, "Thunder Wave")
            , (42, "Double Kick")
            , (44, "Agility")
            , (48, "Pin Missile")
            , (54, "Thunder")
            ]
        }
    , Species
        { _name     = "Flareon"
        , _expCurve = Medium
        , _type1    = Fire
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 130
        , _baseDef  = 60
        , _baseSpd  = 65
        , _baseSpc  = 110
        , _killExp  = 198
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Sand Attack")
            , (0, "Quick Attack")
            , (0, "Ember")
            , (27, "Quick Attack")
            , (31, "Ember")
            , (37, "Tail Whip")
            , (40, "Bite")
            , (42, "Leer")
            , (44, "Fire Spin")
            , (48, "Rage")
            , (54, "Flamethrower")
            ]
        }
    , Species
        { _name     = "Porygon"
        , _expCurve = Medium
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 65
        , _baseAtk  = 60
        , _baseDef  = 70
        , _baseSpd  = 40
        , _baseSpc  = 75
        , _killExp  = 130
        , _learnset = mkLearnset
            [ (0, "Tackle")
            , (0, "Sharpen")
            , (0, "Conversion")
            , (23, "Psybeam")
            , (28, "Recover")
            , (35, "Agility")
            , (42, "Tri Attack")
            ]
        }
    , Species
        { _name     = "Omanyte"
        , _expCurve = Medium
        , _type1    = Rock
        , _type2    = Just Water
        , _baseHP   = 35
        , _baseAtk  = 40
        , _baseDef  = 100
        , _baseSpd  = 35
        , _baseSpc  = 90
        , _killExp  = 120
        , _learnset = mkLearnset
            [ (0, "Water Gun")
            , (0, "Withdraw")
            , (34, "Horn Attack")
            , (39, "Leer")
            , (46, "Spike Cannon")
            , (53, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Omastar"
        , _expCurve = Medium
        , _type1    = Rock
        , _type2    = Just Water
        , _baseHP   = 70
        , _baseAtk  = 60
        , _baseDef  = 125
        , _baseSpd  = 55
        , _baseSpc  = 115
        , _killExp  = 199
        , _learnset = mkLearnset
            [ (0, "Water Gun")
            , (0, "Withdraw")
            , (0, "Horn Attack")
            , (34, "Horn Attack")
            , (39, "Leer")
            , (44, "Spike Cannon")
            , (49, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Kabuto"
        , _expCurve = Medium
        , _type1    = Rock
        , _type2    = Just Water
        , _baseHP   = 30
        , _baseAtk  = 80
        , _baseDef  = 90
        , _baseSpd  = 55
        , _baseSpc  = 45
        , _killExp  = 119
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Harden")
            , (34, "Absorb")
            , (39, "Slash")
            , (44, "Leer")
            , (49, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Kabutops"
        , _expCurve = Medium
        , _type1    = Rock
        , _type2    = Just Water
        , _baseHP   = 60
        , _baseAtk  = 115
        , _baseDef  = 105
        , _baseSpd  = 80
        , _baseSpc  = 70
        , _killExp  = 201
        , _learnset = mkLearnset
            [ (0, "Scratch")
            , (0, "Harden")
            , (0, "Absorb")
            , (34, "Absorb")
            , (39, "Slash")
            , (46, "Leer")
            , (53, "Hydro Pump")
            ]
        }
    , Species
        { _name     = "Aerodactyl"
        , _expCurve = Slow
        , _type1    = Rock
        , _type2    = Just Flying
        , _baseHP   = 80
        , _baseAtk  = 105
        , _baseDef  = 65
        , _baseSpd  = 130
        , _baseSpc  = 60
        , _killExp  = 202
        , _learnset = mkLearnset
            [ (0, "Wing Attack")
            , (0, "Agility")
            , (33, "Supersonic")
            , (38, "Bite")
            , (45, "Take Down")
            , (54, "Hyper Beam")
            ]
        }
    , Species
        { _name     = "Snorlax"
        , _expCurve = Slow
        , _type1    = Normal
        , _type2    = Nothing
        , _baseHP   = 160
        , _baseAtk  = 110
        , _baseDef  = 65
        , _baseSpd  = 30
        , _baseSpc  = 65
        , _killExp  = 154
        , _learnset = mkLearnset
            [ (0, "Headbutt")
            , (0, "Amnesia")
            , (0, "Rest")
            , (35, "Body Slam")
            , (41, "Harden")
            , (48, "Double Edge")
            , (56, "Hyper Beam")
            ]
        }
    , Species
        { _name     = "Articuno"
        , _expCurve = Slow
        , _type1    = Ice
        , _type2    = Just Flying
        , _baseHP   = 90
        , _baseAtk  = 85
        , _baseDef  = 100
        , _baseSpd  = 85
        , _baseSpc  = 125
        , _killExp  = 215
        , _learnset = mkLearnset
            [ (0, "Peck")
            , (0, "Ice Beam")
            , (51, "Blizzard")
            , (55, "Agility")
            , (60, "Mist")
            ]
        }
    , Species
        { _name     = "Zapdos"
        , _expCurve = Slow
        , _type1    = Electric
        , _type2    = Just Flying
        , _baseHP   = 90
        , _baseAtk  = 90
        , _baseDef  = 85
        , _baseSpd  = 100
        , _baseSpc  = 125
        , _killExp  = 216
        , _learnset = mkLearnset
            [ (0, "Thundershock")
            , (0, "Drill Peck")
            , (51, "Thunder")
            , (55, "Agility")
            , (60, "Light Screen")
            ]
        }
    , Species
        { _name     = "Moltres"
        , _expCurve = Slow
        , _type1    = Fire
        , _type2    = Just Flying
        , _baseHP   = 90
        , _baseAtk  = 100
        , _baseDef  = 90
        , _baseSpd  = 90
        , _baseSpc  = 125
        , _killExp  = 217
        , _learnset = mkLearnset
            [ (0, "Peck")
            , (0, "Fire Spin")
            , (51, "Leer")
            , (55, "Agility")
            , (60, "Sky Attack")
            ]
        }
    , Species
        { _name     = "Dratini"
        , _expCurve = Slow
        , _type1    = Dragon
        , _type2    = Nothing
        , _baseHP   = 41
        , _baseAtk  = 64
        , _baseDef  = 45
        , _baseSpd  = 50
        , _baseSpc  = 50
        , _killExp  = 67
        , _learnset = mkLearnset
            [ (0, "Wrap")
            , (0, "Leer")
            , (10, "Thunder Wave")
            , (20, "Agility")
            , (30, "Slam")
            , (40, "Dragon Rage")
            , (50, "Hyper Beam")
            ]
        }
    , Species
        { _name     = "Dragonair"
        , _expCurve = Slow
        , _type1    = Dragon
        , _type2    = Nothing
        , _baseHP   = 61
        , _baseAtk  = 84
        , _baseDef  = 65
        , _baseSpd  = 70
        , _baseSpc  = 70
        , _killExp  = 144
        , _learnset = mkLearnset
            [ (0, "Wrap")
            , (0, "Leer")
            , (0, "Thunder Wave")
            , (10, "Thunder Wave")
            , (20, "Agility")
            , (35, "Slam")
            , (45, "Dragon Rage")
            , (55, "Hyper Beam")
            ]
        }
    , Species
        { _name     = "Dragonite"
        , _expCurve = Slow
        , _type1    = Dragon
        , _type2    = Just Flying
        , _baseHP   = 91
        , _baseAtk  = 134
        , _baseDef  = 95
        , _baseSpd  = 80
        , _baseSpc  = 100
        , _killExp  = 218
        , _learnset = mkLearnset
            [ (0, "Wrap")
            , (0, "Leer")
            , (0, "Thunder Wave")
            , (0, "Agility")
            , (10, "Thunder Wave")
            , (20, "Agility")
            , (35, "Slam")
            , (45, "Dragon Rage")
            , (60, "Hyper Beam")
            ]
        }
    , Species
        { _name     = "Mewtwo"
        , _expCurve = Slow
        , _type1    = Psychic
        , _type2    = Nothing
        , _baseHP   = 106
        , _baseAtk  = 110
        , _baseDef  = 90
        , _baseSpd  = 130
        , _baseSpc  = 154
        , _killExp  = 220
        , _learnset = mkLearnset
            [ (0, "Confusion")
            , (0, "Disable")
            , (0, "Swift")
            , (0, "Psychic")
            , (63, "Barrier")
            , (66, "Psychic")
            , (70, "Recover")
            , (75, "Mist")
            , (81, "Amnesia")
            ]
        }
    , Species
        { _name     = "Mew"
        , _expCurve = MediumSlow
        , _type1    = Psychic
        , _type2    = Nothing
        , _baseHP   = 100
        , _baseAtk  = 100
        , _baseDef  = 100
        , _baseSpd  = 100
        , _baseSpc  = 100
        , _killExp  = 64
        , _learnset = mkLearnset
            [ (0, "Pound")
            , (10, "Transform")
            , (20, "Mega Punch")
            , (30, "Metronome")
            , (40, "Psychic")
            ]
        }
    ]
