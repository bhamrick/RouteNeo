{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Species where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map

import Pokemon.Experience
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
        }
    deriving (Eq, Show, Ord)

makeLenses ''Species

speciesByName :: Map String Species
speciesByName = Map.fromList (map (\s -> (s^.name, s)) allSpecies)

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
        }
    ]
