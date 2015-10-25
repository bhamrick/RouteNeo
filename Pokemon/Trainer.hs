{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Pokemon.Trainer where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map

import Pokemon.Battle
import Pokemon.Moves
import Pokemon.Species
import Pokemon.Stats

data TrainerClass
    = Youngster
    | BugCatcher
    | Lass
    | Sailor
    | JrTrainerM
    | JrTrainerF
    | Pokemaniac
    | SuperNerd
    | Hiker
    | Biker
    | Burglar
    | Engineer
    | JugglerX -- Not used
    | Fisher
    | Swimmer
    | Cueball
    | Gambler
    | Beauty
    | Psychic
    | Rocker
    | Juggler
    | Tamer
    | Birdkeeper
    | Blackbelt
    | Rival1
    | ProfOak
    | Scientist
    | Giovanni
    | Rocket
    | CoolTrainerM
    | CoolTrainerF
    | Bruno
    | Brock
    | Misty
    | LtSurge
    | Erika
    | Koga
    | Blaine
    | Sabrina
    | Gentleman
    | Rival2
    | Rival3
    | Lorelei
    | Channeler
    | Agatha
    | Lance
    deriving (Eq, Show, Ord)

data TrainerPokemon =
    TrainerPokemon
        { _tpSpecies :: Species
        , _tpLevel :: Integer
        }
    deriving (Eq, Show, Ord)

makeLenses ''TrainerPokemon

data Trainer =
    Trainer
        { _tOffset :: Integer
        , _tClass :: TrainerClass
        , _tParty :: [TrainerPokemon]
        }
    deriving (Eq, Show, Ord)

makeLenses ''Trainer

tPokemon :: Integer -> String -> TrainerPokemon
tPokemon lvl name =
    TrainerPokemon
        { _tpSpecies = speciesByName Map.! name
        , _tpLevel = lvl
        }

-- TODO: Move overrides
trainerBattleParty :: Trainer -> [Participant]
trainerBattleParty t =
    t^.tParty
        & map (\tp -> Participant
            { _species = tp^.tpSpecies
            , _level = tp^.tpLevel
            , _partyStats = computeStats (tp^.tpSpecies) (tp^.tpLevel) trainerDVs zeroStatExp
            , _battleStats = computeStats (tp^.tpSpecies) (tp^.tpLevel) trainerDVs zeroStatExp
            , _moves = defaultMoves (tp^.tpSpecies) (tp^.tpLevel)
            }
        )
        & moveOverride
    where
    moveOverride party =
        case t^.tOffset of
            -- Brock
            0x3A3B5 ->
                party & _last . moves %~ flip snoc (movesByName Map.! "Bide")
            -- Misty
            0x3A3BB ->
                party & _last . moves %~ flip snoc (movesByName Map.! "Bubblebeam")
            -- Surge
            0x3A3C1 ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Thunderbolt")
            -- Erika
            0x3A3C9 ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Mega Drain")
            -- Koga
            0x3A3D1 ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Toxic")
            -- Sabrina
            0x3A3E5 ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Psywave")
            -- Blaine
            0x3A3DB ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Fire Blast")
            -- Giovanni (Gym)
            0x3A290 ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Fissure")
            -- Lorelei
            0x3A4BB ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Blizzard")
            -- Bruno
            0x3A3A9 ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Fissure")
            -- Agatha
            0x3A516 ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Toxic")
            -- Lance
            0x3A522 ->
                party & _last . moves . ix 2 .~ (movesByName Map.! "Barrier")
            -- Champion
            0x3A491 ->
                party
                    & ix 0 . moves . ix 2 .~ (movesByName Map.! "Sky Attack")
                    & ix 5 . moves . ix 2 .~ (movesByName Map.! "Blizzard")
            0x3A49F ->
                party
                    & ix 0 . moves . ix 2 .~ (movesByName Map.! "Sky Attack")
                    & ix 5 . moves . ix 2 .~ (movesByName Map.! "Mega Drain")
            0x3A4AD ->
                party
                    & ix 0 . moves . ix 2 .~ (movesByName Map.! "Sky Attack")
                    & ix 5 . moves . ix 2 .~ (movesByName Map.! "Fire Blast")
            _ -> party

trainer :: Integer -> TrainerClass -> [(Integer, String)] -> Trainer
trainer offset cls party =
    Trainer
        { _tOffset = offset
        , _tClass = cls
        , _tParty = map (uncurry tPokemon) party
        }

trainersByOffset :: Map Integer Trainer
trainersByOffset = Map.fromList (map (\t -> (t^.tOffset, t)) allTrainers)

allTrainers :: [Trainer]
allTrainers =
    [ trainer 0x39D99 Youngster [(11, "Rattata"), (11, "Ekans")]
    , trainer 0x39D9D Youngster [(14, "Spearow")]
    , trainer 0x39DA0 Youngster [(10, "Rattata"), (10, "Rattata"), (10, "Zubat")]
    , trainer 0x39DA5 Youngster [(14, "Rattata"), (14, "Ekans"), (14, "Zubat")]
    , trainer 0x39DAA Youngster [(15, "Rattata"), (15, "Spearow")]
    , trainer 0x39DAE Youngster [(17, "Slowpoke")]
    , trainer 0x39DB1 Youngster [(14, "Ekans"), (14, "Sandshrew")]
    , trainer 0x39DB5 Youngster [(21, "NidoranM")]
    , trainer 0x39DB8 Youngster [(21, "Ekans")]
    , trainer 0x39DBB Youngster [(19, "Sandshrew"), (19, "Zubat")]
    , trainer 0x39DBF Youngster [(17, "Rattata"), (17, "Rattata"), (17, "Raticate")]
    , trainer 0x39DC4 Youngster [(18, "NidoranM"), (18, "Nidorino")]
    , trainer 0x39DC8 Youngster [(17, "Spearow"), (17, "Rattata"), (17, "Rattata"), (17, "Spearow")]
    , trainer 0x39DCE BugCatcher [(6, "Weedle"), (6, "Caterpie")]
    , trainer 0x39DD2 BugCatcher [(7, "Weedle"), (7, "Kakuna"), (7, "Weedle")]
    , trainer 0x39DD7 BugCatcher [(9, "Weedle")]
    , trainer 0x39DDA BugCatcher [(10, "Caterpie"), (10, "Weedle"), (10, "Caterpie")]
    , trainer 0x39DDF BugCatcher [(9, "Weedle"), (9, "Kakuna"), (9, "Caterpie"), (9, "Metapod")]
    , trainer 0x39DE5 BugCatcher [(11, "Caterpie"), (11, "Metapod")]
    , trainer 0x39DE9 BugCatcher [(11, "Weedle"), (11, "Kakuna")]
    , trainer 0x39DED BugCatcher [(10, "Caterpie"), (10, "Metapod"), (10, "Caterpie")]
    , trainer 0x39DF2 BugCatcher [(14, "Caterpie"), (14, "Weedle")]
    , trainer 0x39DF6 BugCatcher [(16, "Weedle"), (16, "Caterpie"), (16, "Weedle")]
    , trainer 0x39DFB BugCatcher [(20, "Butterfree")]
    , trainer 0x39DFE BugCatcher [(18, "Metapod"), (18, "Caterpie"), (18, "Venonat")]
    , trainer 0x39E03 BugCatcher [(19, "Beedrill"), (19, "Beedrill")]
    , trainer 0x39E07 BugCatcher [(20, "Caterpie"), (20, "Weedle"), (20, "Venonat")]
    , trainer 0x39E0C Lass [(9, "Pidgey"), (9, "Pidgey")]
    , trainer 0x39E10 Lass [(10, "Rattata"), (10, "NidoranM")]
    , trainer 0x39E14 Lass [(14, "Jigglypuff")]
    , trainer 0x39E17 Lass [(31, "Paras"), (31, "Paras"), (31, "Parasect")]
    , trainer 0x39E1C Lass [(11, "Oddish"), (11, "Bellsprout")]
    , trainer 0x39E20 Lass [(14, "Clefairy")]
    , trainer 0x39E23 Lass [(16, "Pidgey"), (16, "NidoranF")]
    , trainer 0x39E27 Lass [(14, "Pidgey"), (14, "NidoranF")]
    , trainer 0x39E2B Lass [(15, "NidoranM"), (15, "NidoranF")]
    , trainer 0x39E2F Lass [(13, "Oddish"), (13, "Pidgey"), (13, "Oddish")]
    , trainer 0x39E34 Lass [(18, "Pidgey"), (18, "NidoranF")]
    , trainer 0x39E38 Lass [(18, "Rattata"), (18, "Pikachu")]
    , trainer 0x39E3C Lass [(23, "NidoranF"), (23, "Nidorina")]
    , trainer 0x39E40 Lass [(24, "Meowth"), (24, "Meowth"), (24, "Meowth")]
    , trainer 0x39E45 Lass [(19, "Pidgey"), (19, "Rattata"), (19, "NidoranM"), (19, "Meowth"), (19, "Pikachu")]
    , trainer 0x39E4C Lass [(22, "Clefairy"), (22, "Clefairy")]
    , trainer 0x39E50 Lass [(23, "Bellsprout"), (23, "Weepinbell")]
    , trainer 0x39E54 Lass [(23, "Oddish"), (23, "Gloom")]
    , trainer 0x39E58 Sailor [(18, "Machop"), (18, "Shellder")]
    , trainer 0x39E5C Sailor [(17, "Machop"), (17, "Tentacool")]
    , trainer 0x39E60 Sailor [(21, "Shellder")]
    , trainer 0x39E63 Sailor [(17, "Horsea"), (17, "Shellder"), (17, "Tentacool")]
    , trainer 0x39E68 Sailor [(18, "Tentacool"), (18, "Staryu")]
    , trainer 0x39E6C Sailor [(17, "Horsea"), (17, "Horsea"), (17, "Horsea")]
    , trainer 0x39E71 Sailor [(20, "Machop")]
    , trainer 0x39E74 Sailor [(21, "Pikachu"), (21, "Pikachu")]
    , trainer 0x39E78 JrTrainerM [(11, "Diglett"), (11, "Sandshrew")]
    , trainer 0x39E7C JrTrainerM [(14, "Rattata"), (14, "Ekans")]
    , trainer 0x39E80 JrTrainerM [(18, "Mankey")]
    , trainer 0x39E83 JrTrainerM [(20, "Squirtle")]
    , trainer 0x39E86 JrTrainerM [(16, "Spearow"), (16, "Raticate")]
    , trainer 0x39E8A JrTrainerM [(18, "Diglett"), (18, "Diglett"), (18, "Sandshrew")]
    , trainer 0x39E8F JrTrainerM [(21, "Growlithe"), (21, "Charmander")]
    , trainer 0x39E93 JrTrainerM [(19, "Rattata"), (19, "Diglett"), (19, "Ekans"), (19, "Sandshrew")]
    , trainer 0x39E99 JrTrainerM [(29, "NidoranM"), (29, "Nidorino")]
    , trainer 0x39E9D JrTrainerF [(19, "Goldeen")]
    , trainer 0x39EA0 JrTrainerF [(16, "Rattata"), (16, "Pikachu")]
    , trainer 0x39EA4 JrTrainerF [(16, "Pidgey"), (16, "Pidgey"), (16, "Pidgey")]
    , trainer 0x39EA9 JrTrainerF [(22, "Bulbasaur")]
    , trainer 0x39EAC JrTrainerF [(18, "Oddish"), (18, "Bellsprout"), (18, "Oddish"), (18, "Bellsprout")]
    , trainer 0x39EB2 JrTrainerF [(23, "Meowth")]
    , trainer 0x39EB5 JrTrainerF [(20, "Pikachu"), (20, "Clefairy")]
    , trainer 0x39EB9 JrTrainerF [(21, "Pidgey"), (21, "Pidgeotto")]
    , trainer 0x39EBD JrTrainerF [(21, "Jigglypuff"), (21, "Pidgey"), (21, "Meowth")]
    , trainer 0x39EC2 JrTrainerF [(22, "Oddish"), (22, "Bulbasaur")]
    , trainer 0x39EC6 JrTrainerF [(24, "Bulbasaur"), (24, "Ivysaur")]
    , trainer 0x39ECA JrTrainerF [(24, "Pidgey"), (24, "Meowth"), (24, "Rattata"), (24, "Pikachu"), (24, "Meowth")]
    , trainer 0x39ED1 JrTrainerF [(30, "Poliwag"), (30, "Poliwag")]
    , trainer 0x39ED5 JrTrainerF [(27, "Pidgey"), (27, "Meowth"), (27, "Pidgey"), (27, "Pidgeotto")]
    , trainer 0x39ED8 JrTrainerF [(28, "Goldeen"), (28, "Poliwag"), (28, "Horsea")]
    , trainer 0x39EE0 JrTrainerF [(31, "Goldeen"), (31, "Seaking")]
    , trainer 0x39EE4 JrTrainerF [(22, "Bellsprout"), (22, "Clefairy")]
    , trainer 0x39EE8 JrTrainerF [(20, "Meowth"), (20, "Oddish"), (20, "Pidgey")]
    , trainer 0x39EED JrTrainerF [(19, "Pidgey"), (19, "Rattata"), (19, "Rattata"), (19, "Bellsprout")]
    , trainer 0x39EF3 JrTrainerF [(28, "Gloom"), (28, "Oddish"), (28, "Oddish")]
    , trainer 0x39EF8 JrTrainerF [(29, "Pikachu"), (29, "Raichu")]
    , trainer 0x39EFC JrTrainerF [(33, "Clefairy")]
    , trainer 0x39EFF JrTrainerF [(29, "Bellsprout"), (29, "Oddish"), (29, "Tangela")]
    , trainer 0x39F04 JrTrainerF [(30, "Tentacool"), (30, "Horsea"), (30, "Seel")]
    , trainer 0x39F09 Pokemaniac [(30, "Rhyhorn"), (30, "Lickitung")]
    , trainer 0x39F0D Pokemaniac [(20, "Cubone"), (20, "Slowpoke")]
    , trainer 0x39F11 Pokemaniac [(20, "Slowpoke"), (20, "Slowpoke"), (20, "Slowpoke")]
    , trainer 0x39F16 Pokemaniac [(22, "Charmander"), (22, "Cubone")]
    , trainer 0x39F1A Pokemaniac [(25, "Slowpoke")]
    , trainer 0x39F1D Pokemaniac [(40, "Charmeleon"), (40, "Lapras"), (40, "Lickitung")]
    , trainer 0x39F22 Pokemaniac [(23, "Cubone"), (23, "Slowpoke")]
    , trainer 0x39F26 SuperNerd [(11, "Magnemite"), (11, "Voltorb")]
    , trainer 0x39F2A SuperNerd [(12, "Grimer"), (12, "Voltorb"), (12, "Koffing")]
    , trainer 0x39F2F SuperNerd [(20, "Voltorb"), (20, "Koffing"), (20, "Voltorb"), (20, "Magnemite")]
    , trainer 0x39F35 SuperNerd [(22, "Grimer"), (22, "Muk"), (22, "Grimer")]
    , trainer 0x39F3A SuperNerd [(26, "Koffing")]
    , trainer 0x39F3D SuperNerd [(22, "Koffing"), (22, "Magnemite"), (22, "Weezing")]
    , trainer 0x39F42 SuperNerd [(20, "Magnemite"), (20, "Magnemite"), (20, "Koffing"), (20, "Magnemite")]
    , trainer 0x39F48 SuperNerd [(24, "Magnemite"), (24, "Voltorb")]
    , trainer 0x39F4C SuperNerd [(36, "Vulpix"), (36, "Vulpix"), (36, "Ninetales")]
    , trainer 0x39F51 SuperNerd [(34, "Ponyta"), (34, "Charmander"), (34, "Vulpix"), (34, "Growlithe")]
    , trainer 0x39F57 SuperNerd [(41, "Rapidash")]
    , trainer 0x39F5A SuperNerd [(37, "Growlithe"), (37, "Vulpix")]
    , trainer 0x39F5E Hiker [(10, "Geodude"), (10, "Geodude"), (10, "Onix")]
    , trainer 0x39F63 Hiker [(15, "Machop"), (15, "Geodude")]
    , trainer 0x39F67 Hiker [(13, "Geodude"), (13, "Geodude"), (13, "Machop"), (13, "Geodude")]
    , trainer 0x39F6D Hiker [(17, "Onix")]
    , trainer 0x39F70 Hiker [(21, "Geodude"), (21, "Onix")]
    , trainer 0x39F74 Hiker [(20, "Geodude"), (20, "Machop"), (20, "Geodude")]
    , trainer 0x39F79 Hiker [(21, "Geodude"), (21, "Onix")]
    , trainer 0x39F7D Hiker [(19, "Onix"), (19, "Graveler")]
    , trainer 0x39F81 Hiker [(21, "Geodude"), (21, "Geodude"), (21, "Graveler")]
    , trainer 0x39F86 Hiker [(25, "Geodude")]
    , trainer 0x39F89 Hiker [(20, "Machop"), (20, "Onix")]
    , trainer 0x39F8D Hiker [(19, "Geodude"), (19, "Machop"), (19, "Geodude"), (19, "Geodude")]
    , trainer 0x39F93 Hiker [(20, "Onix"), (20, "Onix"), (20, "Geodude")]
    , trainer 0x39F98 Hiker [(21, "Geodude"), (21, "Graveler")]
    , trainer 0x39F9C Biker [(28, "Koffing"), (28, "Koffing"), (28, "Koffing")]
    , trainer 0x39FA1 Biker [(29, "Koffing"), (29, "Grimer")]
    , trainer 0x39FA5 Biker [(25, "Koffing"), (25, "Koffing"), (25, "Weezing"), (25, "Koffing"), (25, "Grimer")]
    , trainer 0x39FAC Biker [(28, "Koffing"), (28, "Grimer"), (28, "Weezing")]
    , trainer 0x39FB1 Biker [(29, "Grimer"), (29, "Koffing")]
    , trainer 0x39FB5 Biker [(33, "Weezing")]
    , trainer 0x39FB8 Biker [(26, "Grimer"), (26, "Grimer"), (26, "Grimer"), (26, "Grimer")]
    , trainer 0x39FBE Biker [(28, "Weezing"), (28, "Koffing"), (28, "Weezing")]
    , trainer 0x39FC3 Biker [(33, "Muk")]
    , trainer 0x39FC6 Biker [(29, "Voltorb"), (29, "Voltorb")]
    , trainer 0x39FCA Biker [(29, "Weezing"), (29, "Muk")]
    , trainer 0x39FCE Biker [(25, "Koffing"), (25, "Weezing"), (25, "Koffing"), (25, "Koffing"), (25, "Weezing")]
    , trainer 0x39FD5 Biker [(26, "Koffing"), (26, "Koffing"), (26, "Grimer"), (26, "Koffing")]
    , trainer 0x39FD8 Biker [(28, "Grimer"), (28, "Grimer"), (28, "Koffing")]
    , trainer 0x39FE0 Biker [(29, "Koffing"), (29, "Muk")]
    , trainer 0x39FE4 Burglar [(29, "Growlithe"), (29, "Vulpix")]
    , trainer 0x39FE8 Burglar [(33, "Growlithe")]
    , trainer 0x39FEB Burglar [(28, "Vulpix"), (28, "Charmander"), (28, "Ponyta")]
    , trainer 0x39FF0 Burglar [(36, "Growlithe"), (36, "Vulpix"), (36, "Ninetales")]
    , trainer 0x39FF5 Burglar [(41, "Ponyta")]
    , trainer 0x39FF8 Burglar [(37, "Vulpix"), (37, "Growlithe")]
    , trainer 0x39FFC Burglar [(34, "Charmander"), (34, "Charmeleon")]
    , trainer 0x3A000 Burglar [(38, "Ninetales")]
    , trainer 0x3A003 Burglar [(34, "Growlithe"), (34, "Ponyta")]
    , trainer 0x3A007 Engineer [(21, "Voltorb"), (21, "Magnemite")]
    , trainer 0x3A00B Engineer [(21, "Magnemite")]
    , trainer 0x3A00E Engineer [(18, "Magnemite"), (18, "Magnemite"), (18, "Magneton")]
    , trainer 0x3A013 Fisher [(17, "Goldeen"), (17, "Tentacool"), (17, "Goldeen")]
    , trainer 0x3A018 Fisher [(17, "Tentacool"), (17, "Staryu"), (17, "Shellder")]
    , trainer 0x3A01D Fisher [(22, "Goldeen"), (22, "Poliwag"), (22, "Goldeen")]
    , trainer 0x3A022 Fisher [(24, "Tentacool"), (24, "Goldeen")]
    , trainer 0x3A026 Fisher [(27, "Goldeen")]
    , trainer 0x3A029 Fisher [(21, "Poliwag"), (21, "Shellder"), (21, "Goldeen"), (21, "Horsea")]
    , trainer 0x3A02F Fisher [(28, "Seaking"), (28, "Goldeen"), (28, "Seaking"), (28, "Seaking")]
    , trainer 0x3A035 Fisher [(31, "Shellder"), (31, "Cloyster")]
    , trainer 0x3A039 Fisher [(27, "Magikarp"), (27, "Magikarp"), (27, "Magikarp"), (27, "Magikarp"), (27, "Magikarp")]
    , trainer 0x3A041 Fisher [(33, "Seaking"), (33, "Goldeen")]
    , trainer 0x3A045 Fisher [(24, "Magikarp"), (24, "Magikarp")]
    , trainer 0x3A049 Swimmer [(16, "Horsea"), (16, "Shellder")]
    , trainer 0x3A04D Swimmer [(30, "Tentacool"), (30, "Shellder")]
    , trainer 0x3A051 Swimmer [(29, "Goldeen"), (29, "Horsea"), (29, "Staryu")]
    , trainer 0x3A056 Swimmer [(30, "Poliwag"), (30, "Poliwhirl")]
    , trainer 0x3A05A Swimmer [(27, "Horsea"), (27, "Tentacool"), (27, "Tentacool"), (27, "Goldeen")]
    , trainer 0x3A060 Swimmer [(29, "Goldeen"), (29, "Shellder"), (29, "Seaking")]
    , trainer 0x3A065 Swimmer [(30, "Horsea"), (30, "Horsea")]
    , trainer 0x3A069 Swimmer [(27, "Tentacool"), (27, "Tentacool"), (27, "Staryu"), (27, "Horsea"), (27, "Tentacool")]
    , trainer 0x3A070 Swimmer [(31, "Shellder"), (31, "Cloyster")]
    , trainer 0x3A074 Swimmer [(35, "Staryu")]
    , trainer 0x3A077 Swimmer [(28, "Horsea"), (28, "Horsea"), (28, "Seadra"), (28, "Horsea")]
    , trainer 0x3A07D Swimmer [(33, "Seadra"), (33, "Tentacruel")]
    , trainer 0x3A081 Swimmer [(37, "Starmie")]
    , trainer 0x3A084 Swimmer [(33, "Staryu"), (33, "Wartortle")]
    , trainer 0x3A088 Swimmer [(32, "Poliwhirl"), (32, "Tentacool"), (32, "Seadra")]
    , trainer 0x3A08D Cueball [(28, "Machop"), (28, "Mankey"), (28, "Machop")]
    , trainer 0x3A092 Cueball [(29, "Mankey"), (29, "Machop")]
    , trainer 0x3A096 Cueball [(33, "Machop")]
    , trainer 0x3A099 Cueball [(29, "Mankey"), (29, "Primeape")]
    , trainer 0x3A09D Cueball [(29, "Machop"), (29, "Machoke")]
    , trainer 0x3A0A1 Cueball [(33, "Machoke")]
    , trainer 0x3A0A4 Cueball [(26, "Mankey"), (26, "Mankey"), (26, "Machoke"), (26, "Machop")]
    , trainer 0x3A0AA Cueball [(29, "Primeape"), (29, "Machoke")]
    , trainer 0x3A0AE Cueball [(31, "Tentacool"), (31, "Tentacool"), (31, "Tentacruel")]
    , trainer 0x3A0B3 Gambler [(18, "Poliwag"), (18, "Horsea")]
    , trainer 0x3A0B7 Gambler [(18, "Bellsprout"), (18, "Oddish")]
    , trainer 0x3A0BB Gambler [(18, "Voltorb"), (18, "Magnemite")]
    , trainer 0x3A0BF Gambler [(18, "Growlithe"), (18, "Vulpix")]
    , trainer 0x3A0C3 Gambler [(22, "Poliwag"), (22, "Poliwag"), (22, "Poliwhirl")]
    , trainer 0x3A0C8 Gambler [(22, "Onix"), (22, "Geodude"), (22, "Graveler")]
    , trainer 0x3A0CD Gambler [(24, "Growlithe"), (24, "Vulpix")]
    , trainer 0x3A0D1 Beauty [(21, "Oddish"), (21, "Bellsprout"), (21, "Oddish"), (21, "Bellsprout")]
    , trainer 0x3A0D7 Beauty [(24, "Bellsprout"), (24, "Bellsprout")]
    , trainer 0x3A0DB Beauty [(26, "Exeggcute")]
    , trainer 0x3A0DE Beauty [(27, "Rattata"), (27, "Pikachu"), (27, "Rattata")]
    , trainer 0x3A0E3 Beauty [(29, "Clefairy"), (29, "Meowth")]
    , trainer 0x3A0E7 Beauty [(35, "Seaking")]
    , trainer 0x3A0EA Beauty [(30, "Shellder"), (30, "Shellder"), (30, "Cloyster")]
    , trainer 0x3A0EF Beauty [(31, "Poliwag"), (31, "Seaking")]
    , trainer 0x3A0F3 Beauty [(29, "Pidgeotto"), (29, "Wigglytuff")]
    , trainer 0x3A0F7 Beauty [(29, "Bulbasaur"), (29, "Ivysaur")]
    , trainer 0x3A0FB Beauty [(33, "Weepinbell"), (33, "Bellsprout"), (33, "Weepinbell")]
    , trainer 0x3A100 Beauty [(27, "Poliwag"), (27, "Goldeen"), (27, "Seaking"), (27, "Goldeen"), (27, "Poliwag")]
    , trainer 0x3A107 Beauty [(30, "Goldeen"), (30, "Seaking")]
    , trainer 0x3A10B Beauty [(29, "Staryu"), (29, "Staryu"), (29, "Staryu")]
    , trainer 0x3A110 Beauty [(30, "Seadra"), (30, "Horsea"), (30, "Seadra")]
    , trainer 0x3A115 Psychic [(31, "Kadabra"), (31, "Slowpoke"), (31, "MrMime"), (31, "Kadabra")]
    , trainer 0x3A11B Psychic [(34, "MrMime"), (34, "Kadabra")]
    , trainer 0x3A11F Psychic [(33, "Slowpoke"), (33, "Slowpoke"), (33, "Slowbro")]
    , trainer 0x3A124 Psychic [(39, "Slowbro")]
    , trainer 0x3A127 Rocker [(20, "Voltorb"), (20, "Magnemite"), (20, "Voltorb")]
    , trainer 0x3A12C Rocker [(29, "Voltorb"), (29, "Electrode")]
    , trainer 0x3A130 Juggler [(29, "Kadabra"), (29, "MrMime")]
    , trainer 0x3A134 Juggler [(41, "Drowzee"), (41, "Hypno"), (41, "Kadabra"), (41, "Kadabra")]
    , trainer 0x3A13A Juggler [(31, "Drowzee"), (31, "Drowzee"), (31, "Kadabra"), (31, "Drowzee")]
    , trainer 0x3A140 Juggler [(34, "Drowzee"), (34, "Hypno")]
    , trainer 0x3A144 Juggler [(48, "MrMime")]
    , trainer 0x3A147 Juggler [(33, "Hypno")]
    , trainer 0x3A14A Juggler [(38, "Hypno")]
    , trainer 0x3A14D Juggler [(34, "Drowzee"), (34, "Kadabra")]
    , trainer 0x3A151 Tamer [(34, "Sandslash"), (34, "Arbok")]
    , trainer 0x3A155 Tamer [(33, "Arbok"), (33, "Sandslash"), (33, "Arbok")]
    , trainer 0x3A15A Tamer [(43, "Rhyhorn")]
    , trainer 0x3A15D Tamer [(39, "Arbok"), (39, "Tauros")]
    , trainer 0x3A161 Tamer [(44, "Persian"), (44, "Golduck")]
    , trainer 0x3A165 Tamer [(42, "Rhyhorn"), (42, "Primeape"), (42, "Arbok"), (42, "Tauros")]
    , trainer 0x3A16B Birdkeeper [(29, "Pidgey"), (29, "Pidgeotto")]
    , trainer 0x3A16F Birdkeeper [(25, "Spearow"), (25, "Pidgey"), (25, "Pidgey"), (25, "Spearow"), (25, "Spearow")]
    , trainer 0x3A176 Birdkeeper [(26, "Pidgey"), (26, "Pidgeotto"), (26, "Spearow"), (26, "Fearow")]
    , trainer 0x3A17C Birdkeeper [(33, "Farfetchd")]
    , trainer 0x3A17F Birdkeeper [(29, "Spearow"), (29, "Fearow")]
    , trainer 0x3A183 Birdkeeper [(26, "Pidgeotto"), (26, "Farfetchd"), (26, "Doduo"), (26, "Pidgey")]
    , trainer 0x3A189 Birdkeeper [(28, "Dodrio"), (28, "Doduo"), (28, "Doduo")]
    , trainer 0x3A18E Birdkeeper [(29, "Spearow"), (29, "Fearow")]
    , trainer 0x3A192 Birdkeeper [(34, "Dodrio")]
    , trainer 0x3A195 Birdkeeper [(26, "Spearow"), (26, "Spearow"), (26, "Fearow"), (26, "Spearow")]
    , trainer 0x3A198 Birdkeeper [(30, "Fearow"), (30, "Fearow"), (30, "Pidgeotto")]
    , trainer 0x3A1A0 Birdkeeper [(39, "Pidgeotto"), (39, "Pidgeotto"), (39, "Pidgey"), (39, "Pidgeotto")]
    , trainer 0x3A1A6 Birdkeeper [(42, "Farfetchd"), (42, "Fearow")]
    , trainer 0x3A1AA Birdkeeper [(28, "Pidgey"), (28, "Doduo"), (28, "Pidgeotto")]
    , trainer 0x3A1AF Birdkeeper [(26, "Pidgey"), (26, "Spearow"), (26, "Pidgey"), (26, "Fearow")]
    , trainer 0x3A1B5 Birdkeeper [(29, "Pidgeotto"), (29, "Fearow")]
    , trainer 0x3A1B9 Birdkeeper [(28, "Spearow"), (28, "Doduo"), (28, "Fearow")]
    , trainer 0x3A1BE Blackbelt [(37, "Hitmonlee"), (37, "Hitmonchan")]
    , trainer 0x3A1C2 Blackbelt [(31, "Mankey"), (31, "Mankey"), (31, "Primeape")]
    , trainer 0x3A1C7 Blackbelt [(32, "Machop"), (32, "Machoke")]
    , trainer 0x3A1CB Blackbelt [(36, "Primeape")]
    , trainer 0x3A1CE Blackbelt [(31, "Machop"), (31, "Mankey"), (31, "Primeape")]
    , trainer 0x3A1D3 Blackbelt [(40, "Machop"), (40, "Machoke")]
    , trainer 0x3A1D7 Blackbelt [(43, "Machoke")]
    , trainer 0x3A1DA Blackbelt [(38, "Machoke"), (38, "Machop"), (38, "Machoke")]
    , trainer 0x3A1DF Blackbelt [(43, "Machoke"), (43, "Machop"), (43, "Machoke")]
    , trainer 0x3A1E4 Rival1 [(5, "Squirtle")]
    , trainer 0x3A1E7 Rival1 [(5, "Bulbasaur")]
    , trainer 0x3A1EA Rival1 [(5, "Charmander")]
    , trainer 0x3A1ED Rival1 [(9, "Pidgey"), (8, "Squirtle")]
    , trainer 0x3A1F3 Rival1 [(9, "Pidgey"), (8, "Bulbasaur")]
    , trainer 0x3A1F9 Rival1 [(9, "Pidgey"), (8, "Charmander")]
    , trainer 0x3A1FF Rival1 [(18, "Pidgeotto"), (15, "Abra"), (15, "Rattata"), (17, "Squirtle")]
    , trainer 0x3A209 Rival1 [(18, "Pidgeotto"), (15, "Abra"), (15, "Rattata"), (17, "Bulbasaur")]
    , trainer 0x3A213 Rival1 [(18, "Pidgeotto"), (15, "Abra"), (15, "Rattata"), (17, "Charmander")]
    , trainer 0x3A21D ProfOak [(66, "Tauros"), (67, "Exeggutor"), (68, "Arcanine"), (69, "Blastoise"), (70, "Gyarados")]
    , trainer 0x3A229 ProfOak [(66, "Tauros"), (67, "Exeggutor"), (68, "Arcanine"), (69, "Venusaur"), (70, "Gyarados")]
    , trainer 0x3A235 ProfOak [(66, "Tauros"), (67, "Exeggutor"), (68, "Arcanine"), (69, "Charizard"), (70, "Gyarados")]
    , trainer 0x3A241 Scientist [(34, "Koffing"), (34, "Voltorb")]
    , trainer 0x3A245 Scientist [(26, "Grimer"), (26, "Weezing"), (26, "Koffing"), (26, "Weezing")]
    , trainer 0x3A24B Scientist [(28, "Magnemite"), (28, "Voltorb"), (28, "Magneton")]
    , trainer 0x3A250 Scientist [(29, "Electrode"), (29, "Weezing")]
    , trainer 0x3A254 Scientist [(33, "Electrode")]
    , trainer 0x3A257 Scientist [(26, "Magneton"), (26, "Koffing"), (26, "Weezing"), (26, "Magnemite")]
    , trainer 0x3A25D Scientist [(25, "Voltorb"), (25, "Koffing"), (25, "Magneton"), (25, "Magnemite"), (25, "Koffing")]
    , trainer 0x3A264 Scientist [(29, "Electrode"), (29, "Muk")]
    , trainer 0x3A268 Scientist [(29, "Grimer"), (29, "Electrode")]
    , trainer 0x3A26C Scientist [(28, "Voltorb"), (28, "Koffing"), (28, "Magneton")]
    , trainer 0x3A271 Scientist [(29, "Magnemite"), (29, "Koffing")]
    , trainer 0x3A275 Scientist [(33, "Magnemite"), (33, "Magneton"), (33, "Voltorb")]
    , trainer 0x3A27A Scientist [(34, "Magnemite"), (34, "Electrode")]
    , trainer 0x3A27E Giovanni [(25, "Onix"), (24, "Rhyhorn"), (29, "Kangaskhan")]
    , trainer 0x3A286 Giovanni [(37, "Nidorino"), (25, "Kangaskhan"), (37, "Rhyhorn"), (41, "Nidoqueen")]
    , trainer 0x3A290 Giovanni [(45, "Rhyhorn"), (42, "Dugtrio"), (44, "Nidoqueen"), (45, "Nidoking"), (50, "Rhydon")]
    , trainer 0x3A29C Rocket [(13, "Rattata"), (13, "Zubat")]
    , trainer 0x3A2A0 Rocket [(11, "Sandshrew"), (11, "Rattata"), (11, "Zubat")]
    , trainer 0x3A2A5 Rocket [(12, "Zubat"), (12, "Ekans")]
    , trainer 0x3A2A9 Rocket [(16, "Raticate")]
    , trainer 0x3A2AC Rocket [(17, "Machop"), (17, "Drowzee")]
    , trainer 0x3A2B0 Rocket [(15, "Ekans"), (15, "Zubat")]
    , trainer 0x3A2B4 Rocket [(20, "Raticate"), (20, "Zubat")]
    , trainer 0x3A2B8 Rocket [(21, "Drowzee"), (21, "Machop")]
    , trainer 0x3A2BC Rocket [(21, "Raticate"), (21, "Raticate")]
    , trainer 0x3A2C0 Rocket [(20, "Grimer"), (20, "Koffing"), (20, "Koffing")]
    , trainer 0x3A2C5 Rocket [(19, "Rattata"), (19, "Raticate"), (19, "Raticate"), (19, "Rattata")]
    , trainer 0x3A2CB Rocket [(22, "Grimer"), (22, "Koffing")]
    , trainer 0x3A2CF Rocket [(17, "Zubat"), (17, "Koffing"), (17, "Grimer"), (17, "Zubat"), (17, "Raticate")]
    , trainer 0x3A2D6 Rocket [(20, "Rattata"), (20, "Raticate"), (20, "Drowzee")]
    , trainer 0x3A2DB Rocket [(21, "Machop"), (21, "Machop")]
    , trainer 0x3A2DF Rocket [(23, "Sandshrew"), (23, "Ekans"), (23, "Sandslash")]
    , trainer 0x3A2E4 Rocket [(23, "Ekans"), (23, "Sandshrew"), (23, "Arbok")]
    , trainer 0x3A2E9 Rocket [(21, "Koffing"), (21, "Zubat")]
    , trainer 0x3A2ED Rocket [(25, "Zubat"), (25, "Zubat"), (25, "Golbat")]
    , trainer 0x3A2F2 Rocket [(26, "Koffing"), (26, "Drowzee")]
    , trainer 0x3A2F6 Rocket [(23, "Zubat"), (23, "Rattata"), (23, "Raticate"), (23, "Zubat")]
    , trainer 0x3A2FC Rocket [(26, "Drowzee"), (26, "Koffing")]
    , trainer 0x3A300 Rocket [(29, "Cubone"), (29, "Zubat")]
    , trainer 0x3A304 Rocket [(25, "Golbat"), (25, "Zubat"), (25, "Zubat"), (25, "Raticate"), (25, "Zubat")]
    , trainer 0x3A30B Rocket [(28, "Raticate"), (28, "Hypno"), (28, "Raticate")]
    , trainer 0x3A310 Rocket [(29, "Machop"), (29, "Drowzee")]
    , trainer 0x3A314 Rocket [(28, "Ekans"), (28, "Zubat"), (28, "Cubone")]
    , trainer 0x3A319 Rocket [(33, "Arbok")]
    , trainer 0x3A31C Rocket [(33, "Hypno")]
    , trainer 0x3A31F Rocket [(29, "Machop"), (29, "Machoke")]
    , trainer 0x3A323 Rocket [(28, "Zubat"), (28, "Zubat"), (28, "Golbat")]
    , trainer 0x3A328 Rocket [(26, "Raticate"), (26, "Arbok"), (26, "Koffing"), (26, "Golbat")]
    , trainer 0x3A32E Rocket [(29, "Cubone"), (29, "Cubone")]
    , trainer 0x3A332 Rocket [(29, "Sandshrew"), (29, "Sandslash")]
    , trainer 0x3A336 Rocket [(26, "Raticate"), (26, "Zubat"), (26, "Golbat"), (26, "Rattata")]
    , trainer 0x3A33C Rocket [(28, "Weezing"), (28, "Golbat"), (28, "Koffing")]
    , trainer 0x3A341 Rocket [(28, "Drowzee"), (28, "Grimer"), (28, "Machop")]
    , trainer 0x3A346 Rocket [(28, "Golbat"), (28, "Drowzee"), (28, "Hypno")]
    , trainer 0x3A34B Rocket [(33, "Machoke")]
    , trainer 0x3A34E Rocket [(25, "Rattata"), (25, "Rattata"), (25, "Zubat"), (25, "Rattata"), (25, "Ekans")]
    , trainer 0x3A355 Rocket [(32, "Cubone"), (32, "Drowzee"), (32, "Marowak")]
    , trainer 0x3A35A CoolTrainerM [(39, "Nidorino"), (39, "Nidoking")]
    , trainer 0x3A35E CoolTrainerM [(43, "Exeggutor"), (43, "Cloyster"), (43, "Arcanine")]
    , trainer 0x3A363 CoolTrainerM [(43, "Kingler"), (43, "Tentacruel"), (43, "Blastoise")]
    , trainer 0x3A368 CoolTrainerM [(45, "Kingler"), (45, "Starmie")]
    , trainer 0x3A36C CoolTrainerM [(42, "Ivysaur"), (42, "Wartortle"), (42, "Charmeleon"), (42, "Charizard")]
    , trainer 0x3A372 CoolTrainerM [(44, "Ivysaur"), (44, "Wartortle"), (44, "Charmeleon")]
    , trainer 0x3A377 CoolTrainerM [(49, "Nidoking")]
    , trainer 0x3A37A CoolTrainerM [(44, "Kingler"), (44, "Cloyster")]
    , trainer 0x3A37E CoolTrainerM [(39, "Sandslash"), (39, "Dugtrio")]
    , trainer 0x3A382 CoolTrainerM [(43, "Rhyhorn")]
    , trainer 0x3A385 CoolTrainerF [(24, "Weepinbell"), (24, "Gloom"), (24, "Ivysaur")]
    , trainer 0x3A38A CoolTrainerF [(43, "Bellsprout"), (43, "Weepinbell"), (43, "Victreebel")]
    , trainer 0x3A38F CoolTrainerF [(43, "Parasect"), (43, "Dewgong"), (43, "Chansey")]
    , trainer 0x3A394 CoolTrainerF [(46, "Vileplume"), (46, "Butterfree")]
    , trainer 0x3A398 CoolTrainerF [(44, "Persian"), (44, "Ninetales")]
    , trainer 0x3A39C CoolTrainerF [(45, "Ivysaur"), (45, "Venusaur")]
    , trainer 0x3A3A0 CoolTrainerF [(45, "Nidorina"), (45, "Nidoqueen")]
    , trainer 0x3A3A4 CoolTrainerF [(43, "Persian"), (43, "Ninetales"), (43, "Raichu")]
    , trainer 0x3A3A9 Bruno [(53, "Onix"), (55, "Hitmonchan"), (55, "Hitmonlee"), (56, "Onix"), (58, "Machamp")]
    , trainer 0x3A3B5 Brock [(12, "Geodude"), (14, "Onix")]
    , trainer 0x3A3BB Misty [(18, "Staryu"), (21, "Starmie")]
    , trainer 0x3A3C1 LtSurge [(21, "Voltorb"), (18, "Pikachu"), (24, "Raichu")]
    , trainer 0x3A3C9 Erika [(29, "Victreebel"), (24, "Tangela"), (29, "Vileplume")]
    , trainer 0x3A3D1 Koga [(37, "Koffing"), (39, "Muk"), (37, "Koffing"), (43, "Weezing")]
    , trainer 0x3A3DB Blaine [(42, "Growlithe"), (40, "Ponyta"), (42, "Rapidash"), (47, "Arcanine")]
    , trainer 0x3A3E5 Sabrina [(38, "Kadabra"), (37, "MrMime"), (38, "Venomoth"), (43, "Alakazam")]
    , trainer 0x3A3EF Gentleman [(18, "Growlithe"), (18, "Growlithe")]
    , trainer 0x3A3F3 Gentleman [(19, "NidoranM"), (19, "NidoranF")]
    , trainer 0x3A3F7 Gentleman [(23, "Pikachu")]
    , trainer 0x3A3FA Gentleman [(48, "Primeape")]
    , trainer 0x3A3FD Gentleman [(17, "Growlithe"), (17, "Ponyta")]
    , trainer 0x3A401 Rival2 [(19, "Pidgeotto"), (16, "Raticate"), (18, "Kadabra"), (20, "Wartortle")]
    , trainer 0x3A40B Rival2 [(19, "Pidgeotto"), (16, "Raticate"), (18, "Kadabra"), (20, "Ivysaur")]
    , trainer 0x3A415 Rival2 [(19, "Pidgeotto"), (16, "Raticate"), (18, "Kadabra"), (20, "Charmeleon")]
    , trainer 0x3A41F Rival2 [(25, "Pidgeotto"), (23, "Growlithe"), (22, "Exeggcute"), (20, "Kadabra"), (25, "Wartortle")]
    , trainer 0x3A42B Rival2 [(25, "Pidgeotto"), (23, "Gyarados"), (22, "Growlithe"), (20, "Kadabra"), (25, "Ivysaur")]
    , trainer 0x3A437 Rival2 [(25, "Pidgeotto"), (23, "Exeggcute"), (22, "Gyarados"), (20, "Kadabra"), (25, "Charmeleon")]
    , trainer 0x3A443 Rival2 [(37, "Pidgeot"), (38, "Growlithe"), (35, "Exeggcute"), (35, "Alakazam"), (40, "Blastoise")]
    , trainer 0x3A44F Rival2 [(37, "Pidgeot"), (38, "Gyarados"), (35, "Growlithe"), (35, "Alakazam"), (40, "Venusaur")]
    , trainer 0x3A45B Rival2 [(37, "Pidgeot"), (38, "Gyarados"), (35, "Growlithe"), (35, "Alakazam"), (40, "Charizard")]
    , trainer 0x3A467 Rival2 [(47, "Pidgeot"), (45, "Rhyhorn"), (45, "Growlithe"), (47, "Exeggcute"), (50, "Alakazam"), (53, "Blastoise")]
    , trainer 0x3A475 Rival2 [(47, "Pidgeot"), (45, "Rhyhorn"), (45, "Gyarados"), (47, "Growlithe"), (50, "Alakazam"), (53, "Venusaur")]
    , trainer 0x3A483 Rival2 [(47, "Pidgeot"), (45, "Rhyhorn"), (45, "Exeggcute"), (47, "Gyarados"), (50, "Alakazam"), (53, "Charizard")]
    , trainer 0x3A491 Rival3 [(61, "Pidgeot"), (59, "Alakazam"), (61, "Rhydon"), (61, "Arcanine"), (63, "Exeggutor"), (65, "Blastoise")]
    , trainer 0x3A49F Rival3 [(61, "Pidgeot"), (59, "Alakazam"), (61, "Rhydon"), (61, "Gyarados"), (63, "Arcanine"), (65, "Venusaur")]
    , trainer 0x3A4AD Rival3 [(61, "Pidgeot"), (59, "Alakazam"), (61, "Rhydon"), (61, "Exeggutor"), (63, "Gyarados"), (65, "Charizard")]
    , trainer 0x3A4BB Lorelei [(54, "Dewgong"), (53, "Cloyster"), (54, "Slowbro"), (56, "Jynx"), (56, "Lapras")]
    , trainer 0x3A4C7 Channeler [(22, "Gastly")]
    , trainer 0x3A4CA Channeler [(24, "Gastly")]
    , trainer 0x3A4CD Channeler [(23, "Gastly"), (23, "Gastly")]
    , trainer 0x3A4D1 Channeler [(24, "Gastly")]
    , trainer 0x3A4D4 Channeler [(23, "Gastly")]
    , trainer 0x3A4D7 Channeler [(24, "Gastly")]
    , trainer 0x3A4DA Channeler [(24, "Haunter")]
    , trainer 0x3A4DD Channeler [(22, "Gastly")]
    , trainer 0x3A4E0 Channeler [(24, "Gastly")]
    , trainer 0x3A4E3 Channeler [(23, "Gastly"), (23, "Gastly")]
    , trainer 0x3A4E7 Channeler [(24, "Gastly")]
    , trainer 0x3A4EA Channeler [(22, "Gastly")]
    , trainer 0x3A4ED Channeler [(24, "Gastly")]
    , trainer 0x3A4F0 Channeler [(23, "Haunter")]
    , trainer 0x3A4F3 Channeler [(24, "Gastly")]
    , trainer 0x3A4F6 Channeler [(22, "Gastly")]
    , trainer 0x3A4F9 Channeler [(24, "Gastly")]
    , trainer 0x3A4FC Channeler [(22, "Haunter")]
    , trainer 0x3A4FF Channeler [(22, "Gastly"), (22, "Gastly"), (22, "Gastly")]
    , trainer 0x3A504 Channeler [(24, "Gastly")]
    , trainer 0x3A507 Channeler [(24, "Gastly")]
    , trainer 0x3A50A Channeler [(34, "Gastly"), (34, "Haunter")]
    , trainer 0x3A50E Channeler [(38, "Haunter")]
    , trainer 0x3A511 Channeler [(33, "Gastly"), (33, "Gastly"), (33, "Haunter")]
    , trainer 0x3A516 Agatha [(56, "Gengar"), (56, "Golbat"), (55, "Haunter"), (58, "Arbok"), (60, "Gengar")]
    , trainer 0x3A522 Lance [(58, "Gyarados"), (56, "Dragonair"), (56, "Dragonair"), (60, "Aerodactyl"), (62, "Dragonite")]
    ]
