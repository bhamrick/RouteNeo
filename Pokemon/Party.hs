{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Party where

import Control.Lens
import Pokemon.Experience
import Pokemon.Moves
import Pokemon.Species
import Pokemon.Stats

data PartyPokemon =
    PartyPokemon
        { _pSpecies :: Species
        , _pExperience :: Integer
        , _pLevel :: Integer
        , _pDVs :: DVs
        , _pStatExp :: StatExp
        , _pStatExpAtLevel :: StatExp
        , _pStats :: Stats
        , _pMoves :: [Move]
        , _pCurHP :: Integer
        }
    deriving (Eq, Show, Ord)

makeLenses ''PartyPokemon

partyPokemon :: Species -> Integer -> DVs -> PartyPokemon
partyPokemon s lvl dvs =
    let
    initialStats = computeStats s lvl dvs zeroStatExp
    in
    PartyPokemon
        { _pSpecies = s
        , _pExperience = lowestExpForLevel (s^.expCurve) lvl
        , _pLevel = lvl
        , _pDVs = dvs
        , _pStatExp = zeroStatExp
        , _pStatExpAtLevel = zeroStatExp
        , _pStats = initialStats
        , _pMoves = []
        , _pCurHP = initialStats^.hpStat
        }

updateStats :: PartyPokemon -> PartyPokemon
updateStats poke =
    poke & pStats .~ computeStats (poke^.pSpecies) (poke^.pLevel) (poke^.pDVs) (poke^.pStatExpAtLevel)

checkLevelUp :: PartyPokemon -> PartyPokemon
checkLevelUp poke
    | poke^.pLevel == 100 = poke
    | poke^.pLevel > 100 = updateStats (poke & pLevel .~ 100 & pStatExpAtLevel .~ poke^.pStatExp)
    | otherwise =
        let
        nextLevelExp = lowestExpForLevel (poke^.pSpecies.expCurve) (poke^.pLevel+1)
        in
        if poke^.pExperience >= nextLevelExp
        then checkLevelUp (updateStats (poke & pLevel +~ 1 & pStatExpAtLevel .~ poke^.pStatExp))
        else poke

defeatPokemon' :: Species -> Integer -> Bool -> Integer -> PartyPokemon -> PartyPokemon
defeatPokemon' enemySpecies enemyLevel isTrainer participants poke =
    let
    expGain = ((enemySpecies^.killExp `div` participants) * enemyLevel `div` 7) * 3 `div` (if isTrainer then 2 else 3)
    in
    poke
        & pExperience +~ expGain
        & pStatExp.hpStatExp +~ enemySpecies^.baseHP
        & pStatExp.atkStatExp +~ enemySpecies^.baseAtk
        & pStatExp.defStatExp +~ enemySpecies^.baseDef
        & pStatExp.spdStatExp +~ enemySpecies^.baseSpd
        & pStatExp.spcStatExp +~ enemySpecies^.baseSpc
        & checkLevelUp

evolveTo' :: Species -> PartyPokemon -> PartyPokemon
evolveTo' s poke =
    poke
        & pSpecies .~ s
        & pStatExpAtLevel .~ (poke^.pStatExp)
        & updateStats

rarecandy :: PartyPokemon -> PartyPokemon
rarecandy poke
    | poke^.pLevel == 100 = poke
    | otherwise =
        poke
            & pExperience .~ lowestExpForLevel (poke^.pSpecies.expCurve) (poke^.pLevel + 1)
            & checkLevelUp
