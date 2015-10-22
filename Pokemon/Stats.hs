{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Stats where

import Control.Lens
import Data.Bits

import Pokemon.Species

data DVs =
    DVs
        { _atkDV :: Integer
        , _defDV :: Integer
        , _spdDV :: Integer
        , _spcDV :: Integer
        }
    deriving (Eq, Show, Ord)

makeLenses ''DVs

trainerDVs :: DVs
trainerDVs =
    DVs { _atkDV = 9, _defDV = 8, _spdDV = 8, _spcDV = 8 }

hpDV :: Functor f => (Integer -> f Integer) -> DVs -> f DVs
hpDV = lens getHP setHP
    where
    getHP :: DVs -> Integer
    getHP dvs = (dvs^.atkDV .&. 1)*8 + (dvs^.defDV .&. 1)*4 + (dvs^.spdDV .&. 1)*2 + (dvs^.spcDV .&. 1)*1
    setHP :: DVs -> Integer -> DVs
    setHP dvs hp = dvs
        & atkDV .~ dvs^.atkDV .&. 0xE .|. ((hp .&. 8) `shiftR` 3)
        & defDV .~ dvs^.defDV .&. 0xE .|. ((hp .&. 4) `shiftR` 2)
        & spdDV .~ dvs^.spdDV .&. 0xE .|. ((hp .&. 2) `shiftR` 1)
        & spcDV .~ dvs^.spcDV .&. 0xE .|. ((hp .&. 1) `shiftR` 0)

data StatExp =
    StatExp
        { _hpStatExp :: Integer
        , _atkStatExp :: Integer
        , _defStatExp :: Integer
        , _spdStatExp :: Integer
        , _spcStatExp :: Integer
        }
    deriving (Eq, Show, Ord)

makeLenses ''StatExp

zeroStatExp :: StatExp
zeroStatExp =
    StatExp
        { _hpStatExp = 0
        , _atkStatExp = 0
        , _defStatExp = 0
        , _spdStatExp = 0
        , _spcStatExp = 0
        }

data Stats =
    Stats
        { _hpStat :: Integer
        , _atkStat :: Integer
        , _defStat :: Integer
        , _spdStat :: Integer
        , _spcStat :: Integer
        }
    deriving (Eq, Show, Ord)

makeLenses ''Stats

computeHP :: Integer -> Integer -> Integer -> Integer -> Integer
computeHP base level dv statExp =
    (((base + dv) * 2 + (ceiling (sqrt (fromInteger statExp)) `div` 4)) * level) `div` 100 + level + 10

computeStat :: Integer -> Integer -> Integer -> Integer -> Integer
computeStat base level dv statExp =
    (((base + dv) * 2 + (ceiling (sqrt (fromInteger statExp)) `div` 4)) * level) `div` 100 + 5

computeStats :: Species -> Integer -> DVs -> StatExp -> Stats
computeStats species level dvs statExp =
    Stats
        { _hpStat = computeHP (species^.baseHP) level (dvs^.hpDV) (statExp^.hpStatExp)
        , _atkStat = computeStat (species^.baseAtk) level (dvs^.atkDV) (statExp^.atkStatExp)
        , _defStat = computeStat (species^.baseDef) level (dvs^.defDV) (statExp^.defStatExp)
        , _spdStat = computeStat (species^.baseSpd) level (dvs^.spdDV) (statExp^.spdStatExp)
        , _spcStat = computeStat (species^.baseSpc) level (dvs^.spcDV) (statExp^.spcStatExp)
        }
