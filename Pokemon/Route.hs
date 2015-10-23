{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Route where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import qualified Data.Map as Map

import Pokemon.Battle
import Pokemon.Experience
import Pokemon.Moves
import Pokemon.Species
import Pokemon.Stats
import Pokemon.Trainer
import Pokemon.Type

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

data RouteState =
    RouteState
        { _party :: [PartyPokemon]
        }
    deriving (Eq, Show, Ord)

makeLenses ''RouteState

emptyParty :: RouteState
emptyParty = RouteState { _party = [] }

newtype RouteT m a = RouteT { unRouteT :: StateT RouteState (ExceptT String m) a }

runRouteT :: RouteT m a -> RouteState -> m (Either String (a, RouteState))
runRouteT r s0 = runExceptT (runStateT (unRouteT r) s0)

instance Functor m => Functor (RouteT m) where
    fmap f (RouteT a) = RouteT (fmap f a)

instance Monad m => Applicative (RouteT m) where
    pure = RouteT . pure
    RouteT f <*> RouteT x = RouteT (f <*> x)

instance Monad m => Monad (RouteT m) where
    return = RouteT . return
    RouteT x >>= f = RouteT (x >>= unRouteT . f)
    fail = throwError

instance Monad m => MonadError String (RouteT m) where
    throwError = RouteT . throwError
    catchError (RouteT x) f = RouteT (catchError x (unRouteT . f))

instance Monad m => MonadState RouteState (RouteT m) where
    get = RouteT get
    put = RouteT . put
    state = RouteT . state

instance MonadIO m => MonadIO (RouteT m) where
    liftIO = RouteT . liftIO

class (MonadError String m, MonadState RouteState m) => MonadRoute m

instance Monad m => MonadRoute (RouteT m)

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

defeatPokemon :: MonadRoute m => Species -> Integer -> Bool -> Integer -> m ()
defeatPokemon enemySpecies enemyLevel isTrainer participants = party._head %= defeatPokemon' enemySpecies enemyLevel isTrainer participants

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

defeatTrainer :: MonadRoute m => Integer -> m ()
defeatTrainer offset = 
    do
        case Map.lookup offset trainersByOffset of
            Nothing -> throwError ("Could not find trainer offset " ++ (show offset))
            Just t ->
                for_ (t^.tParty) $ \enemy ->
                    party._head %= defeatPokemon' (enemy^.tpSpecies) (enemy^.tpLevel) True 1

evolveTo :: Species -> PartyPokemon -> PartyPokemon
evolveTo s poke =
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
