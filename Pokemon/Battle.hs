{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Battle where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Text.Printf

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
        , _moves :: [Move]
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

data BattleState =
    BattleState
        { _playerParticipant :: Participant
        , _enemyParticipant :: Participant
        }
    deriving (Eq, Show, Ord)

makeLenses ''BattleState

newtype BattleT m a = BattleT { unBattleT :: StateT BattleState (ExceptT String m) a }

runBattleT :: BattleT m a -> BattleState -> m (Either String (a, BattleState))
runBattleT r s0 = runExceptT (runStateT (unBattleT r) s0)

instance Functor m => Functor (BattleT m) where
    fmap f (BattleT a) = BattleT (fmap f a)

instance Monad m => Applicative (BattleT m) where
    pure = BattleT . pure
    BattleT f <*> BattleT x = BattleT (f <*> x)

instance Monad m => Monad (BattleT m) where
    return = BattleT . return
    BattleT x >>= f = BattleT (x >>= unBattleT . f)
    fail = throwError

instance Monad m => MonadError String (BattleT m) where
    throwError = BattleT . throwError
    catchError (BattleT x) f = BattleT (catchError x (unBattleT . f))

instance Monad m => MonadState BattleState (BattleT m) where
    get = BattleT get
    put = BattleT . put
    state = BattleT . state

instance MonadIO m => MonadIO (BattleT m) where
    liftIO = BattleT . liftIO

class (MonadError String m, MonadState BattleState m) => MonadBattle m

instance Monad m => MonadBattle (BattleT m)

printDamages :: MonadIO m => Participant -> Participant -> m ()
printDamages attacker defender =
    for_ (attacker^.moves) $ \m ->
        let
            minDamage = damage attacker defender m False minRange
            maxDamage = damage attacker defender m False maxRange
            minCrit = damage attacker defender m True minRange
            maxCrit = damage attacker defender m True maxRange
        in
        printf "%s\t%d-%d\t(crit: %d-%d)\n" (m^.moveName) minDamage maxDamage minCrit maxCrit
