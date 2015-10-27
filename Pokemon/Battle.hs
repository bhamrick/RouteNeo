{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Battle where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Text.Printf

import Pokemon.Moves
import Pokemon.Party
import Pokemon.Species
import Pokemon.Stats
import Pokemon.Type

data Participant =
    Participant
        { _partyData :: PartyPokemon
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
        (if crit then partyData . pStats else battleStats) .
        (case physicality (move^.moveType) of
            Physical -> atkStat
            Special -> spcStat)
    defense =
        defender ^.
        (if crit then partyData . pStats else battleStats) .
        (case physicality (move^.moveType) of
            Physical -> defStat
            Special -> spcStat)
    stab =
        move^.moveType == attacker^.partyData.pSpecies.type1 ||
        Just (move^.moveType) == attacker^.partyData.pSpecies.type2
    effective =
        effectiveness (move^.moveType) (defender^.partyData.pSpecies.type1) <>
        fromMaybe mempty (effectiveness (move^.moveType) <$> (defender^.partyData.pSpecies.type2))
    in
    floor (fromInteger (floor (fromInteger ((floor (fromInteger (attacker^.partyData.pLevel) * (2/5) * (if crit then 2 else 1) + 2) * attack * (move^.power) `div` 50 `div` defense) + 2) * (if stab then 3/2 else 1))) * attackRatio effective) * roll `div` 255

-- TODO: Stage modifiers
recomputeStats :: Badges -> Participant -> Participant
recomputeStats b p =
    let
    newStats =
        p^.partyData.pStats
        & applyBadgeBoosts b
    in
    p & battleStats .~ newStats

data FromParty a =
    FromParty
        { _fpIndex :: Integer
        , _fpData :: a
        }
    deriving (Eq, Show, Ord)

makeLenses ''FromParty

data BattleState =
    BattleState
        { _playerActive :: FromParty Participant
        , _enemyActive :: FromParty Participant
        , _playerBench :: [FromParty PartyPokemon]
        , _enemyBench :: [FromParty PartyPokemon]
        , _playerBadges :: Badges
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
    liftIO $ do
        for_ (attacker^.partyData.pMoves) $ \m ->
            let
                minDamage = damage attacker defender m False minRange
                maxDamage = damage attacker defender m False maxRange
                minCrit = damage attacker defender m True minRange
                maxCrit = damage attacker defender m True maxRange
            in
            printf "%-15s\t%d-%d\t(crit: %d-%d)\n" (m^.moveName) minDamage maxDamage minCrit maxCrit
        printf "\n"

defeatEnemyPokemon :: MonadBattle m => m ()
defeatEnemyPokemon = do
    oldLevel <- use (playerActive.fpData.partyData.pLevel)
    enemyPokemon <- use (enemyActive.fpData.partyData)
    playerActive.fpData.partyData %= defeatPokemon' (enemyPokemon^.pSpecies) (enemyPokemon^.pLevel) True 1
    enemyActive.fpData.partyData.pCurHP .= 0
    newLevel <- use (playerActive.fpData.partyData.pLevel)
    when (newLevel > oldLevel) $ do
        bs <- use playerBadges
        playerActive.fpData %= recomputeStats bs

data BattleResult = Victory | Defeat
    deriving (Eq, Show, Ord)

checkEndOfBattle :: MonadBattle m => m (Maybe BattleResult)
checkEndOfBattle =
    do
        pActive <- use playerActive
        pBench <- use playerBench
        if (pActive^.fpData.partyData.pCurHP <= 0 && all ((<= 0) . view (fpData.pCurHP)) pBench)
            then return (Just Defeat)
            else do
                eActive <- use enemyActive
                eBench <- use enemyBench
                if (eActive^.fpData.partyData.pCurHP <= 0 && all ((<= 0) . view (fpData.pCurHP)) eBench)
                    then return (Just Victory)
                    else return Nothing

partyAfterBattle :: BattleState -> [PartyPokemon]
partyAfterBattle s = map (view fpData) . sortOn (view fpIndex) $ (s^.playerActive & fpData %~ view partyData) : (s^.playerBench)

participant :: Badges -> PartyPokemon -> Participant
participant bs p =
    Participant
        { _partyData = p
        , _battleStats = p^.pStats
        }
    & recomputeStats bs

playerSwitchTo :: MonadBattle m => Integer -> m ()
playerSwitchTo ind = do
    match <- preuse (playerBench . each . filtered ((== ind) . view fpIndex))
    active <- use playerActive
    bs <- use playerBadges
    case match of
        Nothing -> throwError $ printf "Cannot switch player to party position %d." ind
        Just poke -> do
            playerActive .= (poke & fpData %~ participant bs)
            playerBench %= filter (/= poke)
            playerBench %= (:) (active & fpData %~ view partyData)

enemySwitchTo :: MonadBattle m => Integer -> m ()
enemySwitchTo ind = do
    match <- preuse (enemyBench . each . filtered ((== ind) . view fpIndex))
    active <- use enemyActive
    case match of
        Nothing -> throwError $ printf "Cannot switch enemy to party position %d." ind
        Just poke -> do
            enemyActive .= (poke & fpData %~ participant noBadges)
            enemyBench %= filter (/= poke)
            enemyBench %= (:) (active & fpData %~ view partyData)

enemyDefaultSwitch :: MonadBattle m => m ()
enemyDefaultSwitch = do
    liveBenchIndices <- map (view fpIndex) . filter ((> 0) . view (fpData . pCurHP)) <$> use enemyBench
    case liveBenchIndices of
        [] -> return ()
        _ -> enemySwitchTo (minimum liveBenchIndices)

defeatBattleWithRanges :: (MonadBattle m, MonadIO m) => m ()
defeatBattleWithRanges = do
    player <- use (playerActive.fpData)
    enemy <- use (enemyActive.fpData)
    liftIO $ printf "L%d %s vs L%d %s\n"
        (player^.partyData.pLevel)
        (player^.partyData.pSpecies.name)
        (enemy^.partyData.pLevel)
        (enemy^.partyData.pSpecies.name)
    liftIO $ printf "%s (%d/%d/%d/%d/%d)\n"
        (player^.partyData.pSpecies.name)
        (player^.battleStats.hpStat)
        (player^.battleStats.atkStat)
        (player^.battleStats.defStat)
        (player^.battleStats.spdStat)
        (player^.battleStats.spcStat)
    printDamages player enemy
    liftIO $ printf "%s (%d/%d/%d/%d/%d)\n"
        (enemy^.partyData.pSpecies.name)
        (enemy^.battleStats.hpStat)
        (enemy^.battleStats.atkStat)
        (enemy^.battleStats.defStat)
        (enemy^.battleStats.spdStat)
        (enemy^.battleStats.spcStat)
    printDamages enemy player
    defeatEnemyPokemon
    result <- checkEndOfBattle
    case result of
        Nothing -> do
            enemyDefaultSwitch
            defeatBattleWithRanges
        _ -> return ()
