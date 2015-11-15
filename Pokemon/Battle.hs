{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Battle where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Random
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Text.Printf

import Pokemon.Item
import Pokemon.Moves
import Pokemon.Party
import Pokemon.Species
import Pokemon.Stats
import Pokemon.Type

data StatMods =
    StatMods
        { _atkMod :: Integer
        , _defMod :: Integer
        , _spdMod :: Integer
        , _spcMod :: Integer
        , _accMod :: Integer
        , _evaMod :: Integer
        }
    deriving (Eq, Show, Ord)

makeLenses ''StatMods

defaultStatMods :: StatMods
defaultStatMods =
    StatMods
        { _atkMod = 0
        , _defMod = 0
        , _spdMod = 0
        , _spcMod = 0
        , _accMod = 0
        , _evaMod = 0
        }

statModRatio :: Integer -> Ratio Integer
statModRatio l =
    case l of
        -6 -> 25/100
        -5 -> 28/100
        -4 -> 33/100
        -3 -> 40/100
        -2 -> 50/100
        -1 -> 66/100
        0 -> 100/100
        1 -> 150/100
        2 -> 200/100
        3 -> 250/100
        4 -> 300/100
        5 -> 350/100
        6 -> 400/100
        _ -> 1


data Participant =
    Participant
        { _partyData :: PartyPokemon
        , _battleStats :: Stats
        , _battleStatMods :: StatMods
        , _turnsInBattle :: Integer
        }
    deriving (Eq, Show, Ord)

makeLenses ''Participant

data PlayerBattleAction
    = PUseMove Move
    | PUseItem Item
    | PSwitch Integer
    deriving (Eq, Show, Ord)

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
    floor (fromInteger (floor (fromInteger ((floor (fromInteger (attacker^.partyData.pLevel) * (2/5) * (if crit then 2 else 1) + 2) * attack * (move^.power) `div` defense `div` 50) + 2) * (if stab then 3/2 else 1))) * attackRatio effective) * roll `div` 255

applyStatMods :: StatMods -> Stats -> Stats
applyStatMods mods stats =
    Stats
        { _hpStat = stats^.hpStat
        , _atkStat = floor (fromInteger (stats^.atkStat) * statModRatio (mods^.atkMod))
        , _defStat = floor (fromInteger (stats^.defStat) * statModRatio (mods^.defMod))
        , _spdStat = floor (fromInteger (stats^.spdStat) * statModRatio (mods^.spdMod))
        , _spcStat = floor (fromInteger (stats^.spcStat) * statModRatio (mods^.spcMod))
        }

-- TODO: Burn/Paralysis modifiers
recomputeStats :: Badges -> Participant -> Participant
recomputeStats b p =
    let
    newStats =
        p^.partyData.pStats
        & applyBadgeBoosts b
        & applyStatMods (p^.battleStatMods)
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

instance MonadRandom m => MonadRandom (BattleT m) where
    getRandom = BattleT getRandom
    getRandoms = BattleT getRandoms
    getRandomR = BattleT . getRandomR
    getRandomRs = BattleT . getRandomRs

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
            printf "%-15s\t%d-%d\t%.2f%%-%.2f%%\t(crit: %d-%d\t%.2f%%-%.2f%%)\n"
                (m^.moveName)
                minDamage
                maxDamage
                (fromInteger minDamage * 100 / fromInteger (defender^.battleStats.hpStat) :: Double)
                (fromInteger maxDamage * 100 / fromInteger (defender^.battleStats.hpStat) :: Double)
                minCrit
                maxCrit
                (fromInteger minCrit * 100 / fromInteger (defender^.battleStats.hpStat) :: Double)
                (fromInteger maxCrit * 100 / fromInteger (defender^.battleStats.hpStat) :: Double)
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
        , _turnsInBattle = 0
        , _battleStatMods = defaultStatMods
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

playerDefaultSwitch :: MonadBattle m => m ()
playerDefaultSwitch = do
    liveBenchIndices <- map (view fpIndex) . filter ((> 0) . view (fpData . pCurHP)) <$> use enemyBench
    case liveBenchIndices of
        [] -> return ()
        _ -> enemySwitchTo (minimum liveBenchIndices)

defeatBattle :: (MonadBattle m, MonadIO m) => m ()
defeatBattle = do
    defeatEnemyPokemon
    result <- checkEndOfBattle
    case result of
        Nothing -> do
            enemyDefaultSwitch
            defeatBattle
        _ -> return ()

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

-- TODO: Move effects
-- TODO: Complete accuracy check (XAcc, etc)
-- TODO: Account for high crit moves
useMove :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> ALens' BattleState (FromParty Participant) -> Move -> m ()
useMove attackerL defenderL m = do
    moveHits <- do
        accByte <- getRandomR (0, 255)
        pure (accByte < m^.accuracy)
    damageRoll <- getRandomR (minRange, maxRange)
    attacker <- use (cloneLens attackerL . fpData)
    defender <- use (cloneLens defenderL . fpData)
    isCrit <- do
        attackerBaseSpd <- use (cloneLens attackerL . fpData . partyData . pSpecies . baseSpd)
        critByte <- getRandomR (0, 255)
        pure (critByte < attackerBaseSpd `div` 2)
    let damageDone = damage attacker defender m isCrit damageRoll
    cloneLens defenderL . fpData . partyData . pCurHP %= max 0 . subtract damageDone
    pure ()

-- TODO: Account for using items on non-lead pokemon.
useItem :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> Item -> m ()
useItem target item = pure ()

runTurn :: (MonadBattle m, MonadRandom m) => m PlayerBattleAction -> m Move -> m Bool -> m ()
runTurn playerStrategy enemyStrategy enemySpecialAI = do
    -- TODO: Check if player is locked into an action.
    playerAction <- playerStrategy
    case playerAction of
        PUseMove move -> do
            enemyMove <- enemyStrategy
            playerFirst <-
                case (move^.moveName == "Quick Attack", move^.moveName == "Counter", enemyMove^.moveName == "Quick Attack", enemyMove^.moveName == "Counter") of
                    (True, _, False, _) -> pure True
                    (False, _, True, _) -> pure False
                    (_, True, _, False) -> pure False
                    (_, False, _, True) -> pure True
                    _ -> do
                        playerSpeed <- use (playerActive . fpData . battleStats . spdStat)
                        enemySpeed <- use (playerActive . fpData . battleStats . spdStat)
                        case compare playerSpeed enemySpeed of
                            LT -> pure False
                            GT -> pure True
                            EQ -> getRandom
            if playerFirst
                then do
                    useMove playerActive enemyActive move
                    enemyHP <- use (enemyActive . fpData . partyData . pCurHP)
                    when (enemyHP > 0) $ do
                        usedSpecialAI <- enemySpecialAI
                        when (not usedSpecialAI) $ useMove enemyActive playerActive enemyMove
                else do
                    usedSpecialAI <- enemySpecialAI
                    when (not usedSpecialAI) $ useMove enemyActive playerActive enemyMove
                    playerHP <- use (playerActive . fpData . partyData . pCurHP)
                    when (playerHP > 0) $ useMove playerActive enemyActive move
            pure ()
        PUseItem item -> do
            useItem playerActive item
            enemyMove <- enemyStrategy
            usedSpecialAI <- enemySpecialAI
            when (not usedSpecialAI) $ useMove enemyActive playerActive enemyMove
        PSwitch ind -> do
            playerSwitchTo ind
            enemyMove <- enemyStrategy
            usedSpecialAI <- enemySpecialAI
            when (not usedSpecialAI) $ useMove enemyActive playerActive enemyMove

simulateBattle :: (MonadBattle m, MonadRandom m) => m PlayerBattleAction -> m Move -> m Bool -> m BattleResult
simulateBattle playerStrategy enemyStrategy enemySpecialAI = do
    runTurn playerStrategy enemyStrategy enemySpecialAI
    enemyHP <- use (enemyActive . fpData . partyData . pCurHP)
    playerHP <- use (playerActive . fpData . partyData . pCurHP)

    when (enemyHP == 0 && playerHP > 0) $ do
        enemyPokemon <- use (enemyActive.fpData.partyData)
        oldLevel <- use (playerActive.fpData.partyData.pLevel)
        playerActive.fpData.partyData %= defeatPokemon' (enemyPokemon^.pSpecies) (enemyPokemon^.pLevel) True 1
        newLevel <- use (playerActive.fpData.partyData.pLevel)
        when (newLevel > oldLevel) $ do
            bs <- use playerBadges
            playerActive.fpData %= recomputeStats bs

    result <- checkEndOfBattle
    case result of
        Nothing -> do
            when (enemyHP == 0) $ do
                enemyDefaultSwitch
            when (playerHP == 0) $ do
                -- TODO: Allow player to specify switch strategies
                playerDefaultSwitch
            simulateBattle playerStrategy enemyStrategy enemySpecialAI
        Just r -> pure r
