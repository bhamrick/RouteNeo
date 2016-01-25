{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Pokemon.Battle where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe
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
import Pokemon.Status
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
        , _ownerBadges :: Badges
        , _isEnemy :: Bool
        , _turnsInBattle :: Integer
        , _usingXAccuracy :: Bool
        , _isConfused :: Bool
        , _confusionCounter :: Integer
        , _movePrevented :: Bool
        }
    deriving (Eq, Show, Ord)

makeLenses ''Participant

data PlayerBattleAction
    = PUseMove Move
    | PUseItem Item
    | PSwitch Integer
    | PForfeit
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
    case move^.effect of
        SpecialDamageEffect -> case move^.moveName of
            "Seismic Toss" -> attacker^.partyData.pLevel
            "Night Shade" -> attacker^.partyData.pLevel
            "Sonicboom" -> 20
            "Dragon Rage" -> 40
            "Psywave" -> 0 -- TODO: Will require a refactor
            _ -> 0
        OHKOEffect -> 0 -- Damage done in effect execution
        _ -> floor (fromInteger (floor (fromInteger ((floor (fromInteger (attacker^.partyData.pLevel) * (2/5) * (if crit then 2 else 1) + 2) * attack * (move^.power) `div` defense `div` 50) + 2) * (if stab then 3/2 else 1))) * attackRatio effective) * roll `div` 255

applyStatMods :: StatMods -> Stats -> Stats
applyStatMods mods stats =
    Stats
        { _hpStat = stats^.hpStat
        , _atkStat = floor (fromInteger (stats^.atkStat) * statModRatio (mods^.atkMod))
        , _defStat = floor (fromInteger (stats^.defStat) * statModRatio (mods^.defMod))
        , _spdStat = floor (fromInteger (stats^.spdStat) * statModRatio (mods^.spdMod))
        , _spcStat = floor (fromInteger (stats^.spcStat) * statModRatio (mods^.spcMod))
        }

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x
    | x <= lo = lo
    | x >= hi = hi
    | otherwise = x

quarterSpeedForParalysis :: Participant -> Participant
quarterSpeedForParalysis p =
    case p^.partyData.pStatus of
        PAR -> p & battleStats . spdStat %~ max 1 . (`div` 4)
        _ -> p

-- TODO: Burn modifier
recomputeStats :: Participant -> Participant
recomputeStats p =
    let
    newStats =
        p^.partyData.pStats
        & applyBadgeBoosts (p^.ownerBadges)
        & applyStatMods (p^.battleStatMods)
    in
    p & battleStats .~ newStats
      & quarterSpeedForParalysis

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
        , _turnCount :: Integer
        , _halfTurnCount :: Integer
        , _frameCount :: Integer
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

instance (MonadError String m, MonadState BattleState m) => MonadBattle m

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
        playerActive.fpData %= recomputeStats

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

participant :: Bool -> Badges -> PartyPokemon -> Participant
participant enemy bs p =
    Participant
        { _partyData = p
        , _battleStats = p^.pStats
        , _turnsInBattle = 0
        , _ownerBadges = bs
        , _isEnemy = enemy
        , _battleStatMods = defaultStatMods
        , _usingXAccuracy = False
        , _isConfused = False
        , _confusionCounter = 0
        , _movePrevented = False
        }
    & recomputeStats

playerSwitchTo :: MonadBattle m => Integer -> m ()
playerSwitchTo ind = do
    match <- preuse (playerBench . each . filtered ((== ind) . view fpIndex))
    active <- use playerActive
    bs <- use playerBadges
    case match of
        Nothing -> throwError $ printf "Cannot switch player to party position %d." ind
        Just poke -> do
            playerActive .= (poke & fpData %~ participant False bs)
            playerBench %= filter (/= poke)
            playerBench %= (:) (active & fpData %~ view partyData)

enemySwitchTo :: MonadBattle m => Integer -> m ()
enemySwitchTo ind = do
    match <- preuse (enemyBench . each . filtered ((== ind) . view fpIndex))
    active <- use enemyActive
    case match of
        Nothing -> throwError $ printf "Cannot switch enemy to party position %d." ind
        Just poke -> do
            enemyActive .= (poke & fpData %~ participant True noBadges)
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
    liveBenchIndices <- map (view fpIndex) . filter ((> 0) . view (fpData . pCurHP)) <$> use playerBench
    case liveBenchIndices of
        [] -> return ()
        _ -> playerSwitchTo (minimum liveBenchIndices)

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

earlyReturn :: Applicative m => e -> ExceptT e m a
earlyReturn e = ExceptT (pure $ Left e)

statusCheck :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> m Bool
statusCheck attackerL = fmap (either id id) . runExceptT $ do
    prevented <- use (cloneLens attackerL . fpData . movePrevented)
    when prevented $ earlyReturn False
    passSleep <- lift $ sleepCheck attackerL
    when (not passSleep) $ earlyReturn False
    passFreeze <- lift $ freezeCheck attackerL
    when (not passFreeze) $ earlyReturn False
    passConfused <- lift $ confusionCheck attackerL
    when (not passConfused) $ earlyReturn False
    passParalysis <- lift $ paralysisCheck attackerL
    when (not passParalysis) $ earlyReturn False
    pure True

sleepCheck :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> m Bool
sleepCheck attackerL = do
    curStatus <- use (cloneLens attackerL . fpData . partyData . pStatus)
    case curStatus of
        SLP 1 -> do
            cloneLens attackerL . fpData . partyData . pStatus .= Healthy
            pure True
        SLP n -> do
            frameCount += 102
            cloneLens attackerL . fpData . partyData . pStatus .= SLP (n-1)
            pure False
        _ -> pure True

freezeCheck :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> m Bool
freezeCheck attackerL = do
    curStatus <- use (cloneLens attackerL . fpData . partyData . pStatus)
    case curStatus of
        FRZ -> pure False
        _ -> pure True

confusionCheck :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> m Bool
confusionCheck attackerL = do
    confused <- use (cloneLens attackerL . fpData . isConfused)
    if confused
        then do
            turns <- cloneLens attackerL . fpData . confusionCounter <%= subtract 1
            if turns == 0
                then do
                    cloneLens attackerL . fpData . isConfused .= False
                    frameCount += 33
                    pure True
                else do
                    hitSelf <- getRandom
                    if hitSelf
                        then do
                            attacker <- use (cloneLens attackerL . fpData)
                            let dmg = damage attacker attacker hitSelfMove False maxRange
                            cloneLens attackerL . fpData . partyData . pCurHP %= max 0 . subtract dmg
                            frameCount += 204
                            pure False
                        else do
                            frameCount += 85
                            pure True
        else pure True

paralysisCheck :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> m Bool
paralysisCheck attackerL = do
    curStatus <- use (cloneLens attackerL . fpData . partyData . pStatus)
    case curStatus of
        PAR -> do
            x <- getRandomR (0, 255)
            when (x <= (0x39 :: Integer)) $ frameCount += 43
            pure (x > (0x39 :: Integer))
        _ -> pure True

-- TODO: Account for high crit moves
-- TODO: Complete accuracy check
-- TODO: Complete move effects

-- Includes hard coded frame numbers that are relevant for Agatha
useMove :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> ALens' BattleState (FromParty Participant) -> Move -> m ()
useMove attackerL defenderL m =
    fmap (fromMaybe ()) . runMaybeT $ do
        moveHits <- fmap (either id id) . runExceptT $ do
            when (m^.effect == DreamEaterEffect) $ do
                s <- use (cloneLens defenderL . fpData . partyData . pStatus)
                case s of
                    SLP _ -> pure ()
                    _ -> earlyReturn False
            when (m^.effect == SwiftEffect) $ earlyReturn True
            hasXAcc <- use (cloneLens attackerL . fpData . usingXAccuracy)
            when hasXAcc $ earlyReturn True
            accByte <- getRandomR (0, 255)
            pure (accByte < m^.accuracy)
        when (not moveHits) $ do
            frameCount += case m^.moveName of
                "Earthquake" -> 91
                "Ice Beam" -> 89
                "Dream Eater" -> 111
                "Hypnosis" -> 102
                "Confuse Ray" -> 124
                "Supersonic" -> 123
                "Screech" -> 97
                "Glare" -> 118
                "Toxic" -> 108
                -- These numbers are "wrong" because they're an average of the
                -- miss chance and non-miss chance, but in the right ratio so it
                -- shouldn't affect average computations
                "Wing Attack" -> 190
                "Night Shade" -> 275
                "Bite" -> 219
                "Acid" -> 218
                _ -> 0
            abort
        frameCount += case m^.moveName of
            "Earthquake" -> 192
            "Ice Beam" -> 140
            "Dream Eater" -> 111
            "Hypnosis" -> 102
            "Confuse Ray" -> 124
            "Supersonic" -> 123
            "Screech" -> 97
            "Glare" -> 153
            "Toxic" -> 108
            "Horn Drill" -> 127
            -- These numbers are "wrong" because they're an average of the
            -- miss chance and non-miss chance, but in the right ratio so it
            -- shouldn't affect average computations
            "Wing Attack" -> 190
            "Night Shade" -> 275
            "Bite" -> 219
            "Acid" -> 218
            "Haze" -> 120
            _ -> 0
        damageRoll <- getRandomR (minRange, maxRange)
        attacker <- use (cloneLens attackerL . fpData)
        defender <- use (cloneLens defenderL . fpData)
        isCrit <- do
            attackerBaseSpd <- use (cloneLens attackerL . fpData . partyData . pSpecies . baseSpd)
            critByte <- getRandomR (0, 255)
            pure (critByte < attackerBaseSpd `div` 2)
        when (isCrit && m^.moveName == "Ice Beam") $ frameCount += 26
        let damageDone = damage attacker defender m isCrit damageRoll
        cloneLens defenderL . fpData . partyData . pCurHP %= max 0 . subtract damageDone
        executeMoveEffect attackerL defenderL (m^.effect)
        pure ()

abort :: Applicative m => MaybeT m b
abort = MaybeT (pure Nothing)

statModifierDownEffect :: (MonadBattle m, MonadRandom m) => ALens' StatMods Integer -> ALens' Stats Integer -> Integer -> ALens' BattleState (FromParty Participant) -> m ()
statModifierDownEffect modL statL modDelta targetL =
    fmap (fromMaybe ()) . runMaybeT $ do
        target <- use (cloneLens targetL . fpData)
        when (not $ target^.isEnemy) $ do
            missByte <- getRandomR (0, 0xFF)
            when (missByte < (0x40 :: Integer)) abort
        frameCount += 97
        modifyStat modL statL (-modDelta) targetL

statModifierDownSideEffect :: (MonadBattle m, MonadRandom m) => ALens' StatMods Integer -> ALens' Stats Integer -> Integer -> ALens' BattleState (FromParty Participant) -> m ()
statModifierDownSideEffect modL statL modDelta targetL =
    fmap (fromMaybe ()) . runMaybeT $ do
        target <- use (cloneLens targetL . fpData)
        when (not $ target^.isEnemy) $ do
            missByte <- getRandomR (0, 0xFF)
            when (missByte < (0x40 :: Integer)) abort
        successByte <- getRandomR (0, 0xFF)
        when (successByte < (0x55 :: Integer)) $ modifyStat modL statL (-modDelta) targetL

executeMoveEffect :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> ALens' BattleState (FromParty Participant) -> MoveEffect -> m ()
executeMoveEffect attackerL defenderL effect =
    case effect of
        FreezeSideEffect -> do
            curStatus <- use (cloneLens defenderL . fpData . partyData . pStatus)
            case curStatus of
                Healthy -> do
                    x <- getRandomR (0, 0xFF)
                    when (x < (0x1A :: Integer)) $ do
                        frameCount += 93
                        cloneLens defenderL . fpData . partyData . pStatus .= FRZ
                _ -> pure ()
        AttackDown1Effect -> statModifierDownEffect atkMod atkStat 1 defenderL
        DefenseDown1Effect -> statModifierDownEffect defMod defStat 1 defenderL
        SpeedDown1Effect -> statModifierDownEffect spdMod spdStat 1 defenderL
        SpecialDown1Effect -> statModifierDownEffect spcMod spcStat 1 defenderL
        HazeEffect -> do
            -- TODO: Complete all the effects.
            cloneLens attackerL . fpData . battleStatMods .= defaultStatMods
            cloneLens defenderL . fpData . battleStatMods .= defaultStatMods
            
            attackerPartyStats <- use (cloneLens attackerL . fpData . partyData . pStats)
            cloneLens attackerL . fpData . battleStats .= attackerPartyStats
            defenderPartyStats <- use (cloneLens defenderL . fpData . partyData . pStats)
            cloneLens defenderL . fpData . battleStats .= defenderPartyStats

            cloneLens attackerL . fpData . partyData . pStatus .= Healthy
            defenderStatus <- cloneLens defenderL . fpData . partyData . pStatus <<.= Healthy
            case defenderStatus of
                SLP _ -> cloneLens defenderL . fpData . movePrevented .= True
                FRZ -> cloneLens defenderL . fpData . movePrevented .= True
                _ -> pure ()

            -- TODO: Clear disabled move
            -- TODO: Clear Mist / Guard Spec
            -- TODO: Clear Focus Energy
            -- TODO: Clear Leech Seed
            -- TODO: Clear Toxic
            -- TODO: Clear Reflect, Light Screen

            cloneLens attackerL . fpData . isConfused .= False
            cloneLens attackerL . fpData . usingXAccuracy .= False
            cloneLens defenderL . fpData . isConfused .= False
            cloneLens defenderL . fpData . usingXAccuracy .= False

            pure ()
        FlinchSideEffect1 -> do
            x <- getRandomR (0, 255)
            when (x < (0x1a :: Integer)) $ do
                frameCount += 34
                cloneLens defenderL . fpData . movePrevented .= True
        SleepEffect -> do
            curStatus <- use (cloneLens defenderL . fpData . partyData . pStatus)
            case curStatus of
                Healthy -> do
                    frameCount += 55
                    turns <- getRandomR (1, 7)
                    cloneLens defenderL . fpData . partyData . pStatus .= SLP turns
                _ -> pure ()
        OHKOEffect -> do
            s1 <- use (cloneLens attackerL . fpData . battleStats . spdStat)
            s2 <- use (cloneLens defenderL . fpData . battleStats . spdStat)
            when (s1 >= s2) $ cloneLens defenderL . fpData . partyData . pCurHP .= 0
        SpecialDamageEffect -> pure () -- Handled in damage calculation
        ConfusionEffect -> do
            alreadyConfused <- use (cloneLens defenderL . fpData . isConfused)
            when (not alreadyConfused) $ do
                frameCount += 26
                numTurns <- getRandomR (2, 5)
                cloneLens defenderL . fpData . isConfused .= True
                cloneLens defenderL . fpData . confusionCounter .= numTurns
        AttackDown2Effect -> statModifierDownEffect atkMod atkStat 2 defenderL
        DefenseDown2Effect -> statModifierDownEffect defMod defStat 2 defenderL
        SpeedDown2Effect -> statModifierDownEffect spdMod spdStat 2 defenderL
        SpecialDown2Effect -> statModifierDownEffect spcMod spcStat 2 defenderL
        PoisonEffect -> fmap (fromMaybe ()) . runMaybeT $ do
            -- TODO: Handle poison damage
            t1 <- use (cloneLens defenderL . fpData . partyData . pSpecies . type1)
            when (t1 == Poison) abort
            t2 <- use (cloneLens defenderL . fpData . partyData . pSpecies . type2)
            when (t2 == Just Poison) abort
            curStatus <- use (cloneLens defenderL . fpData . partyData . pStatus)
            case curStatus of
                Healthy -> do
                    cloneLens defenderL . fpData . partyData . pStatus .= PSN
                _ -> pure ()
        ParalyzeEffect -> do
            -- TODO: Check for immunities
            curStatus <- use (cloneLens defenderL . fpData . partyData . pStatus)
            case curStatus of
                Healthy -> do
                    cloneLens defenderL . fpData . partyData . pStatus .= PAR
                    cloneLens defenderL . fpData %= quarterSpeedForParalysis
                _ -> pure ()
        AttackDownSideEffect -> statModifierDownSideEffect atkMod atkStat 1 defenderL
        DefenseDownSideEffect -> statModifierDownSideEffect defMod defStat 1 defenderL
        SpeedDownSideEffect -> statModifierDownSideEffect spdMod spdStat 1 defenderL
        SpecialDownSideEffect -> statModifierDownSideEffect spcMod spcStat 1 defenderL
                
        NoEffect -> pure ()
        _ -> throwError (show effect ++ " not implemented")

-- TODO: Account for using items on non-lead pokemon.
useItem :: (MonadBattle m, MonadRandom m) => ALens' BattleState (FromParty Participant) -> Item -> m ()
useItem targetL item = do
    case item of
        XAccuracy -> cloneLens targetL . fpData . usingXAccuracy .= True
        XAttack -> modifyAtk 1 targetL
        XDefend -> modifyDef 1 targetL
        XSpeed -> modifySpd 1 targetL
        XSpecial -> modifySpc 1 targetL
        Pokeflute -> do
            let wakeup p = case p^.pStatus of
                    SLP _ -> p & pStatus .~ Healthy
                    _ -> p
            playerActive . fpData . partyData %= wakeup
            playerBench . each . fpData %= wakeup
            enemyActive . fpData . partyData %= wakeup
            enemyBench . each . fpData %= wakeup
        _ -> pure ()
    frameCount += case item of
        XAccuracy -> 63
        XSpeed -> 167
        XSpecial -> 169
        Pokeflute -> 479
        _ -> 0

runTurn :: (MonadBattle m, MonadRandom m) => m PlayerBattleAction -> m Move -> m Bool -> m ()
runTurn playerStrategy enemyStrategy enemySpecialAI = do
    -- TODO: Check if player is locked into an action.
    playerActive . fpData . movePrevented .= False
    enemyActive . fpData . movePrevented .= False
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
                        enemySpeed <- use (enemyActive . fpData . battleStats . spdStat)
                        case compare playerSpeed enemySpeed of
                            LT -> pure False
                            GT -> pure True
                            EQ -> getRandom
            if playerFirst
                then do
                    -- Player turn starts here
                    canMove <- statusCheck playerActive
                    when canMove $ useMove playerActive enemyActive move
                    halfTurnCount += 1
                    enemyHP <- use (enemyActive . fpData . partyData . pCurHP)
                    when (enemyHP > 0) $ do
                        -- Enemy turn starts here
                        usedSpecialAI <- enemySpecialAI
                        when (not usedSpecialAI) $ do
                            canMove <- statusCheck enemyActive
                            when canMove $ useMove enemyActive playerActive enemyMove
                        halfTurnCount += 1
                else do
                    -- Enemy turn starts here
                    usedSpecialAI <- enemySpecialAI
                    when (not usedSpecialAI) $ do
                        canMove <- statusCheck enemyActive
                        when canMove $ useMove enemyActive playerActive enemyMove
                    halfTurnCount += 1
                    playerHP <- use (playerActive . fpData . partyData . pCurHP)
                    when (playerHP > 0) $ do
                        -- Player turn starts here
                        canMove <- statusCheck playerActive
                        when canMove $ useMove playerActive enemyActive move
                        halfTurnCount += 1
            pure ()
        PUseItem item -> do
            useItem playerActive item
            halfTurnCount += 1
            enemyMove <- enemyStrategy
            -- Enemy Turn starts here
            usedSpecialAI <- enemySpecialAI
            when (not usedSpecialAI) $ do
                canMove <- statusCheck enemyActive
                when canMove $ useMove enemyActive playerActive enemyMove
            halfTurnCount += 1
        PSwitch ind -> do
            playerSwitchTo ind
            halfTurnCount += 1
            enemyMove <- enemyStrategy
            usedSpecialAI <- enemySpecialAI
            when (not usedSpecialAI) $ do
                canMove <- statusCheck enemyActive
                when canMove $ useMove enemyActive playerActive enemyMove
            halfTurnCount += 1
        PForfeit -> do
            playerActive . fpData . partyData . pCurHP .= 0
            playerBench . each . fpData . pCurHP .= 0
            -- Count a forfeit as having lost on the previous turn.
            playerActive.fpData.turnsInBattle -= 1
            enemyActive.fpData.turnsInBattle -= 1
            turnCount -= 1
    playerActive.fpData.turnsInBattle += 1
    enemyActive.fpData.turnsInBattle += 1
    turnCount += 1

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
        when (newLevel > oldLevel) $ playerActive.fpData %= recomputeStats

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

-- TODO: Apply Burn/Paralysis modifiers
-- TODO: Fix behavior when modifying a stat already at +/-6 (no badge boost should be applied).
modifyStat :: MonadBattle m => ALens' StatMods Integer -> ALens' Stats Integer -> Integer -> ALens' BattleState (FromParty Participant) -> m ()
modifyStat statModL statL modDelta targetL = do
    newMod <- cloneLens targetL . fpData . battleStatMods . cloneLens statModL <%= clamp (-6) 6 . (+) modDelta
    partyStat <- use (cloneLens targetL . fpData . partyData . pStats . cloneLens statL)
    cloneLens targetL . fpData . battleStats . cloneLens statL .= floor (fromInteger partyStat * statModRatio newMod)
    bs <- use (cloneLens targetL . fpData . ownerBadges)
    cloneLens targetL . fpData . battleStats %= applyBadgeBoosts bs
    -- TODO: Investigate possible bug in which pokemon gets speed quartered.
    cloneLens targetL . fpData %= quarterSpeedForParalysis

modifyAtk :: MonadBattle m => Integer -> ALens' BattleState (FromParty Participant) -> m ()
modifyAtk = modifyStat atkMod atkStat

modifyDef :: MonadBattle m => Integer -> ALens' BattleState (FromParty Participant) -> m ()
modifyDef = modifyStat defMod defStat

modifySpd :: MonadBattle m => Integer -> ALens' BattleState (FromParty Participant) -> m ()
modifySpd = modifyStat spdMod spdStat

modifySpc :: MonadBattle m => Integer -> ALens' BattleState (FromParty Participant) -> m ()
modifySpc = modifyStat spcMod spcStat

-- Used for modifying accuracy/evasion stat mods
modifyMod :: MonadBattle m => ALens' StatMods Integer -> Integer -> ALens' BattleState (FromParty Participant) -> m ()
modifyMod modL modDelta targetL = do
    cloneLens targetL . fpData . battleStatMods . cloneLens modL %= clamp (-6) 6 . (+) modDelta
