{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Route where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Random
import Data.Foldable
import Data.List
import qualified Data.Map as Map
import Text.Printf

import Pokemon.Battle
import Pokemon.EnemyAI
import Pokemon.Experience
import Pokemon.Moves
import Pokemon.Party
import Pokemon.Species
import Pokemon.Stats
import Pokemon.Trainer
import Pokemon.Type

data RouteState =
    RouteState
        { _party :: [PartyPokemon]
        , _badges :: Badges
        , _pokemonDefeated :: Int
        }
    deriving (Eq, Show, Ord)

makeLenses ''RouteState

emptyParty :: RouteState
emptyParty =
    RouteState
        { _party = []
        , _badges = noBadges
        , _pokemonDefeated = 0
        }

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

instance MonadRandom m => MonadRandom (RouteT m) where
    getRandom = RouteT getRandom
    getRandoms = RouteT getRandoms
    getRandomR = RouteT . getRandomR
    getRandomRs = RouteT . getRandomRs

class (MonadError String m, MonadState RouteState m) => MonadRoute m

instance Monad m => MonadRoute (RouteT m)

defeatPokemon :: MonadRoute m => Species -> Integer -> Bool -> Integer -> m ()
defeatPokemon enemySpecies enemyLevel isTrainer participants = party._head %= defeatPokemon' enemySpecies enemyLevel isTrainer participants

runTrainer :: (MonadRoute m, MonadIO m) => Integer -> BattleT IO a -> m ()
runTrainer offset battle =
    do
        initialState <- trainerBattleState offset
        result <- liftIO $ runBattleT battle initialState
        pokemonDefeated += 1 + length (initialState^.enemyBench)
        case result of
            Left e -> throwError e
            Right (_, endState) -> party .= partyAfterBattle endState

defeatTrainer :: (MonadRoute m, MonadIO m) => Integer -> m ()
defeatTrainer offset = runTrainer offset defeatBattle

evolveTo :: MonadRoute m => String -> m ()
evolveTo sName =
    case Map.lookup sName speciesByName of
        Nothing -> throwError ("Could not find species " ++ sName)
        Just s -> party . _head %= evolveTo' s

printStats :: (MonadIO m, MonadRoute m) => m ()
printStats = preuse (party . _head) >>= maybe (return ()) printStats'

printStats' :: MonadIO m => PartyPokemon -> m ()
printStats' poke = liftIO $ do
    printf "L%d %s Exp Needed: %d/%d\n"
        (poke^.pLevel)
        (poke^.pSpecies.name)
        (lowestExpForLevel (poke^.pSpecies.expCurve) (poke^.pLevel + 1) - poke^.pExperience)
        (lowestExpForLevel (poke^.pSpecies.expCurve) (poke^.pLevel + 1) - lowestExpForLevel (poke^.pSpecies.expCurve) (poke^.pLevel))
    printf "          HP   Atk   Def   Spd   Spc\n"
    printf "       %5d %5d %5d %5d %5d\n" (poke^.pStats.hpStat) (poke^.pStats.atkStat) (poke^.pStats.defStat) (poke^.pStats.spdStat) (poke^.pStats.spcStat)
    printf "DV     %5d %5d %5d %5d %5d\n" (poke^.pDVs.hpDV) (poke^.pDVs.atkDV) (poke^.pDVs.defDV) (poke^.pDVs.spdDV) (poke^.pDVs.spcDV)
    printf "SExp   %5d %5d %5d %5d %5d\n" (poke^.pStatExp.hpStatExp) (poke^.pStatExp.atkStatExp) (poke^.pStatExp.defStatExp) (poke^.pStatExp.spdStatExp) (poke^.pStatExp.spcStatExp)
    printf "\n"

printRanges :: (MonadIO m, MonadRoute m) => m ()
printRanges = preuse (party . _head) >>= maybe (return ()) printRanges'

printRanges' :: MonadIO m => PartyPokemon -> m ()
printRanges' poke = liftIO $ do
    printf "L%d %s\n" (poke^.pLevel) (poke^.pSpecies.name)
    printf "DV  |"
    for_ [0 .. 15 :: Integer] $ \dv ->
        printf "%4d|" dv
    printf "\n"
    printf (replicate (5*17) '-')
    printf "\n"
    printf "HP  |"
    for_ [0 .. 15 :: Integer] $ \dv ->
        printf "%4d|" (computeHP (poke^.pSpecies.baseHP) (poke^.pLevel) dv (poke^.pStatExp.hpStatExp))
    printf "\n"
    for_ [("Atk", baseAtk, atkStatExp), ("Def", baseDef, defStatExp), ("Spd", baseSpd, spdStatExp), ("Spc", baseSpc, spcStatExp)] $ \(statName, baseLens, statExpLens) -> do
        printf "%s |" statName
        for_ [0 .. 15 :: Integer] $ \dv ->
            printf "%4d|" (computeStat (poke^.pSpecies.baseLens) (poke^.pLevel) dv (poke^.pStatExpAtLevel.statExpLens))
        printf "\n"
    printf "\n"

learnMove :: MonadRoute m => String -> m ()
learnMove name =
    case Map.lookup name movesByName of
        Nothing -> throwError ("Could not find move: " ++ name)
        Just m -> party . _head %= learnMove' m

learnMove' :: Move -> PartyPokemon -> PartyPokemon
learnMove' m poke =
    poke & pMoves .~ (poke^.pMoves ++ [m])

unlearnMove :: MonadRoute m => String -> m ()
unlearnMove name =
    case Map.lookup name movesByName of
        Nothing -> return ()
        Just m -> party . _head %= unlearnMove' m

unlearnMove' :: Move -> PartyPokemon -> PartyPokemon
unlearnMove' m = pMoves %~ filter (/= m)

trainerBattleState :: MonadRoute m => Integer -> m BattleState
trainerBattleState offset =
    case Map.lookup offset trainersByOffset of
        Nothing -> throwError (printf "Could not find trainer offset 0x%X" offset)
        Just t ->
            case trainerBattleParty t of
                [] -> throwError (printf "Trainer offset 0x%X has an empty party" offset)
                eLead:eRest -> do
                    pParty <- use party
                    curBadges <- use badges
                    case pParty of
                        [] -> throwError "Player has an empty party"
                        pLead:pRest ->
                            return $
                                BattleState
                                    { _playerActive =
                                        FromParty
                                            { _fpIndex = 0
                                            , _fpData = participant False curBadges pLead
                                            }
                                    , _playerBench =
                                        zipWith (\i d ->
                                            FromParty
                                                { _fpIndex = i
                                                , _fpData = d
                                                }
                                        ) [1..] pRest
                                    , _enemyActive =
                                        FromParty
                                            { _fpIndex = 0
                                            , _fpData = participant True noBadges eLead
                                            }
                                    , _enemyBench =
                                        zipWith (\i d ->
                                            FromParty
                                                { _fpIndex = i
                                                , _fpData = d
                                                }
                                        ) [1..] eRest
                                    , _playerBadges = curBadges
                                    , _turnCount = 0
                                    , _halfTurnCount = 0
                                    , _frameCount = 0
                                    }

defeatTrainerWithRanges :: (MonadRoute m, MonadIO m) => Integer -> m ()
defeatTrainerWithRanges offset =
    do
        case Map.lookup offset trainersByOffset of
            Nothing -> throwError (printf "Could not find trainer offset 0x%X" offset)
            Just t ->
                liftIO $ printf "%s (0x%X)\n" (show (t^.tClass)) (t^.tOffset)
        runTrainer offset defeatBattleWithRanges

simulateTrainerBattle :: (MonadRoute m, MonadRandom m) => Integer -> BattleT m PlayerBattleAction -> m (BattleResult, BattleState)
simulateTrainerBattle offset playerStrategy =
    do
        case Map.lookup offset trainersByOffset of
            Nothing -> throwError (printf "Could not find trainer offset 0x%X" offset)
            Just t -> do
                initialState <- trainerBattleState offset
                let enemyStrategy = trainerStrategy (t^.tClass)
                let enemySpecialAI = trainerSpecialAI (t^.tClass)
                result <- runBattleT (simulateBattle playerStrategy enemyStrategy enemySpecialAI) initialState
                case result of
                    Left e -> throwError e
                    Right r -> pure r

printTrainerInfo :: (MonadRoute m, MonadIO m) => Integer -> m ()
printTrainerInfo offset =
    do
        case Map.lookup offset trainersByOffset of
            Nothing -> throwError (printf "Could not find trainer offset 0x%X" offset)
            Just t -> do
                let descriptions = map (\tp -> printf "L%d %s" (tp^.tpLevel) (tp^.tpSpecies.name)) (t^.tParty)
                liftIO $ printf "%s (0x%X: %s)\n" (show (t^.tClass)) (t^.tOffset) (intercalate ", " descriptions)

counts :: Ord a => [a] -> [(a, Integer)]
counts = Map.toList . foldr (\k -> Map.insertWith (+) k 1) Map.empty

summarizeResults :: MonadIO m => [(BattleResult, BattleState)] -> m ()
summarizeResults results =
    do
        let winCount = length . filter (\(r, _) -> r == Victory) $ results
            lossCount = length . filter (\(r, _) -> r == Defeat) $ results
            turnCounts = map (\(_, s) -> s^.turnCount) results
            victoryTurnCounts = map (\(_, s) -> s^.turnCount) . filter (\(r, _) -> r == Victory) $ results
            halfTurnCounts = map (\(_, s) -> s^.halfTurnCount) results
            victoryHalfTurnCounts = map (\(_, s) -> s^.halfTurnCount) . filter (\(r, _) -> r == Victory) $ results
            numTrials = length results
        liftIO $ printf "Winrate %.2f%% (%d/%d)\n" (100 * fromIntegral winCount / fromIntegral numTrials :: Double) winCount numTrials
        liftIO $ printf "Average turns: %.2f\n" (fromIntegral (sum turnCounts) / fromIntegral (length turnCounts) :: Double)
        liftIO $ printf "Average victory turns: %.2f\n" (fromIntegral (sum victoryTurnCounts) / fromIntegral (length victoryTurnCounts) :: Double)
        liftIO $ printf "Average half turns: %.2f\n" (fromIntegral (sum halfTurnCounts) / fromIntegral (length halfTurnCounts) :: Double)
        liftIO $ printf "Average victory half turns: %.2f\n" (fromIntegral (sum victoryHalfTurnCounts) / fromIntegral (length victoryHalfTurnCounts) :: Double)
        liftIO $ printf "Turn distribution:\n"
        for_ (reverse . sortOn snd . counts . map (\(r, s) -> (r, s^.turnCount, s^.halfTurnCount)) $ results) $ \((r, t, ht), n) -> do
            liftIO $ printf "  %d turn %s (%d half-turns): %.2f%%\n" t (show r) ht (100 * fromInteger n / fromIntegral numTrials :: Double)
