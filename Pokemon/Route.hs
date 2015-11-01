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
import Text.Printf

import Pokemon.Battle
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
        }
    deriving (Eq, Show, Ord)

makeLenses ''RouteState

emptyParty :: RouteState
emptyParty =
    RouteState
        { _party = []
        , _badges = noBadges
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

class (MonadError String m, MonadState RouteState m) => MonadRoute m

instance Monad m => MonadRoute (RouteT m)

defeatPokemon :: MonadRoute m => Species -> Integer -> Bool -> Integer -> m ()
defeatPokemon enemySpecies enemyLevel isTrainer participants = party._head %= defeatPokemon' enemySpecies enemyLevel isTrainer participants

defeatTrainer :: MonadRoute m => Integer -> m ()
defeatTrainer offset = 
    do
        case Map.lookup offset trainersByOffset of
            Nothing -> throwError (printf "Could not find trainer offset 0x%X" offset)
            Just t ->
                for_ (t^.tParty) $ \enemy ->
                    party._head %= defeatPokemon' (enemy^.tpSpecies) (enemy^.tpLevel) True 1

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
                                            , _fpData = participant curBadges pLead
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
                                            , _fpData = participant noBadges eLead
                                            }
                                    , _enemyBench =
                                        zipWith (\i d ->
                                            FromParty
                                                { _fpIndex = i
                                                , _fpData = d
                                                }
                                        ) [1..] eRest
                                    , _playerBadges = curBadges
                                    }

defeatTrainerWithRanges :: (MonadRoute m, MonadIO m) => Integer -> m ()
defeatTrainerWithRanges offset =
    do
        case Map.lookup offset trainersByOffset of
            Nothing -> throwError (printf "Could not find trainer offset 0x%X" offset)
            Just t ->
                liftIO $ printf "%s (0x%X)\n" (show (t^.tClass)) (t^.tOffset)
        initialState <- trainerBattleState offset
        result <- liftIO $ runBattleT defeatBattleWithRanges initialState
        case result of
            Left e -> throwError e
            Right (_, endState) -> party .= partyAfterBattle endState
