module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.Maybe
import System.IO
import Text.Printf

import qualified Data.Map as Map
import Pokemon.Battle
import Pokemon.Item
import Pokemon.Moves
import Pokemon.Party
import Pokemon.Route
import Pokemon.Species
import Pokemon.Stats
import Pokemon.Status
import Pokemon.Trainer

localState :: MonadState s m => m a -> m a
localState x = do
    s <- get
    r <- x
    put s
    pure r

route :: RouteT IO ()
route = do
    let 
        nidoDVs = DVs
            { _atkDV = 15
            , _defDV = 0
            , _spdDV = 15
            , _spcDV = 15
            }
        squirtleDVs = DVs
            { _atkDV = 13
            , _defDV = 12
            , _spdDV = 12
            , _spcDV = 12
            }

    {-
    party .=
        [ partyPokemon (speciesByName Map.! "Squirtle") 5 squirtleDVs ]
    results <- replicateM 100 $ do
        simulateTrainerBattle 0x3A1E7 $ do
            enemyDefMod <- use (enemyActive.fpData.battleStatMods.defMod)
            if enemyDefMod > (-1)
                then pure $ PUseMove (movesByName Map.! "Tail Whip")
                else do
                    turn <- use turnCount
                    playerAtkMod <- use (playerActive.fpData.battleStatMods.atkMod)
                    if turn == 1 && playerAtkMod == (-1)
                        then pure $ PUseMove (movesByName Map.! "Tail Whip")
                        else pure $ PUseMove (movesByName Map.! "Tackle")

    printTrainerInfo 0x3A1E7
    summarizeResults results
    -}

    party .=
        [ partyPokemon (speciesByName Map.! "NidoranM") 3 nidoDVs
        , partyPokemon (speciesByName Map.! "Squirtle") 8 squirtleDVs
        ]

    learnMove "Leer"
    learnMove "Tackle"

    -- Defeat Brock
    for_ ((trainersByOffset Map.! 0x3A3B5) ^. tParty) $ \enemy -> do
        party . each %= defeatPokemon' (enemy^.tpSpecies) (enemy^.tpLevel) True 2
        -- preuse (party . _head) >>= maybe (return ()) printRanges'

    badges . boulderBadge .= True
    learnMove "Horn Attack"

    -- Route 3
    defeatTrainer 0x39DDA
    defeatTrainer 0x39D99
    defeatTrainer 0x39DDF
    defeatTrainer 0x39DE5

    learnMove "Poison Sting"

    -- Mt Moon
    defeatTrainer 0x39F26
    -- defeatTrainer 0x39E1C

    -- party . _head %= defeatPokemon' (speciesByName Map.! "Zubat") 8 False 1
    -- party . _head %= defeatPokemon' (speciesByName Map.! "Zubat") 8 False 1
    -- party . _head %= defeatPokemon' (speciesByName Map.! "Zubat") 8 False 1
    
    printStats
    defeatTrainer 0x3A29C
    printStats
    defeatTrainerWithRanges 0x39F2A
    printStats

    evolveTo "Nidorino"
    evolveTo "Nidoking"
    learnMove "Mega Punch"
    learnMove "Water Gun"

    -- Nugget Bridge
    defeatTrainer 0x3A209
    defeatTrainer 0x39DF2
    defeatTrainer 0x39E27
    defeatTrainer 0x39DA5
    defeatTrainer 0x39E23
    defeatTrainer 0x39E80
    defeatTrainer 0x3A2B0

    -- Route 25
    defeatTrainerWithRanges 0x39F63 -- WG Skip hiker
    -- defeatTrainer 0x39F6D
    -- defeatTrainerWithRanges 0x39DAA
    defeatTrainer 0x39E2B
    defeatTrainer 0x39E7C
    -- defeatTrainerWithRanges 0x39DB1
    defeatTrainer 0x39E2F

    -- Cerulean Gym
    defeatTrainer 0x39E9D
    printStats
    party . _head %= rarecandy
    party . _head %= rarecandy
    learnMove "Thrash"
    unlearnMove "Leer"
    defeatTrainer 0x3A3BB

    -- Route 6
    defeatTrainer 0x3A2AC
    defeatTrainer 0x39EA4
    defeatTrainer 0x39E86

    learnMove "Bubblebeam"
    unlearnMove "Tackle"

    defeatTrainer 0x3A40B

    -- Vermillion Gym
    defeatTrainer 0x3A3C1
    badges . thunderBadge .= True

    learnMove "Thunderbolt"
    unlearnMove "Horn Attack"

    -- Route 9
    defeatTrainer 0x39EAC
    defeatTrainer 0x39E07

    -- Rock Tunnel
    defeatTrainer 0x39F22
    defeatTrainer 0x39F1A
    defeatTrainer 0x39EC2
    defeatTrainer 0x39F81
    defeatTrainer 0x39EE8

    -- Route 8
    defeatTrainer 0x3A0CD

    learnMove "Horn Drill"
    unlearnMove "Bubblebeam"

    learnMove "Rock Slide"
    unlearnMove "Poison Sting"

    -- Lavender Tower
    defeatTrainer 0x3A42B
    defeatTrainer 0x3A4E3
    defeatTrainer 0x3A507
    defeatTrainer 0x3A504

    defeatTrainer 0x3A2ED
    defeatTrainer 0x3A2F2
    defeatTrainer 0x3A2F6

    learnMove "Ice Beam"
    unlearnMove "Rock Slide"

    -- Silph
    defeatTrainer 0x3A34B
    learnMove "Earthquake"
    unlearnMove "Thrash"

    defeatTrainer 0x3A319
    defeatTrainer 0x3A44F
    defeatTrainer 0x3A355
    defeatTrainer 0x3A286

    -- Fuchsia Gym
    defeatTrainer 0x3A13A
    party . _head %= rarecandy
    defeatTrainer 0x3A140
    defeatTrainer 0x3A3D1
    badges . soulBadge .= True
    party . _head %= rarecandy
    party . _head %= rarecandy

    party . _head %= rarecandy
    party . _head %= rarecandy

    -- Celadon Gym
    defeatTrainer 0x3A0DB
    defeatTrainer 0x3A3C9

    -- Cinnabar Gym
    defeatTrainer 0x3A3DB
    badges . volcanoBadge .= True

    -- Saffron Gym
    defeatTrainer 0x3A3E5

    -- Viridian Gym
    defeatTrainer 0x3A382
    defeatTrainer 0x3A1DA
    defeatTrainer 0x3A290

    -- Route 22
    defeatTrainer 0x3A475

    -- Elite Four
    defeatTrainer 0x3A4BB
    defeatTrainer 0x3A3A9

    -- TODO: Figure out why Nidoking isn't already full HP here.
    party . _head %= \p -> p & pCurHP .~ (p^.pStats.hpStat)
    let setuplessAgatha :: MonadBattle m => m PlayerBattleAction
        setuplessAgatha = fmap (either id id) . runExceptT $ do
            player <- use playerActive
            when (player^.fpIndex /= 0) $ earlyReturn PForfeit
            case player^.fpData.partyData.pStatus of
                SLP _ -> earlyReturn (PUseItem Pokeflute)
                _ -> pure ()
            enemy <- use enemyActive
            case enemy^.fpIndex of
                0 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                1 -> pure $ PUseMove (movesByName Map.! "Ice Beam")
                2 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                3 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                4 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                _ -> lift $ throwError "Unknown enemy index"
        xspeedAgatha :: MonadBattle m => m PlayerBattleAction
        xspeedAgatha = fmap (either id id) . runExceptT $ do
            player <- use playerActive
            when (player^.fpIndex /= 0) $ earlyReturn PForfeit
            case player^.fpData.partyData.pStatus of
                SLP _ -> earlyReturn (PUseItem Pokeflute)
                _ -> pure ()
            when (player^.fpData.turnsInBattle == 0) $ earlyReturn (PUseItem XSpeed)
            enemy <- use enemyActive
            case enemy^.fpIndex of
                0 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                1 -> pure $ PUseMove (movesByName Map.! "Ice Beam")
                2 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                3 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                4 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                _ -> lift $ throwError "Unknown enemy index"
        xspecialAgatha :: MonadBattle m => m PlayerBattleAction
        xspecialAgatha = fmap (either id id) . runExceptT $ do
            player <- use playerActive
            when (player^.fpIndex /= 0) $ earlyReturn PForfeit
            case player^.fpData.partyData.pStatus of
                SLP _ -> earlyReturn (PUseItem Pokeflute)
                _ -> pure ()
            when (player^.fpData.turnsInBattle == 0) $ earlyReturn (PUseItem XSpecial)
            enemy <- use enemyActive
            case enemy^.fpIndex of
                0 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                1 -> pure $ PUseMove (movesByName Map.! "Ice Beam")
                2 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                3 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                4 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                _ -> lift $ throwError "Unknown enemy index"
        xaccAgatha :: MonadBattle m => m PlayerBattleAction
        xaccAgatha = fmap (either id id) . runExceptT $ do
            player <- use playerActive
            when (player^.fpIndex /= 0) $ earlyReturn PForfeit
            case player^.fpData.partyData.pStatus of
                SLP _ -> earlyReturn (PUseItem Pokeflute)
                _ -> pure ()
            when (player^.fpData.turnsInBattle == 0) $ earlyReturn (PUseItem XAccuracy)
            enemy <- use enemyActive
            case enemy^.fpIndex of
                0 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                1 -> pure $ PUseMove (movesByName Map.! "Horn Drill")
                2 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                3 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                4 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                _ -> lift $ throwError "Unknown enemy index"
        xaccxspdAgatha :: MonadBattle m => m PlayerBattleAction
        xaccxspdAgatha = fmap (either id id) . runExceptT $ do
            player <- use playerActive
            when (player^.fpIndex /= 0) $ earlyReturn PForfeit
            case player^.fpData.partyData.pStatus of
                SLP _ -> earlyReturn (PUseItem Pokeflute)
                _ -> pure ()
            when (player^.fpData.turnsInBattle == 0) $ earlyReturn (PUseItem XAccuracy)
            enemy <- use enemyActive
            when (enemy^.fpIndex == 1) $ earlyReturn (PUseMove (movesByName Map.! "Horn Drill"))
            when (player^.fpData.battleStatMods.spdMod == 0) $ earlyReturn (PUseItem XSpeed)
            case enemy^.fpIndex of
                0 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                1 -> pure $ PUseMove (movesByName Map.! "Horn Drill")
                2 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                3 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                4 -> pure $ PUseMove (movesByName Map.! "Earthquake")
                _ -> lift $ throwError "Unknown enemy index"

    printRanges

    printTrainerInfo 0x3A516
    {-
    liftIO $ printf "\nSetupless\n=========\n"
    results <- replicateM trials $ simulateTrainerBattle 0x3A516 setuplessAgatha
    summarizeResults results
    liftIO $ printf "\nXSpeed\n=========\n"
    results <- replicateM trials $ simulateTrainerBattle 0x3A516 xspeedAgatha
    summarizeResults results
    liftIO $ printf "\nXSpecial\n=========\n"
    results <- replicateM trials $ simulateTrainerBattle 0x3A516 xspecialAgatha
    summarizeResults results
    liftIO $ printf "\nXAccuracy\n=========\n"
    results <- replicateM trials $ simulateTrainerBattle 0x3A516 xaccAgatha
    summarizeResults results
    liftIO $ printf "\nXAccuracy + XSpeed\n=========\n"
    results <- replicateM trials $ simulateTrainerBattle 0x3A516 xaccxspdAgatha
    summarizeResults results
    let trials = 1000
        createAgathaChart strat =
            localState $ do
                liftIO $ printf "       |"
                for_ ([0..15] :: [Integer]) $ \spc -> do
                    liftIO $ printf "    %2d SPC    |" spc
                liftIO $ printf "\n"
                for_ [0..15] $ \spd -> do
                    liftIO $ printf "%2d SPD |" spd
                    for_ [0..15] $ \spc -> do
                        party . _head . pDVs . spdDV .= spd
                        party . _head . pDVs . spcDV .= spc
                        party . _head %= updateStats
                        results <- replicateM trials $ simulateTrainerBattle 0x3A516 strat
                        let victoryHalfTurnCounts = map (\(_, s) -> s^.halfTurnCount) . filter (\(r, _) -> r == Victory) $ results
                            avg = fromIntegral (sum victoryHalfTurnCounts) / fromIntegral (length victoryHalfTurnCounts) :: Double
                            numWins = length . filter (\(r, _) -> r == Victory) $ results
                            winrate = 100 * fromIntegral numWins / fromIntegral trials :: Double
                        liftIO $ printf " %4.1f%% %6.2f |" winrate avg
                        liftIO $ hFlush stdout
                    liftIO $ printf "\n"
                
    liftIO $ printf "\nSetupless\n=========\n"
    createAgathaChart setuplessAgatha
    liftIO $ printf "\nXSpeed\n=========\n"
    createAgathaChart xspeedAgatha
    liftIO $ printf "\nXSpecial\n=========\n"
    createAgathaChart xspecialAgatha
    liftIO $ printf "\nXAccuracy\n=========\n"
    createAgathaChart xaccAgatha
    liftIO $ printf "\nXAccuracy + XSpeed\n=========\n"
    createAgathaChart xaccxspdAgatha
    -}

    defeatTrainer 0x3A516
    defeatTrainer 0x3A522
    defeatTrainer 0x3A49F

main :: IO ()
main = do
    res <- runRouteT route emptyParty
    case res of
        Left e -> printf "Error: %s\n" e
        Right _ -> printf "Total pokemon defeated: %d\n" (fromJust $ res^?_Right._2.pokemonDefeated)
