module Pokemon.EnemyAI where

import Control.Lens
import Control.Monad.Random
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Tuple

import Pokemon.Battle
import Pokemon.Moves
import Pokemon.Party
import Pokemon.Species
import Pokemon.Status
import Pokemon.Trainer
import Pokemon.Type

-- TODO: Disabled move
enemyMoveDistribution :: (MonadBattle m, Num a) => [Move -> m Integer] -> m [(a, Move)]
enemyMoveDistribution mods =
    do
        moveChoices <- use (enemyActive . fpData . partyData . pMoves)
        priorities <-
            for moveChoices $ \m -> do
                modResults <- for mods ($ m)
                pure (sum modResults)
        let normalizedPriorities = map (subtract (minimum priorities)) priorities
        pure $ map snd . filter ((== 0) . fst) . zip normalizedPriorities . zip [63, 64, 63, 66] $ moveChoices

trainerStrategy :: (MonadBattle m, MonadRandom m) => TrainerClass -> m Move
trainerStrategy c = do
    let mods = trainerAIMods c
    dist <- enemyMoveDistribution mods
    fromList (map swap dist)

trainerAIModification1 :: MonadBattle m => Move -> m Integer
trainerAIModification1 m =
    do
        playerStatus <- use (playerActive . fpData . partyData . pStatus)
        case playerStatus of
            Healthy -> pure 0
            _ ->
                if elem (m^.effect)
                    [ SleepEffect
                    , PoisonEffect
                    , ParalyzeEffect
                    ]
                then pure 5
                else pure 0

trainerAIModification2 :: MonadBattle m => Move -> m Integer
trainerAIModification2 m =
    do
        turns <- use (enemyActive . fpData . turnsInBattle)
        -- The turn counter starts at 0, so this takes effect on the second turn
        if (turns == 1 && elem (m^.effect)
            [ AttackUp1Effect
            , DefenseUp1Effect
            , SpeedUp1Effect
            , SpecialUp1Effect
            , AccuracyUp1Effect
            , EvasionUp1Effect
            , PayDayEffect
            , SwiftEffect
            , AttackDown1Effect
            , DefenseDown1Effect
            , SpeedDown1Effect
            , SpecialDown1Effect
            , AccuracyDown1Effect
            , EvasionDown1Effect
            , ConversionEffect
            , HazeEffect
            , AttackUp2Effect
            , DefenseUp2Effect
            , SpeedUp2Effect
            , SpecialUp2Effect
            , AccuracyUp2Effect
            , EvasionUp2Effect
            , HealEffect
            , TransformEffect
            , AttackDown2Effect
            , DefenseDown2Effect
            , SpeedDown2Effect
            , SpecialDown2Effect
            , AccuracyDown2Effect
            , EvasionDown2Effect
            , LightScreenEffect
            , ReflectEffect
            ])
            then pure (-1)
            else pure 0

trainerAIModification3 :: MonadBattle m => Move -> m Integer
trainerAIModification3 m =
    do
        defender <- use (playerActive . fpData . partyData)
        case compare (aiMoveEffectiveness m defender) neutral of
            LT -> pure 1
            EQ -> pure 0
            GT -> do
                alternatives <- use (enemyActive . fpData . partyData . pMoves)
                if any isBetterMove alternatives
                    then pure (-1)
                    else pure 0
    where
    isBetterMove :: Move -> Bool
    isBetterMove m' =
        case m'^.effect of
            SuperFangEffect -> True
            SpecialDamageEffect -> True
            FlyEffect -> True
            _ -> m'^.moveType /= m^.moveType && m'^.power > 0

aiMoveEffectiveness :: Move -> PartyPokemon -> TypeEffectiveness
aiMoveEffectiveness move defender =
    fromMaybe neutral . getFirst $ foldMap (\(offType, defType) ->
        if offType == move^.moveType && (defType == defender^.pSpecies.type1 || Just defType == defender^.pSpecies.type2)
        then First . Just $ effectiveness offType defType
        else First Nothing
        ) typePairs
    where
    typePairs =
        [ (Water, Fire)
        , (Fire, Grass)
        , (Fire, Ice)
        , (Grass, Water)
        , (Electric, Water)
        , (Water, Rock)
        , (Ground, Flying)
        , (Water, Water)
        , (Fire, Fire)
        , (Electric, Electric)
        , (Ice, Ice)
        , (Grass, Grass)
        , (Pokemon.Type.Psychic, Pokemon.Type.Psychic)
        , (Fire, Water)
        , (Grass, Fire)
        , (Water, Grass)
        , (Normal, Rock)
        , (Normal, Ghost)
        , (Ghost, Ghost)
        , (Fire, Bug)
        , (Fire, Rock)
        , (Water, Ground)
        , (Electric, Ground)
        , (Electric, Flying)
        , (Grass, Ground)
        , (Grass, Bug)
        , (Grass, Poison)
        , (Grass, Rock)
        , (Grass, Flying)
        , (Ice, Water)
        , (Ice, Grass)
        , (Ice, Ground)
        , (Ice, Flying)
        , (Fighting, Normal)
        , (Fighting, Poison)
        , (Fighting, Flying)
        , (Fighting, Pokemon.Type.Psychic)
        , (Fighting, Bug)
        , (Fighting, Rock)
        , (Fighting, Ice)
        , (Fighting, Ghost)
        , (Poison, Grass)
        , (Poison, Poison)
        , (Poison, Ground)
        , (Poison, Bug)
        , (Poison, Rock)
        , (Poison, Ghost)
        , (Ground, Fire)
        , (Ground, Electric)
        , (Ground, Grass)
        , (Ground, Bug)
        , (Ground, Rock)
        , (Ground, Poison)
        , (Flying, Electric)
        , (Flying, Fighting)
        , (Flying, Bug)
        , (Flying, Grass)
        , (Flying, Rock)
        , (Pokemon.Type.Psychic, Fighting)
        , (Pokemon.Type.Psychic, Poison)
        , (Bug, Fire)
        , (Bug, Grass)
        , (Bug, Fighting)
        , (Bug, Flying)
        , (Bug, Pokemon.Type.Psychic)
        , (Bug, Ghost)
        , (Bug, Poison)
        , (Rock, Fire)
        , (Rock, Fighting)
        , (Rock, Ground)
        , (Rock, Flying)
        , (Rock, Bug)
        , (Rock, Ice)
        , (Ghost, Normal)
        , (Ghost, Pokemon.Type.Psychic)
        , (Fire, Dragon)
        , (Water, Dragon)
        , (Electric, Dragon)
        , (Grass, Dragon)
        , (Ice, Dragon)
        , (Dragon, Dragon)
        ]

trainerAIMods :: MonadBattle m => TrainerClass -> [Move -> m Integer]
trainerAIMods c =
    case c of
        Youngster -> []
        BugCatcher -> [trainerAIModification1]
        Lass -> [trainerAIModification1]
        Sailor -> [trainerAIModification1, trainerAIModification3]
        JrTrainerM -> [trainerAIModification1]
        JrTrainerF -> [trainerAIModification1]
        Pokemaniac -> [trainerAIModification1, trainerAIModification2, trainerAIModification3]
        SuperNerd -> [trainerAIModification1, trainerAIModification2]
        Hiker -> [trainerAIModification1]
        Biker -> [trainerAIModification1]
        Burglar -> [trainerAIModification1, trainerAIModification3]
        Engineer -> [trainerAIModification1]
        JugglerX -> [trainerAIModification1, trainerAIModification2]
        Fisher -> [trainerAIModification1, trainerAIModification3]
        Swimmer -> [trainerAIModification1, trainerAIModification3]
        Cueball -> []
        Gambler -> [trainerAIModification1]
        Beauty -> [trainerAIModification1, trainerAIModification3]
        Pokemon.Trainer.Psychic -> [trainerAIModification1, trainerAIModification2]
        Rocker -> [trainerAIModification1, trainerAIModification3]
        Juggler -> [trainerAIModification1]
        Tamer -> [trainerAIModification1]
        Birdkeeper -> [trainerAIModification1]
        Blackbelt -> [trainerAIModification1]
        Rival1 -> [trainerAIModification1]
        ProfOak -> [trainerAIModification1, trainerAIModification3]
        Scientist -> [trainerAIModification1, trainerAIModification2]
        Giovanni -> [trainerAIModification1, trainerAIModification3]
        Rocket -> [trainerAIModification1]
        CoolTrainerM -> [trainerAIModification1, trainerAIModification3]
        CoolTrainerF -> [trainerAIModification1, trainerAIModification3]
        Bruno -> [trainerAIModification1]
        Brock -> [trainerAIModification1]
        Misty -> [trainerAIModification1, trainerAIModification3]
        LtSurge -> [trainerAIModification1, trainerAIModification3]
        Erika -> [trainerAIModification1, trainerAIModification3]
        Koga -> [trainerAIModification1, trainerAIModification3]
        Blaine -> [trainerAIModification1, trainerAIModification3]
        Sabrina -> [trainerAIModification1, trainerAIModification3]
        Gentleman -> [trainerAIModification1, trainerAIModification2]
        Rival2 -> [trainerAIModification1, trainerAIModification3]
        Rival3 -> [trainerAIModification1, trainerAIModification3]
        Lorelei -> [trainerAIModification1, trainerAIModification2, trainerAIModification3]
        Channeler -> [trainerAIModification1]
        Agatha -> [trainerAIModification1]
        Lance -> [trainerAIModification1, trainerAIModification3]

-- TODO: Implement special AIs
noSpecialAI :: (MonadBattle m, MonadRandom m) => m Bool
noSpecialAI = pure False
