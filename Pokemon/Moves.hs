{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Moves where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map

import Pokemon.Type

data Move =
    Move
        { _moveName :: String
        , _moveType :: Type
        , _pp :: Integer
        , _power :: Integer
        , _accuracy :: Integer
        -- TODO: Effect
        }
    deriving (Eq, Show, Ord)

makeLenses ''Move

data MoveEffect
    = NoEffect
    | PoisonSideEffect1
    | DrainHPEffect
    | BurnSideEffect1
    | FreezeSideEffect
    | ParalyzeSideEffect1
    | ExplodeEffect
    | DreamEaterEffect
    | MirrorMoveEffect
    | AttackUp1Effect
    | DefenseUp1Effect
    | SpeedUp1Effect
    | SpecialUp1Effect
    | AccuracyUp1Effect
    | EvasionUp1Effect
    | PayDayEffect
    | SwiftEffect
    | AttackDown1Effect
    | DefenseDown1Effect
    | SpeedDown1Effect
    | SpecialDown1Effect
    | AccuracyDown1Effect
    | EvasionDown1Effect
    | ConversionEffect
    | HazeEffect
    | BideEffect
    | ThrashPetalDanceEffect
    | SwitchAndTeleportEffect
    | TwoToFiveAttacksEffect
    | FlinchSideEffect1
    | SleepEffect
    | PoisonSideEffect2
    | BurnSideEffect2
    | ParalyzeSideEffect2
    | FlinchSideEffect2
    | OHKOEffect
    | ChargeEffect
    | SuperFangEffect
    | SpecialDamageEffect
    | TrappingEffect
    | FlyEffect
    | AttackTwiceEffect
    | JumpKickEffect
    | MistEffect
    | FocusEnergyEffect
    | RecoilEffect
    | ConfusionEffect
    | AttackUp2Effect
    | DefenseUp2Effect
    | SpeedUp2Effect
    | SpecialUp2Effect
    | AccuracyUp2Effect
    | EvasionUp2Effect
    | HealEffect
    | TransformEffect
    | AttackDown2Effect
    | DefenseDown2Effect
    | SpeedDown2Effect
    | SpecialDown2Effect
    | AccuracyDown2Effect
    | EvasionDown2Effect
    | LightScreenEffect
    | ReflectEffect
    | PoisonEffect
    | ParalyzeEffect
    | AttackDownSideEffect
    | DefenseDownSideEffect
    | SpeedDownSideEffect
    | SpecialDownSideEffect
    | ConfusionSideEffect
    | TwineedleEffect
    | SubstituteEffect
    | HyperBeamEffect
    | RageEffect
    | MimicEffect
    | MetronomeEffect
    | LeechSeedEffect
    | SplashEffect
    | DisableEffect
    deriving (Eq, Show, Ord)

move :: String -> MoveEffect -> Integer -> Type -> Integer -> Integer -> Move
move name effect power ty acc_pct pp =
    Move
        { _moveName = name
        , _moveType = ty
        , _pp = pp
        , _power = power
        , _accuracy = acc_pct * 0xFF `div` 100
        }

movesByName :: Map String Move
movesByName = Map.fromList (map (\m -> (m^.moveName, m)) allMoves)

allMoves =
    [ move "Pound" NoEffect 40 Normal 100 35
    , move "Karate Chop" NoEffect 50 Normal 100 25
    , move "Doubleslap" TwoToFiveAttacksEffect 15 Normal 85 10
    , move "Comet Punch" TwoToFiveAttacksEffect 15 Normal 85 10
    , move "Mega Punch" NoEffect 80 Normal 85 20
    , move "Pay Day" PayDayEffect 40 Normal 100 20
    , move "Fire Punch" BurnSideEffect1 75 Fire 100 15
    , move "Ice Punch" FreezeSideEffect 75 Ice 100 15
    , move "Thunderpunch" ParalyzeSideEffect1 75 Electric 100 15
    , move "Scratch" NoEffect 40 Normal 100 35
    , move "Vicegrip" NoEffect 55 Normal 100 30
    , move "Guillotine" OHKOEffect 1 Normal 30 5
    , move "Razor Wind" ChargeEffect 80 Normal 75 10
    , move "Swords Dance" AttackUp2Effect 0 Normal 100 30
    , move "Cut" NoEffect 50 Normal 95 30
    , move "Gust" NoEffect 40 Normal 100 35
    , move "Wing Attack" NoEffect 35 Flying 100 35
    , move "Whirlwind" SwitchAndTeleportEffect 0 Normal 85 20
    , move "Fly" FlyEffect 70 Flying 95 15
    , move "Bind" TrappingEffect 15 Normal 75 20
    , move "Slam" NoEffect 80 Normal 75 20
    , move "Vine Whip" NoEffect 80 Normal 75 20
    , move "Stomp" FlinchSideEffect2 65 Normal 100 20
    , move "Double Kick" AttackTwiceEffect 30 Fighting 100 30
    , move "Mega Kick" NoEffect 120 Normal 75 5
    , move "Jump Kick" JumpKickEffect 70 Fighting 95 25
    , move "Rolling Kick" FlinchSideEffect2 60 Fighting 85 15
    , move "Sand Attack" AccuracyDown1Effect 0 Normal 100 15
    , move "Headbutt" FlinchSideEffect2 70 Normal 100 15
    , move "Horn Attack" NoEffect 65 Normal 100 25
    , move "Fury Attack" TwoToFiveAttacksEffect 15 Normal 85 20
    , move "Horn Drill" OHKOEffect 1 Normal 30 5
    , move "Tackle" NoEffect 35 Normal 95 35
    , move "Body Slam" ParalyzeSideEffect2 85 Normal 100 15
    , move "Wrap" TrappingEffect 15 Normal 85 20
    , move "Take Down" RecoilEffect 90 Normal 85 20
    , move "Thrash" ThrashPetalDanceEffect 90 Normal 100 20
    , move "Double Edge" RecoilEffect 100 Normal 100 15
    , move "Tail Whip" DefenseDown1Effect 0 Normal 100 30
    , move "Poison Sting" PoisonSideEffect1 15 Poison 100 35
    , move "Twineedle" TwineedleEffect 25 Bug 100 20
    , move "Pin Missile" TwoToFiveAttacksEffect 14 Bug 85 20
    , move "Leer" DefenseDown1Effect 0 Normal 100 30
    , move "Bite" FlinchSideEffect1 60 Normal 100 25
    , move "Growl" AttackDown1Effect 0 Normal 100 40
    , move "Roar" SwitchAndTeleportEffect 0 Normal 100 20
    , move "Sing" SleepEffect 0 Normal 55 15
    , move "Supersonic" ConfusionEffect 0 Normal 55 20
    , move "Sonicboom" SpecialDamageEffect 1 Normal 90 20
    , move "Disable" DisableEffect 0 Normal 55 20
    , move "Acid" DefenseDownSideEffect 40 Poison 100 30
    , move "Ember" BurnSideEffect1 40 Fire 100 25
    , move "Flamethrower" BurnSideEffect1 95 Fire 100 15
    , move "Mist" MistEffect 0 Ice 100 30
    , move "Water Gun" NoEffect 40 Water 100 25
    , move "Hydro Pump" NoEffect 120 Water 80 5
    , move "Surf" NoEffect 95 Water 100 15
    , move "Ice Beam" FreezeSideEffect 95 Ice 100 10
    , move "Blizzard" FreezeSideEffect 120 Ice 90 5
    , move "Psybeam" ConfusionSideEffect 65 Psychic 100 20
    , move "Bubblebeam" SpeedDownSideEffect 65 Water 100 20
    , move "Aurora Beam" AttackDownSideEffect 65 Ice 100 20
    , move "Hyper Beam" HyperBeamEffect 150 Normal 90 5
    , move "Peck" NoEffect 35 Flying 100 35
    , move "Drill Peck" NoEffect 35 Flying 100 35
    , move "Submission" RecoilEffect 80 Fighting 80 25
    , move "Low Kick" FlinchSideEffect2 50 Fighting 90 20
    , move "Counter" NoEffect 1 Fighting 100 20
    , move "Seismic Toss" SpecialDamageEffect 1 Fighting 100 20
    , move "Strength" NoEffect 80 Normal 100 15
    , move "Absorb" DrainHPEffect 20 Grass 100 20
    , move "Mega Drain" DrainHPEffect 40 Grass 100 10
    , move "Leech Seed" LeechSeedEffect 0 Grass 90 10
    , move "Growth" SpecialUp1Effect 0 Normal 100 40
    , move "Razor Leaf" NoEffect 55 Grass 95 25
    , move "Solarbeam" ChargeEffect 120 Grass 100 10
    , move "Poisonpowder" PoisonEffect 0 Poison 75 35
    , move "Stun Spore" ParalyzeEffect 0 Grass 75 30
    , move "Sleep Powder" SleepEffect 0 Grass 75 15
    , move "Petal Dance" ThrashPetalDanceEffect 70 Grass 100 20
    , move "String Shot" SpeedDown1Effect 0 Bug 95 40
    , move "Dragon Rage" SpecialDamageEffect 1 Dragon 100 10
    , move "Fire Spin" TrappingEffect 15 Fire 70 15
    , move "Thundershock" ParalyzeSideEffect1 40 Electric 100 30
    , move "Thunderbolt" ParalyzeSideEffect1 95 Electric 100 15
    , move "Thunder Wave" ParalyzeEffect 0 Electric 100 20
    , move "Thunder" ParalyzeSideEffect1 120 Electric 70 10
    , move "Rock Throw" NoEffect 50 Rock 65 15
    , move "Earthquake" NoEffect 100 Ground 100 10
    , move "Fissure" OHKOEffect 1 Ground 30 5
    , move "Dig" ChargeEffect 100 Ground 100 10
    , move "Toxic" PoisonEffect 0 Poison 85 10
    , move "Confusion" ConfusionSideEffect 50 Psychic 100 25
    , move "Psychic" SpecialDownSideEffect 90 Psychic 100 10
    , move "Hypnosis" SleepEffect 0 Psychic 60 20
    , move "Meditate" AttackUp1Effect 0 Psychic 100 40
    , move "Agility" SpeedUp2Effect 0 Psychic 100 30
    , move "Quick Attack" NoEffect 40 Normal 100 30
    , move "Rage" RageEffect 20 Normal 100 20
    , move "Teleport" SwitchAndTeleportEffect 0 Psychic 100 20
    , move "Night Shade" SpecialDamageEffect 0 Ghost 100 15
    , move "Mimic" MimicEffect 0 Normal 100 10
    , move "Screech" DefenseDown2Effect 0 Normal 85 40
    , move "Double Team" EvasionUp1Effect 0 Normal 100 15
    , move "Recover" HealEffect 0 Normal 100 20
    , move "Harden" DefenseUp1Effect 0 Normal 100 30
    , move "Minimize" EvasionUp1Effect 0 Normal 100 20
    , move "Smokescreen" AccuracyDown1Effect 0 Normal 100 20
    , move "Confuse Ray" ConfusionEffect 0 Ghost 100 10
    , move "Withdraw" DefenseUp1Effect 0 Water 100 40
    , move "Defense Curl" DefenseUp1Effect 0 Normal 100 40
    , move "Barrier" DefenseUp2Effect 0 Psychic 100 30
    , move "Light Screen" LightScreenEffect 0 Psychic 100 30
    , move "Haze" HazeEffect 0 Ice 100 30
    , move "Reflect" ReflectEffect 0 Psychic 100 20
    , move "Focus Energy" FocusEnergyEffect 0 Normal 100 30
    , move "Bide" BideEffect 0 Normal 100 10
    , move "Metronome" MetronomeEffect 0 Normal 100 10
    , move "Mirror Move" MirrorMoveEffect 0 Flying 100 20
    , move "Selfdestruct" ExplodeEffect 130 Normal 100 5
    , move "Egg Bomb" NoEffect 100 Normal 75 10
    , move "Lick" ParalyzeSideEffect2 20 Ghost 100 30
    , move "Smog" PoisonSideEffect2 20 Poison 70 20
    , move "Sludge" PoisonSideEffect2 65 Poison 100 20
    , move "Bone Club" FlinchSideEffect1 65 Ground 85 20
    , move "Fire Blast" BurnSideEffect2 120 Fire 85 5
    , move "Waterfall" NoEffect 80 Water 100 15
    , move "Clamp" TrappingEffect 35 Water 75 10
    , move "Swift" SwiftEffect 60 Normal 100 20
    , move "Skull Bash" ChargeEffect 100 Normal 100 15
    , move "Spike Cannon" TwoToFiveAttacksEffect 20 Normal 100 15
    , move "Constrict" SpeedDownSideEffect 10 Normal 100 35
    , move "Amnesia" SpecialUp2Effect 0 Psychic 100 20
    , move "Kinesis" AccuracyDown1Effect 0 Psychic 80 15
    , move "Softboiled" HealEffect 0 Normal 100 10
    , move "Hi Jump Kick" JumpKickEffect 85 Fighting 90 20
    , move "Glare" ParalyzeEffect 0 Normal 75 30
    , move "Dream Eater" DreamEaterEffect 100 Psychic 100 15
    , move "Poison Gas" PoisonEffect 0 Poison 55 40
    , move "Barrage" TwoToFiveAttacksEffect 15 Normal 85 20
    , move "Leech Life" DrainHPEffect 20 Bug 100 15
    , move "Lovely Kiss" SleepEffect 0 Normal 75 10
    , move "Sky Attack" ChargeEffect 140 Flying 90 5
    , move "Transform" TransformEffect 0 Normal 100 10
    , move "Bubble" SpeedDownSideEffect 20 Water 100 30
    , move "Dizzy Punch" NoEffect 70 Normal 100 10
    , move "Spore" SleepEffect 0 Grass 100 15
    , move "Flash" AccuracyDown1Effect 0 Normal 70 20
    , move "Psywave" SpecialDamageEffect 1 Psychic 80 15
    , move "Splash" SplashEffect 0 Normal 100 40
    , move "Acid Armor" DefenseUp2Effect 0 Poison 100 40
    , move "Crabhammer" NoEffect 90 Water 85 10
    , move "Explosion" ExplodeEffect 170 Normal 100 5
    , move "Fury Swipes" TwoToFiveAttacksEffect 10 Normal 80 15
    , move "Bonemerang" AttackTwiceEffect 50 Ground 90 10
    , move "Rest" HealEffect 0 Psychic 100 10
    , move "Rock Slide" NoEffect 75 Rock 90 10
    , move "Hyper Fang" FlinchSideEffect1 80 Normal 90 15
    , move "Sharpen" AttackUp1Effect 0 Normal 100 30
    , move "Conversion" ConversionEffect 0 Normal 100 30
    , move "Tri Attack" NoEffect 80 Normal 100 10
    , move "Super Fang" SuperFangEffect 1 Normal 90 10
    , move "Slash" NoEffect 70 Normal 100 20
    , move "Substitute" SubstituteEffect 0 Normal 100 10
    , move "Struggle" RecoilEffect 50 Normal 100 10
    ]
