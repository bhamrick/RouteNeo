module Pokemon.Type where

import Data.Ratio

data Type
    = Normal
    | Fighting
    | Flying
    | Poison
    | Ground
    | Rock
    | Bug
    | Ghost
    | Fire
    | Water
    | Grass
    | Electric
    | Psychic
    | Ice
    | Dragon
    deriving (Eq, Show, Ord)

data TypeEffectiveness
    = Immune
    | Effective Int
    deriving (Eq, Show, Ord)

super :: TypeEffectiveness
super = Effective 1

neutral :: TypeEffectiveness
neutral = Effective 0

notVery :: TypeEffectiveness
notVery = Effective (-1)

immune :: TypeEffectiveness
immune = Immune

attackRatio :: TypeEffectiveness -> Ratio Integer
attackRatio effectiveness =
    case effectiveness of
        Immune -> 0
        Effective n ->
            if n > 0
            then 2^n
            else 1 % 2^(-n)

instance Monoid TypeEffectiveness where
    mempty = Effective 0
    Immune `mappend` _ = Immune
    _ `mappend` Immune = Immune
    Effective n `mappend` Effective m = Effective (n + m)

effectiveness :: Type -> Type -> TypeEffectiveness
effectiveness attack defender =
    case (attack, defender) of
        (Water, Fire) -> super
        (Fire, Grass) -> super
        (Fire, Ice) -> super
        (Grass, Water) -> super
        (Electric, Water) -> super
        (Water, Rock) -> super
        (Ground, Flying) -> immune
        (Water, Water) -> notVery
        (Fire, Fire) -> notVery
        (Electric, Electric) -> notVery
        (Ice, Ice) -> notVery
        (Grass, Grass) -> notVery
        (Psychic, Psychic) -> notVery
        (Fire, Water) -> notVery
        (Grass, Fire) -> notVery
        (Water, Grass) -> notVery
        (Normal, Rock) -> notVery
        (Normal, Ghost) -> immune
        (Ghost, Ghost) -> super
        (Fire, Bug) -> super
        (Fire, Rock) -> notVery
        (Water, Ground) -> super
        (Electric, Ground) -> immune
        (Electric, Flying) -> super
        (Grass, Ground) -> super
        (Grass, Bug) -> notVery
        (Grass, Poison) -> notVery
        (Grass, Rock) -> super
        (Grass, Flying) -> notVery
        (Ice, Water) -> notVery
        (Ice, Grass) -> super
        (Ice, Ground) -> super
        (Ice, Flying) -> super
        (Fighting, Normal) -> super
        (Fighting, Poison) -> notVery
        (Fighting, Flying) -> notVery
        (Fighting, Psychic) -> notVery
        (Fighting, Bug) -> notVery
        (Fighting, Rock) -> super
        (Fighting, Ice) -> super
        (Fighting, Ghost) -> immune
        (Poison, Grass) -> super
        (Poison, Poison) -> notVery
        (Poison, Ground) -> notVery
        (Poison, Bug) -> super
        (Poison, Rock) -> notVery
        (Poison, Ghost) -> notVery
        (Ground, Fire) -> super
        (Ground, Electric) -> super
        (Ground, Grass) -> notVery
        (Ground, Bug) -> notVery
        (Ground, Rock) -> super
        (Ground, Poison) -> super
        (Flying, Electric) -> notVery
        (Flying, Fighting) -> super
        (Flying, Bug) -> super
        (Flying, Grass) -> super
        (Flying, Rock) -> notVery
        (Psychic, Fighting) -> super
        (Psychic, Poison) -> super
        (Bug, Fire) -> notVery
        (Bug, Grass) -> super
        (Bug, Fighting) -> notVery
        (Bug, Flying) -> notVery
        (Bug, Psychic) -> super
        (Bug, Ghost) -> notVery
        (Bug, Poison) -> super
        (Rock, Fire) -> super
        (Rock, Fighting) -> notVery
        (Rock, Ground) -> notVery
        (Rock, Flying) -> super
        (Rock, Bug) -> super
        (Rock, Ice) -> super
        (Ghost, Normal) -> immune
        (Ghost, Psychic) -> notVery
        (Fire, Dragon) -> notVery
        (Water, Dragon) -> notVery
        (Electric, Dragon) -> notVery
        (Grass, Dragon) -> notVery
        (Ice, Dragon) -> super
        (Dragon, Dragon) -> super
        _ -> neutral
