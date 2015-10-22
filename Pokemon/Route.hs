{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Pokemon.Route where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State

import Pokemon.Battle
import Pokemon.Moves
import Pokemon.Species
import Pokemon.Stats
import Pokemon.Type

data PartyPokemon =
    PartyPokemon
        { _pSpecies :: Species
        , _pExperience :: Integer
        , _pLevel :: Integer
        , _pDVs :: DVs
        , _pStatExp :: StatExp
        , _pStatExpAtLevel :: StatExp
        , _pStats :: Stats
        , _pMoves :: [Move]
        , _pCurHP :: Integer
        }
    deriving (Eq, Show, Ord)

makeLenses ''PartyPokemon

data RouteState =
    RouteState
        { _party :: [PartyPokemon]
        }
    deriving (Eq, Show, Ord)

makeLenses ''RouteState

newtype RouteT m a = RouteT { runRouteT :: StateT RouteState (ExceptT String m) a }

instance Functor m => Functor (RouteT m) where
    fmap f (RouteT a) = RouteT (fmap f a)

instance Monad m => Applicative (RouteT m) where
    pure = RouteT . pure
    RouteT f <*> RouteT x = RouteT (f <*> x)

instance Monad m => Monad (RouteT m) where
    return = RouteT . return
    RouteT x >>= f = RouteT (x >>= runRouteT . f)
    fail = throwError

instance Monad m => MonadError String (RouteT m) where
    throwError = RouteT . throwError
    catchError (RouteT x) f = RouteT (catchError x (runRouteT . f))

instance Monad m => MonadState RouteState (RouteT m) where
    get = RouteT get
    put = RouteT . put
    state = RouteT . state

class (MonadError String m, MonadState RouteState m) => MonadRoute m

instance Monad m => MonadRoute (RouteT m)

updateStats :: PartyPokemon -> PartyPokemon
updateStats poke =
    poke & pStats .~ computeStats (poke^.pSpecies) (poke^.pLevel) (poke^.pDVs) (poke^.pStatExp)
