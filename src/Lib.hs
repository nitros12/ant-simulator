{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Lens
import Control.Monad.State.Lazy
import Data.Default
import Data.HashMap
import Data.Hashable
import Data.Maybe
import qualified Graphics.Gloss.Data.Color as C
import qualified Graphics.Gloss.Rendering as R
import Protolude hiding (Map)
import qualified Prelude

data AntState = AntState
  { getColour :: R.Color
  , getUpdater :: State Ant ()
  }

instance Prelude.Show AntState where
  show antstate = "AntState " <> show (getColour antstate)

data Vec2 =
  Vec2 {-# UNPACK #-}!Int
       {-# UNPACK #-}!Int
  deriving (Show, Eq, Generic, Ord)

instance Hashable Vec2

instance Num Vec2 where
  (+) (Vec2 a b) (Vec2 a' b') = Vec2 (a + a') (b + b')
  negate (Vec2 a b) = Vec2 (-a) (-b)
  (*) (Vec2 a b) (Vec2 a' b') = Vec2 (a * a') (b * b')
  abs (Vec2 a b) = Vec2 (abs a) (abs b)

scaleVec2 :: Int -> Vec2 -> Vec2
scaleVec2 n (Vec2 a b) = Vec2 (a * n) (b * n)

data Direction
  = DirUp
  | DirRight
  | DirDown
  | DirLeft
  deriving (Enum, Ord, Eq, Show, Bounded)

nextEnum :: forall a. (Bounded a, Enum a) => a -> a
nextEnum i = toEnum index
  where index = (fromEnum i + 1) `mod` range
        range = 1 + upper
        upper = fromEnum (maxBound :: a)

prevEnum :: forall a. (Bounded a, Enum a) => a -> a
prevEnum i = toEnum index
  where index = (fromEnum i - 1) `mod` range
        range = 1 + upper
        upper = fromEnum (maxBound :: a)

type WorkSpace = Map Vec2 AntState

data Ant = Ant
  { _workspace :: WorkSpace
  , _direction :: Direction
  , _position :: Vec2
  , _defaultState :: AntState
  }

makeLenses ''Ant

instance Prelude.Show Ant where
  show ant = "Ant " <> show (ant ^. workspace) <> show (ant ^. direction) <> show (ant ^. position)

instance Default Ant where
  def = Ant Data.HashMap.empty DirUp (Vec2 0 0) (AntState C.black moveOne)

setState :: AntState -> State Ant ()
setState newstate = do
  ant <- get
  let newworkspace = insert (ant ^. position) newstate (ant ^. workspace)
  put $ ant & workspace .~ newworkspace

movementVector :: Direction -> Vec2
movementVector DirUp = Vec2 0 1
movementVector DirDown = Vec2 0 (-1)
movementVector DirLeft = Vec2 (-1) 0
movementVector DirRight = Vec2 1 0

stepTimes :: Int -> State Ant ()
stepTimes n = do
  ant <- get
  let movement = scaleVec2 n $ movementVector (ant ^. direction)
  put (ant & position +~ movement)

moveOne :: State Ant ()
moveOne = stepTimes 1

rotateRight :: State Ant ()
rotateRight = direction %= nextEnum

rotateLeft :: State Ant ()
rotateLeft = direction %= prevEnum

rotateRN :: Int -> State Ant ()
rotateRN n = replicateM_ n rotateRight

rotateLN :: Int -> State Ant ()
rotateLN n = replicateM_ n rotateLeft

stepAnt :: State Ant ()
stepAnt = do
  ant <- get
  let state = fromMaybe (ant ^. defaultState) $ lookup (ant ^. position) (ant ^. workspace)
  getUpdater state
