module Main where

import Data.HashMap
import qualified Data.Tuple.Extra as DE
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Lib
import Protolude
import Control.Lens
import Data.Default

main :: IO ()
main = do
  let window = InWindow "Ant" (200, 200) (10, 10)
  simulateAnt window

intToFloat :: Int -> Float
intToFloat = fromInteger . toInteger

drawSquare :: Int -> (Int, Int) -> Color -> Picture
drawSquare size (x, y) col =
  let size' = intToFloat size
      (x', y') = intToFloat `DE.both` (x, y)
  in translate x' y' $ color col $ rectangleSolid size' size'

renderWorkspace :: Int -> WorkSpace -> Picture
renderWorkspace sqrsize workspace = pictures $ do
  (Vec2 x y, AntState c _) <- assocs workspace
  pure $ drawSquare sqrsize (x * sqrsize, y * sqrsize) c


defaultProgram :: ([AntState], AntState)
defaultProgram =
  let a = AntState black (rotateLeft  >> setState b >> stepAnt)
      b = AntState white (rotateRight >> setState a >> stepAnt)
  in ([a, b], a)

defaultAnt :: Ant
defaultAnt = def & defaultState .~ snd defaultProgram

renderAnt :: Ant -> Picture
renderAnt a = renderWorkspace 5 (a ^. workspace)

updateAnt :: ViewPort -> Float -> Ant -> Ant
updateAnt _ _ = execState stepAnt

simulateAnt :: Display -> IO ()
simulateAnt d =
  simulate
  d
  black
  10
  defaultAnt
  renderAnt
  updateAnt
