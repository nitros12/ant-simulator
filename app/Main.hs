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


defaultProgram :: AntState
defaultProgram =
  let a = AntState black (rotateLeft  >> setState b >> moveOne)
      b = AntState white (rotateRight >> setState a >> moveOne)
  in a

programSquare :: AntState
programSquare =
  let a = AntState white   (rotateRN 2  >> setState b >> stepTimes 3)
      b = AntState black   (rotateRight >> setState c >> stepTimes 2)
      c = AntState magenta (rotateLN 2  >> setState d >> stepTimes 3)
      d = AntState cyan    (rotateLeft  >> setState a >> moveOne)
  in a

defaultAnt :: Ant
defaultAnt = def & defaultState .~ programSquare

renderAnt :: Ant -> Picture
renderAnt a = renderWorkspace 5 (a ^. workspace)

updateAnt :: ViewPort -> Float -> Ant -> Ant
updateAnt _ _ = execState stepAnt

simulateAnt :: Display -> IO ()
simulateAnt d =
  simulate
  d
  (greyN 0.3)
  60
  defaultAnt
  renderAnt
  updateAnt


debugRun :: IO ()
debugRun = go defaultAnt
  where go ant = do
          let newant = execState stepAnt ant
          print newant
          go newant
