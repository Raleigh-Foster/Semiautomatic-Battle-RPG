-------------------------------------------------------------------------------
module Game where
-------------------------------------------------------------------------------
import System.Random
import Player
import Item
-------------------------------------------------------------------------------
data Game =
 Game {
--  worldLocation :: WorldLocation,
  rnd :: StdGen,
  player :: Player,
  mob :: Player,
  mobSlain :: Bool,
  currentCommand :: String,
  commandQueue :: [String],
  messageQueue :: [String],
  nearbyItems :: [Item]
 }
-------------------------------------------------------------------------------
