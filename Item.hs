-------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------------------
module Item where
-------------------------------------------------------------------------------
import System.Random
import Enchantment
import Location
import Enchantment
-------------------------------------------------------------------------------
data BeginItem =
 BeginItem {
  beginLocation :: Location,
  beginEnchantments :: [Enchantment],
  beginDepth :: Int -- the depth at which the item was retrieved (affects rad reduction generation!)
 }
 deriving Show
-------------------------------------------------------------------------------
data Item =
 Item {
  location :: Location,
  enchantments :: [Enchantment],
  depth :: Int,
  depthRoll :: Double, -- (uniform 1.00~0.98)
  depthFactor :: Double, -- (uniform 1.00~0.98)^depth factor applied to radiation due to depth (THIS IS RAD REDUCTION!)
  rarity :: ItemRarityType,
  radiation :: Int
 }
-------------------------------------------------------------------------------
instance Show Item where
 show item =
  let lineLength = 84 in
  let title = ("A " ++ (show $ rarity $ item) ++ " " ++ (show $ location item)) in
  let linesRemaining = 84 - (length $ title) in
  let initialPadding = div linesRemaining 2 in
  let finalPadding = linesRemaining - initialPadding in
  let header = (replicate initialPadding '-') ++ title ++ (replicate finalPadding '-') in
  header ++ "\n" ++ (displayDepth item) ++ (displayEnchantments $ enchantments item) ++ "\n" ++ (displayRadiation item) ++ (replicate lineLength '-')
-------------------------------------------------------------------------------
instantiateItem :: (BeginItem, StdGen) -> (Item, StdGen)
instantiateItem (beginItem, r) =
 let (roll :: Double, r') = random r in
 let dRoll = (roll / 50.0) + 0.98 in
 let dFactor = dRoll ** (fromIntegral $ beginDepth beginItem) in
 let dRarity = if dRoll >= 0.9822 then Common else if dRoll >= 0.9802 then Uncommon else Rare in
 let dBaseRadiation = fromIntegral (beginItemCost beginItem) in
 let dRadiation = round (dBaseRadiation * dFactor) in
 (Item {
  location = beginLocation beginItem,
  enchantments = beginEnchantments beginItem,
  depth = beginDepth beginItem,
  depthRoll = dRoll,
  depthFactor = dFactor,
  rarity = dRarity,
  radiation = dRadiation
 }, r')
-------------------------------------------------------------------------------
itemCost :: Item -> Int
itemCost item = radiation item
-------------------------------------------------------------------------------
beginItemCost :: BeginItem -> Int
beginItemCost item = sum $ map enchantmentCost $ beginEnchantments item
-------------------------------------------------------------------------------
displayDepth :: Item -> String
displayDepth item = "You found it at level " ++ (show $ depth $ item) ++ " of the dungeon.\n"
-------------------------------------------------------------------------------
displayRadiation :: Item -> String
displayRadiation item =
 "Total Radiation: " ++ (show $ radiation item) ++ "\n" ++
 "Radiation Reduction: " ++ (take 5 $ show $ (100.0 * (1.0 - (depthFactor item)))) ++ "%\n" -- always rounds down, which is good enough
-------------------------------------------------------------------------------
