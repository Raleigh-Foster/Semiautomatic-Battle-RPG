-------------------------------------------------------------------------------
module Equipment where
-------------------------------------------------------------------------------
import Item
-------------------------------------------------------------------------------
data Equipment =
 Equipment {
  helmet :: Maybe Item,
  torso :: Maybe Item,
  pants :: Maybe Item,
  rightRing :: Maybe Item,
  leftRing :: Maybe Item,
  belt :: Maybe Item,
  scarf :: Maybe Item,
  gloves :: Maybe Item,
  socks :: Maybe Item,
  boots :: Maybe Item,
  shirt :: Maybe Item
 } deriving Show
-------------------------------------------------------------------------------
noEquipment :: Equipment
noEquipment =
 Equipment {
  helmet = Nothing,
  torso = Nothing,
  pants = Nothing,
  rightRing = Nothing,
  leftRing = Nothing,
  belt = Nothing,
  scarf = Nothing,
  gloves = Nothing,
  socks = Nothing,
  boots = Nothing,
  shirt = Nothing
 }
-------------------------------------------------------------------------------
