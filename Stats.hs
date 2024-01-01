-------------------------------------------------------------------------------
module Stats where
------------------------------------------------------------------------------
--データ型
data BaseStats = -- just base stats without items
 BaseStats {
  baseIntelligence :: Int,
  baseWisdom :: Int,
  baseConstitution :: Int,
  baseStrength :: Int,
  baseDexterity :: Int,
  baseWill :: Int
 } deriving Show
-------------------------------------------------------------------------------
--データ型
data ModifiedStats = -- base stats plus bonuses from items
 ModifiedStats {
  modifiedIntelligence :: Int,
  modifiedWisdom :: Int,
  modifiedConstitution :: Int,
  modifiedStrength :: Int,
  modifiedDexterity :: Int,
  modifiedWill :: Int,
  maxHp :: Int,
  maxMana :: Int,
  healthRegen :: Int,
  manaRegen :: Int,
  resist :: Int,
  damage :: Int,
  defense :: Int,
  attack :: Int
 } deriving Show
-------------------------------------------------------------------------------
--データ型
data BuffedStats = -- modified stats, plus bonuses due to powers and effects
 BuffedStats {
  buffedIntelligence :: Int,
  buffedWisdom :: Int,
  buffedConstitution :: Int,
  buffedStrength :: Int,
  buffedDexterity :: Int,
  buffedWill :: Int,
  buffedMaxHp :: Int,
  buffedMaxMana :: Int,
  buffedHealthRegen :: Int,
  buffedManaRegen :: Int,
  buffedResist :: Int,
  buffedDamage :: Int,
  buffedDefense :: Int,
  buffedAttack :: Int
 }
 deriving Show
-------------------------------------------------------------------------------
--データ型
data FinalStats = -- buffed stats plus primary effects on secondary stats
 FinalStats {
  finalMaxHp :: Int,
  finalMaxMana :: Int,
  finalHealthRegen :: Int,
  finalManaRegen :: Int,
  finalResist :: Int,
  finalDamage :: Int,
  finalDefense :: Int,
  finalAttack :: Int
 }
 deriving Show
-------------------------------------------------------------------------------
