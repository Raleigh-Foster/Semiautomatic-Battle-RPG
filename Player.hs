{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------------------
module Player where
-------------------------------------------------------------------------------
import System.Random
import Debug.Trace
import Classes
import Stats
import Enchantment
import Item
import Equipment
-------------------------------------------------------------------------------
--関数
emptyBuffedStats :: ModifiedStats -> BuffedStats
emptyBuffedStats modifiedStats =
 BuffedStats {
  buffedIntelligence = modifiedIntelligence modifiedStats,
  buffedWisdom = modifiedWisdom modifiedStats,
  buffedConstitution = modifiedConstitution modifiedStats,
  buffedStrength = modifiedStrength modifiedStats,
  buffedDexterity = modifiedDexterity modifiedStats,
  buffedWill = modifiedWill modifiedStats,
  buffedMaxHp = maxHp modifiedStats,
  buffedMaxMana = maxMana modifiedStats,
  buffedHealthRegen = healthRegen modifiedStats,
  buffedManaRegen = manaRegen modifiedStats,
  buffedResist = resist modifiedStats,
  buffedDamage = damage modifiedStats,
  buffedDefense = defense modifiedStats,
  buffedAttack = attack modifiedStats
 }
-------------------------------------------------------------------------------
--関数
getBuffedStats :: ModifiedStats -> BuffedStats
getBuffedStats modifiedStats =
 BuffedStats {
  buffedIntelligence = buffedIntelligence,
  buffedWisdom = buffedWisdom,
  buffedConstitution = buffedConstitution,
  buffedStrength = buffedStrength,
  buffedDexterity = buffedDexterity,
  buffedWill = buffedWill,
  buffedMaxHp = buffedMaxHp,
  buffedMaxMana = buffedMaxMana,
  buffedHealthRegen = buffedHealthRegen,
  buffedManaRegen = buffedManaRegen,
  buffedResist = buffedResist,
  buffedDamage = buffedDamage,
  buffedDefense = buffedDefense,
  buffedAttack = buffedAttack
 }
 where
  buffedIntelligence = undefined
  buffedWisdom = undefined
  buffedConstitution = undefined
  buffedStrength = undefined
  buffedDexterity = undefined
  buffedWill = undefined
  buffedMaxHp = undefined
  buffedMaxMana = undefined
  buffedHealthRegen = undefined
  buffedManaRegen = undefined
  buffedResist = undefined
  buffedDamage = undefined
  buffedDefense = undefined
  buffedAttack = undefined
-------------------------------------------------------------------------------
getFinalStats :: BuffedStats -> FinalStats
getFinalStats stats = FinalStats {
  finalMaxHp = (buffedMaxHp stats) + (10 * (buffedConstitution $ stats)),
  finalMaxMana = (buffedMaxMana stats) + (5 * (buffedWisdom $ stats)),
  finalHealthRegen = (buffedHealthRegen stats) + (10 * (buffedWill stats)),
  finalManaRegen = (buffedManaRegen stats) + (5 * (buffedWill stats)),
  finalResist = buffedResist stats,
  finalDamage = (buffedDamage stats) + (buffedStrength stats),
  finalDefense = (buffedDefense stats) + (buffedDexterity stats),
  finalAttack = (buffedAttack stats) + (buffedDexterity stats)
 }
-------------------------------------------------------------------------------
{-

radiation reduction caused by depth ranges from 0 to 2% per level
(radiation = base_radiation * (uniform 1.00~0.98)^depth )

As a point of comparison, mobs get about 2% harder each depth (although this is spread across various stats!)

This means that perfect rolls will correspond to items of the same radiation growing in value at a rate matching the level difficulty growth;
meanwhile characters can also get stronger through leveling, which gives stats and powers.
This means that characters will grow faster than the depth assuming perfect drops.
That said, the chance of perfect drops trends towards 0 the deeper you go!

Assuming we always get average/median (same because distribution is uniform) drops,
characters will hit a ceiling where they cannot progress without ever higher and higher levels
to compensate for the difficulty of the depth not being compensated by corresponding drops.

Leveling does not allow for exponential progress (instead is eventually polynomial), so this will eventually hit diminishing returns!

-}

{-

ItemRarityType indicates which part of the depth roll was hit. (It is a quality of life feature!)

A base of 0.9802 and lower is rare (1% chance)
A base above rare but lower than 0.9822 is uncommon (10% chance)
A base above uncommon is common (89% chance)

-}
-------------------------------------------------------------------------------
data Player =
 Player {
  baseStats :: BaseStats,
  modifiedStats :: ModifiedStats,
  buffedStats :: BuffedStats,
  finalStats :: FinalStats,
  damageTaken :: Int,
  manaUsed :: Int,
  equipment :: Equipment,
  inventory :: [Item],
  level :: Int,
  xp :: Int,
  charClass :: Class,
  stale :: Bool -- used for graphics
 }
 deriving Show
-------------------------------------------------------------------------------
itemItems :: Maybe Item -> [Item]
itemItems Nothing = []
itemItems (Just item) = [item]
-------------------------------------------------------------------------------
getEnchantments :: Equipment -> [Enchantment]
getEnchantments eq =
 let l = [helmet eq, torso eq, pants eq, rightRing eq, leftRing eq, belt eq, scarf eq, gloves eq, socks eq, boots eq, shirt eq] in
 let itemList = concat $ map itemItems $ l in
 concat $ map enchantments itemList
-------------------------------------------------------------------------------
-- maybe not the best algorithm....? (actually should I be caching this until it is updated?)

-- currently only counts base stat bonuses!!!!

captureEnchantments' :: [Enchantment] -> ModifiedStats -> ModifiedStats
captureEnchantments' [] modifiedStats = modifiedStats
captureEnchantments' (x:xs) modifiedStats =
 let MkEnchantment enchantmentType val = x in
 captureEnchantments' xs $
 case enchantmentType of
  IntelligenceBonus -> modifiedStats {modifiedIntelligence = val + (modifiedIntelligence modifiedStats)}
  WisdomBonus -> modifiedStats {modifiedWisdom = val + (modifiedWisdom modifiedStats)}
  ConstitutionBonus -> modifiedStats {modifiedConstitution = val + (modifiedConstitution modifiedStats)}
  StrengthBonus -> modifiedStats {modifiedStrength = val + (modifiedStrength modifiedStats)}
  DexterityBonus -> modifiedStats {modifiedDexterity = val + (modifiedDexterity modifiedStats)}
  WillBonus -> modifiedStats {modifiedWill = val + (modifiedWill modifiedStats)}
  MaxHpBonus -> modifiedStats {maxHp = val + (maxHp modifiedStats)}
  MaxManaBonus -> modifiedStats {maxMana = val + (maxMana modifiedStats)}
  HealthRegenBonus -> modifiedStats {healthRegen = val + (healthRegen modifiedStats)}
  ManaRegenBonus -> modifiedStats {manaRegen = val + (manaRegen modifiedStats)}
  ResistBonus -> modifiedStats {resist = val + (resist modifiedStats)}
  DamageBonus -> modifiedStats {damage = val + (damage modifiedStats)}
  DefenseBonus -> modifiedStats {defense = val + (defense modifiedStats)}
  AttackBonus -> modifiedStats {attack = val + (attack modifiedStats)}
  -- Treating weapon bonuses as affecting character, at least for now.
  WeaponDamageBonus -> modifiedStats {damage = val + (damage modifiedStats)}
  WeaponAttackBonus -> modifiedStats {attack = val + (attack modifiedStats)}
  WeaponDefenseBonus -> modifiedStats {defense = val + (defense modifiedStats)}


-- compute secondary bonuses afterwards...
captureEnchantments :: [Enchantment] -> BaseStats -> ModifiedStats
captureEnchantments enchantments baseStats =
 captureEnchantments' enchantments $
 ModifiedStats {
  modifiedIntelligence = baseIntelligence baseStats,
  modifiedWisdom = baseWisdom baseStats,
  modifiedConstitution = baseConstitution baseStats,
  modifiedStrength = baseStrength baseStats,
  modifiedDexterity = baseDexterity baseStats,
  modifiedWill = baseWill baseStats,
  maxHp = 0,
  maxMana = 0,
  healthRegen = 0,
  manaRegen = 0,
  resist = 0,
  damage = 0,
  defense = 0,
  attack = 0
 }


getModifiedStats :: Equipment -> BaseStats -> ModifiedStats
getModifiedStats eq stats = captureEnchantments (getEnchantments eq) stats -- captureEnchantments (getEnchantments $ equipment p) (baseStats p)
-------------------------------------------------------------------------------
getPlayer :: BaseStats -> Player
getPlayer baseStats =
 let modifiedStats = captureEnchantments [] baseStats in
 let buffedStats = emptyBuffedStats modifiedStats in
 let finalStats = getFinalStats buffedStats in
 Player {
  charClass = Avatar,
  baseStats = baseStats,
  modifiedStats = modifiedStats,
  buffedStats = buffedStats,
  finalStats = finalStats,
  damageTaken = 0,
  manaUsed = 0,
  equipment = noEquipment,
  inventory = [],
  level = 1,
  xp = 0,
  stale = True
 }
-------------------------------------------------------------------------------
--refreshPlayer :: Player -> Player
--refreshPlayer p = p {modifiedStats = getModifiedStats p}
--
refreshPlayer :: Player -> Player
refreshPlayer p =
 let m = getModifiedStats (equipment p) (baseStats p) in
 let b = emptyBuffedStats m in
 let f = getFinalStats b in
 p {modifiedStats = m, buffedStats = b, finalStats = f, stale = True}

-------------------------------------------------------------------------------
showPlayer :: Player -> IO ()
showPlayer player = do
 putStrLn $ show $ baseStats $ player
 putStrLn $ show $ modifiedStats $ player
 putStr "maxHp: "
 putStrLn $ show $ maxHp $ modifiedStats $ player
 putStr "maxMana: "
 putStrLn $ show $ maxMana $ modifiedStats $ player
 putStr "hp regen: "
 putStrLn $ show $ healthRegen $ modifiedStats $ player
 putStr "mana regen: "
 putStrLn $ show $ manaRegen $ modifiedStats $ player
 putStr "resist: "
 putStrLn $ show $ resist $ modifiedStats $ player
 putStr "damage: "
 putStrLn $ show $ damage $ modifiedStats $ player
 putStr "defense: "
 putStrLn $ show $ defense $ modifiedStats $ player
 putStr "attack: "
 putStrLn $ show $ attack $ modifiedStats $ player
-------------------------------------------------------------------------------
initialPlayer :: Player
initialPlayer =
 getPlayer $
 BaseStats {
  baseIntelligence = 5,
  baseWisdom = 5,
  baseConstitution = 5,
  baseStrength = 5,
  baseDexterity = 5,
  baseWill = 5
 }
-------------------------------------------------------------------------------
testMob :: Player
testMob =
 getPlayer $
 BaseStats {
  baseIntelligence = 3,
  baseWisdom = 3,
  baseConstitution = 5,
  baseStrength = 3,
  baseDexterity = 3,
  baseWill = 2
 }
-------------------------------------------------------------------------------
computeAccuracyAttack :: Player -> Player -> Double
computeAccuracyAttack attacker defender =
 let a = fromIntegral $ finalAttack $ finalStats $ attacker in -- getAttack $ baseStats $ attacker in
 let d = fromIntegral $ finalDefense $ finalStats $ defender in -- getDefense $ baseStats $ defender in
 if a+d == 0 then 0.5 else
 a / (a + d)
-------------------------------------------------------------------------------
computeDamage :: Player -> Player -> Double
computeDamage attacker defender =
 let d = fromIntegral $ finalDamage $ finalStats $ attacker in
 let r = fromIntegral $ finalResist $ finalStats $ defender in
 if d+r == 0 then 0.5 else
 (d / (d + r)) * d
-------------------------------------------------------------------------------

--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
