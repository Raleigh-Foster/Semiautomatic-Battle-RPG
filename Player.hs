{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------------------
module Player where
-------------------------------------------------------------------------------
import System.Random
import Debug.Trace
-------------------------------------------------------------------------------
data HelpFile =
 HelpFile {
  helpName :: String,
  helpText :: String
 }
-------------------------------------------------------------------------------
serializeHelp :: HelpFile -> String -- assume help name is shorter than 1 line
serializeHelp helpFile =
 let lineLength = 84 in
 let lineRemaining = 84 - (length $ helpName helpFile) in
 let initialPadding = div lineRemaining 2 in
 let finalPadding = lineRemaining - initialPadding in
 let title = (replicate initialPadding '-') ++ (helpName helpFile) ++ (replicate finalPadding '-') in
 title ++ "\n" ++ (helpText helpFile) ++ "\n" ++ (replicate lineLength '-')
--
--
data Classes -- perhaps more later!!
 = Lich
 | Werewolf
 | Vampire
-------------------------------------------------------------------------------
data LichPowers
 = LegendaryBlademaster -- bonus crit damage
 | EtherealBlade -- summon a 0 rad blade that gains power as you get kills
 | Magician -- bonus mana regeneration and spell power
 | Archmage -- gain access to a variety of spells
 | MartialArtist -- Gain additional attack, damage, and defense
 | ManaShield -- Divert some of your maximum mana to provide additional defense and protection
-------------------------------------------------------------------------------
legendaryBlademasterHelpFile :: HelpFile
legendaryBlademasterHelpFile =
 HelpFile {
  helpName = "Legendary Blademaster",
  helpText = "Instead of the usual 200% damage, your critical strikes do (200+X)% damage,\nwhere X is your level in this power."
 }
-------------------------------------------------------------------------------
legendaryBlademasterHelp :: String
legendaryBlademasterHelp = serializeHelp legendaryBlademasterHelpFile
-------------------------------------------------------------------------------
etherealBladeHelpFile :: HelpFile
etherealBladeHelpFile =
 HelpFile {
  helpName = "Ethereal Blade",
  helpText =
   "You gain access to the commands \ESC[32m\'summon ethereal\'\ESC[0m and \ESC[32m\'remove ethereal\'\ESC[0m.\n" ++
   "Any kill made while wielding your ethereal blade will give it experience as well.\n" ++
   "Your ethereal blade has damage equal to log_1.2(experience),\n"++
   "and attack equal to 2 * log_1.2(experience).\n" ++
   "Note that while you will lose experience on death,\n" ++
   "your ethereal blade is immune to such loss,\n"++
   "as it belongs to a separate plane of existence."
 }
-------------------------------------------------------------------------------
etherealBladeHelp :: String
etherealBladeHelp = serializeHelp etherealBladeHelpFile
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- critical strikes should also bypass resistance! (huge help against very high resist enemies; otherwise on average another 2x approximately)
-------------------------------------------------------------------------------
data WerewolfPowers
 = NaturalFortitude -- You have additional resistance
 | SupernaturalHealing -- You have additional regeneration
 | ClawsOfAttack -- You have a powerful unarmed attack
 | WolfForm -- Gain massive resistances and damage; prevents use of most item slots (separate inventory).
 | BearSpirit -- Gain bonuses at low health; prevents use of other spirit powers
 | FoxSpirit -- Gain additional attack and defense; prevents use of other spirit powers
 | SharkSpirit -- Your attacks heal you; prevents use of other spirit powers
 | PenguinSpirit -- Gain massive resistances; prevents use of other spirit powers
-------------------------------------------------------------------------------
data VampirePowers
 = BloodShield -- Your blood power fuels a massive shield
 | ChiropteranHearing -- You have additional defense
 | NightBlade -- Summon a 0 rad blade fueled by your blood power
 | BloodFanatic -- You gain blood power from your kills
 | Infusion -- Divert some of your maximum mana to your damage
 | LivingForm -- Divert some of your mana regen to health regen, prevents training of undead form
 | UndeadForm -- Divert some of your health regen to mana regen, prevents training of living form
-------------------------------------------------------------------------------

data BaseStats =
 BaseStats {
  intelligence :: Int,
  wisdom :: Int,
  constitution :: Int,
  strength :: Int,
  dexterity :: Int,
  will :: Int
 } deriving Show
-------------------------------------------------------------------------------
data Location
 = Helmet
 | Torso
 | Pants
 | RightRing
 | LeftRing
 | Belt
 | Scarf
 | Gloves
 | Socks
 | Boots
 | Shirt
 deriving Show
-------------------------------------------------------------------------------
data EnchantmentType
 = IntelligenceBonus
 | WisdomBonus
 | ConstitutionBonus
 | StrengthBonus
 | DexterityBonus
 | WillBonus
 | MaxHpBonus
 | MaxManaBonus
 | HealthRegenBonus
 | ManaRegenBonus
 | ResistBonus
 | DamageBonus
 | DefenseBonus
 | AttackBonus
 | ProtectionBonus
 | AscendancyBonus
 | WeaponDamageBonus
 | WeaponAttackBonus
 | WeaponDefenseBonus
 deriving Show
-------------------------------------------------------------------------------
{-
   Weapon damage attack, and defense are bonuses only available on weapons.
   They provide a cheaper way of getting damage, which also provides a balance
   against powers that get free weapon damage via unarmed combat (claws, etc)
   or summonable magical weapons (ethereal blades, etc)


   If you use a real weapon, its weapon specific bonuses super important!!
-}
-------------------------------------------------------------------------------
data Enchantment = MkEnchantment EnchantmentType Int deriving Show

-- get base radiation cost for each type of bonus
enchantmentTypeCost :: EnchantmentType -> Int
enchantmentTypeCost IntelligenceBonus = 40
enchantmentTypeCost WisdomBonus = 40
enchantmentTypeCost ConstitutionBonus = 40
enchantmentTypeCost StrengthBonus = 40
enchantmentTypeCost DexterityBonus = 40
enchantmentTypeCost WillBonus = 40
enchantmentTypeCost MaxHpBonus = 1
enchantmentTypeCost MaxManaBonus = 2
enchantmentTypeCost HealthRegenBonus = 1
enchantmentTypeCost ManaRegenBonus = 2
enchantmentTypeCost ResistBonus = 8
enchantmentTypeCost DamageBonus = 10
enchantmentTypeCost DefenseBonus = 5
enchantmentTypeCost AttackBonus = 5
enchantmentTypeCost AscendancyBonus = 4
enchantmentTypeCost ProtectionBonus = 4
enchantmentTypeCost WeaponDamageBonus = 6
enchantmentTypeCost WeaponAttackBonus = 3
enchantmentTypeCost WeaponDefenseBonus = 3

enchantmentCost :: Enchantment -> Int
enchantmentCost (MkEnchantment enchantmentType amount) =
 (enchantmentTypeCost enchantmentType) * amount
-------------------------------------------------------------------------------
data ItemRarityType = Common | Uncommon | Rare deriving Show


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
displayEnchantment :: Enchantment -> String
displayEnchantment (MkEnchantment enchantmentType bonus) = (show enchantmentType) ++ "+" ++ (show bonus)

displayEnchantments :: [Enchantment] -> String
displayEnchantments [] = ""
displayEnchantments (x:xs) = (displayEnchantment x) ++ "\n" ++ (displayEnchantments xs)

displayDepth :: Item -> String
displayDepth item = "You found it at level " ++ (show $ depth $ item) ++ " of the dungeon.\n"

displayRadiation :: Item -> String
displayRadiation item =
 "Total Radiation: " ++ (show $ radiation item) ++ "\n" ++
 "Radiation Reduction: " ++ (take 5 $ show $ (100.0 * (1.0 - (depthFactor item)))) ++ "%\n" -- always rounds down, which is good enough

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
{-
serializeHelp :: HelpFile -> String -- assume help name is shorter than 1 line
serializeHelp helpFile =
 let lineLength = 84 in
 let lineRemaining = 84 - (length $ helpName helpFile) in
 let initialPadding = div lineRemaining 2 in
 let finalPadding = lineRemaining - initialPadding in
 let title = (replicate initialPadding '-') ++ (helpName helpFile) ++ (replicate finalPadding '-') in
 title ++ "\n" ++ (helpText helpFile) ++ "\n" ++ (replicate lineLength '-')
-}
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
data Player =
 Player {
  baseStats :: BaseStats,
  damageTaken :: Int,
  manaUsed :: Int,
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
  shirt :: Maybe Item,

  inventory :: [Item],

  level :: Int,
  xp :: Int,

-- computed values
  maxHp :: Int,
  maxMana :: Int,
  healthRegen :: Int,
  manaRegen :: Int,
  resist :: Int,
  damage :: Int,
  defense :: Int,
  attack :: Int,
  protection :: Int,
  ascendancy :: Int
 }
 deriving Show
-------------------------------------------------------------------------------
itemItems :: Maybe Item -> [Item]
itemItems Nothing = []
itemItems (Just item) = [item]
-------------------------------------------------------------------------------
getEnchantments :: Player -> [Enchantment]
getEnchantments p =
 let itemList = concat $ map itemItems [helmet p, torso p, pants p, rightRing p, leftRing p, belt p, scarf p, gloves p, socks p, boots p, shirt p] in
 concat $ map enchantments itemList
-------------------------------------------------------------------------------
-- maybe not the best algorithm....? (actually should I be caching this until it is updated?)

-- currently only counts base stat bonuses!!!!

captureEnchantments :: [Enchantment] -> BaseStats -> BaseStats
captureEnchantments [] baseStats = trace ("base stats!!:" ++ (show baseStats)) $ baseStats
captureEnchantments (x:xs) baseStats =
 let MkEnchantment enchantmentType val = x in
 captureEnchantments xs $
  case enchantmentType of
   WisdomBonus -> baseStats {wisdom = val + (wisdom baseStats)}
   ConstitutionBonus -> baseStats {constitution = val + (constitution baseStats)}
   StrengthBonus -> baseStats {strength = val + (strength baseStats)}
   DexterityBonus -> baseStats {dexterity = val + (dexterity baseStats)}
   WillBonus -> baseStats {will = val + (will baseStats)}
   _ -> baseStats -- IGNORE ALL OTHER BUFFS FOR NOW!


modifiedStats :: Player -> BaseStats
modifiedStats p = captureEnchantments (getEnchantments p) (baseStats p)

{- = IntelligenceBonus
 | WisdomBonus
 | ConstitutionBonus
 | StrengthBonus
 | DexterityBonus
 | WillBonus
 | MaxHpBonus
 | MaxManaBonus
 | HealthRegenBonus
 | ManaRegenBonus
 | ResistBonus
 | DamageBonus
 | DefenseBonus
 | AttackBonus
 | ProtectionBonus
 | AscendancyBonus
 | WeaponDamageBonus
 | WeaponAttackBonus
 | WeaponDefenseBonus
-}
-------------------------------------------------------------------------------
getPlayer' :: BaseStats -> Player
getPlayer' baseStats =
 Player {
  baseStats = baseStats,
  damageTaken = 0,
  manaUsed = 0,
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
  shirt = Nothing,
  inventory = [],
  maxHp = getMaxHp baseStats,
  maxMana = getMaxMana baseStats,
  healthRegen = getHealthRegen baseStats,
  manaRegen = getManaRegen baseStats,
  resist = getResist baseStats,
  damage = getDamage baseStats,
  defense = getDefense baseStats,
  attack = getAttack baseStats,
  protection = getProtection baseStats,
  ascendancy = getAscendancy baseStats,
  level = 1,
  xp = 0
 }

getPlayer :: BaseStats -> Player
getPlayer baseStats =
 let p = getPlayer' baseStats in
 let s = modifiedStats p in
 p {
  maxHp = getMaxHp s,
  maxMana = getMaxMana s,
  healthRegen = getHealthRegen s,
  manaRegen = getManaRegen s,
  resist = getResist s,
  damage = getDamage s,
  defense = getDefense s,
  attack = getAttack s,
  protection = getProtection s,
  ascendancy = getAscendancy s
 }

refreshPlayer :: Player -> Player
refreshPlayer p =
 let s = modifiedStats p in
 let rv = p {
  maxHp = getMaxHp s,
  maxMana = getMaxMana s,
  healthRegen = getHealthRegen s,
  manaRegen = getManaRegen s,
  resist = getResist s,
  damage = getDamage s,
  defense = getDefense s,
  attack = getAttack s,
  protection = getProtection s,
  ascendancy = getAscendancy s
 } in trace (show rv) $ rv


showPlayer :: Player -> IO ()
showPlayer player = do
 putStrLn $ show $ baseStats $ player
 putStr "maxHp: "
 putStrLn $ show $ maxHp $ player
 putStr "maxMana: "
 putStrLn $ show $ maxMana $ player
 putStr "hp regen: "
 putStrLn $ show $ healthRegen $ player
 putStr "mana regen: "
 putStrLn $ show $ manaRegen $ player
 putStr "resist: "
 putStrLn $ show $ resist $ player
 putStr "damage: "
 putStrLn $ show $ damage $ player
 putStr "defense: "
 putStrLn $ show $ defense $ player
 putStr "attack: "
 putStrLn $ show $ attack $ player
 putStr "protection: "
 putStrLn $ show $ protection $ player
 putStr "ascendancy: "
 putStrLn $ show $ ascendancy $ player
 
-------------------------------------------------------------------------------
getMaxHp :: BaseStats -> Int
getMaxHp baseStats =
 (15 * (constitution $ baseStats)) +
 (4 * (strength $ baseStats))
-------------------------------------------------------------------------------
getMaxMana :: BaseStats -> Int
getMaxMana baseStats =
 (1 * (intelligence $ baseStats)) +
 (3 * (wisdom $ baseStats))
-------------------------------------------------------------------------------
-- per minute
getHealthRegen :: BaseStats -> Int
getHealthRegen baseStats =
 (1 * (strength $ baseStats)) +
 (3 * (constitution $ baseStats)) +
 (5 * (will $ baseStats))
-------------------------------------------------------------------------------
-- per minute
getManaRegen :: BaseStats -> Int
getManaRegen baseStats =
 (1 * (intelligence $ baseStats)) +
 (3 * (wisdom $ baseStats)) +
 (5 * (will $ baseStats))
-------------------------------------------------------------------------------
getResist :: BaseStats -> Int
getResist baseStats =
 (2 * (constitution $ baseStats)) +
 (2 * (will $ baseStats))
-------------------------------------------------------------------------------
getDamage :: BaseStats -> Int
getDamage baseStats = (3 * (strength $ baseStats))
-------------------------------------------------------------------------------
getPower :: BaseStats -> Int
getPower baseStats = (3 * (intelligence $ baseStats))
-------------------------------------------------------------------------------
getAttack :: BaseStats -> Int
getAttack baseStats =
 (3 * (dexterity $ baseStats)) +
 (1 * (strength $ baseStats))
-------------------------------------------------------------------------------
getDefense :: BaseStats -> Int
getDefense baseStats =
 (3 * (dexterity $ baseStats)) +
 (1 * (constitution $ baseStats))
-------------------------------------------------------------------------------
getAscendancy :: BaseStats -> Int
getAscendancy baseStats =
 (3 * (wisdom $ baseStats)) +
 (1 * (strength $ baseStats)) +
 (1 * (constitution $ baseStats))
-------------------------------------------------------------------------------
getProtection :: BaseStats -> Int
getProtection baseStats =
 (3 * (will $ baseStats)) +
 (1 * (strength $ baseStats)) +
 (1 * (constitution $ baseStats))
-------------------------------------------------------------------------------
initialPlayer :: Player
initialPlayer =
 getPlayer $
 BaseStats {
  intelligence = 5,
  wisdom = 5,
  constitution = 5,
  strength = 5,
  dexterity = 5,
  will = 5
 }
-------------------------------------------------------------------------------
testMob :: Player
testMob =
 getPlayer $
 BaseStats {
  intelligence = 3,
  wisdom = 3,
  constitution = 5,
  strength = 3,
  dexterity = 3,
  will = 2
 }
-------------------------------------------------------------------------------
computeAccuracyAttack :: Player -> Player -> Double
computeAccuracyAttack attacker defender =
 let attack = fromIntegral $ getAttack $ baseStats $ attacker in
 let defense = fromIntegral $ getDefense $ baseStats $ defender in
 if attack == 0 && defense == 0 then 0.5 else
 attack / (attack + defense)
-------------------------------------------------------------------------------
computeAccuracySpell :: Player -> Player -> Double
computeAccuracySpell caster defender =
 let ascendancy = fromIntegral $ getAscendancy $ baseStats $ caster in
 let protection = fromIntegral $ getProtection $ baseStats $ defender in
 if ascendancy == 0 && protection == 0 then 0.5 else
 ascendancy / (ascendancy + protection)
-------------------------------------------------------------------------------
computeDamage :: Player -> Player -> Double
computeDamage attacker defender =
 let damage = fromIntegral $ getDamage $ baseStats $ attacker in
 let resistance = fromIntegral $ getResist $ baseStats $ defender in
 if damage == 0 && resistance == 0 then 0.5 else
 (damage / (damage + resistance)) * damage
-------------------------------------------------------------------------------
computePower :: Player -> Player -> Double
computePower attacker defender =
 let power = fromIntegral $ getPower $ baseStats $ attacker in
 let resistance = fromIntegral $ getResist $ baseStats $ defender in
 if power == 0 && resistance == 0 then 0.5 else
 (power / (power + resistance)) * power
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
