-------------------------------------------------------------------------------
module Enchantment where
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
-------------------------------------------------------------------------------
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
enchantmentTypeCost WeaponDamageBonus = 6
enchantmentTypeCost WeaponAttackBonus = 3
enchantmentTypeCost WeaponDefenseBonus = 3
-------------------------------------------------------------------------------
enchantmentCost :: Enchantment -> Int
enchantmentCost (MkEnchantment enchantmentType amount) =
 (enchantmentTypeCost enchantmentType) * amount
-------------------------------------------------------------------------------
data ItemRarityType = Common | Uncommon | Rare deriving Show
-------------------------------------------------------------------------------
displayEnchantment :: Enchantment -> String
displayEnchantment (MkEnchantment enchantmentType bonus) = (show enchantmentType) ++ "+" ++ (show bonus)
-------------------------------------------------------------------------------
displayEnchantments :: [Enchantment] -> String
displayEnchantments [] = ""
displayEnchantments (x:xs) = (displayEnchantment x) ++ "\n" ++ (displayEnchantments xs)
-------------------------------------------------------------------------------
