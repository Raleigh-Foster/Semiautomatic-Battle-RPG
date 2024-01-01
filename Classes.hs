-------------------------------------------------------------------------------
module Classes where
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
data Class -- perhaps more later!!
 = Avatar -- unclassed
 | Lich LichPowers
 | Werewolf WerewolfPowers
 | Vampire VampirePowers
 deriving Show
-------------------------------------------------------------------------------
data LichPowers =
 LichPowers {
  etherealBlade :: Int,
  magician :: Int,
  archmage :: Int,
  martialArtist :: Int,
  manaShield :: Int,
  deathKnight :: Int
 } deriving Show

{-
data LichPowers
 = LegendaryBlademaster -- bonus crit damage
 | EtherealBlade -- summon a 0 rad blade that gains power as you get kills
 | Magician -- bonus mana regeneration and spell power -- rename to wizardry?
 | Archmage -- gain access to a variety of spells -- rename to wizardry?
 | MartialArtist -- Your dexterity grants additional bonuses
 | ManaShield -- Divert some of your maximum mana to provide additional defense and protection
 | DeathKnight -- Your ethereal blade gains additional bonuses every time it crits
-}
-------------------------------------------------------------------------------
martialArtistHelpFile :: HelpFile
martialArtistHelpFile =
 HelpFile {
  helpName = "Martial Artist",
  helpText =
   "Your foray into death has taught you a great deal about the strengths and weaknesses of the body.\n" ++
   "Each point of dexterity now provides one additional attack and defense for each level in this power."
 }
-------------------------------------------------------------------------------
martialArtistHelp :: String
martialArtistHelp = serializeHelp martialArtistHelpFile
-------------------------------------------------------------------------------
manaShieldHelpFile :: HelpFile
manaShieldHelpFile =
 HelpFile {
  helpName = "Mana Shield",
  helpText =
   "You have developed the ability to redirect damage to your mind.\n" ++
   "You gain access to the \'shield on\' and \'shield off\' commands." ++
   "While activated, attacks drain your mana instead of damaging your health." ++
   "Each mana absorbs one damage for each level in this power." ++
   "Upon receiving damage corresponding to a fraction of one mana, an entire mana is consumed."
 }
-------------------------------------------------------------------------------
manaShieldHelp :: String
manaShieldHelp = serializeHelp manaShieldHelpFile
-------------------------------------------------------------------------------
legendaryBlademasterHelpFile :: HelpFile
legendaryBlademasterHelpFile =
 HelpFile {
  helpName = "Legendary Blademaster",
  helpText = "Instead of the usual 200% damage, your critical strikes do (200+10X)% damage,\nwhere X is your level in this power."
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
deathKnightHelpFile :: HelpFile
deathKnightHelpFile =
 HelpFile {
  helpName = "Death Knight",
  helpText =
   "While wielding your ethereal blade, every time you land a critical hit,\n" ++
   "you gain additional damage equal to your spell amplification times your level in this power." -- strength after a certain level?
 }
-------------------------------------------------------------------------------
deathKnightHelp :: String
deathKnightHelp = serializeHelp deathKnightHelpFile
-------------------------------------------------------------------------------
magicianHelpFile :: HelpFile
magicianHelpFile =
 HelpFile {
  helpName = "Magician",
  helpText =
   "Your connection to the magical plane has granted you various abilities.\n" ++
   "You gain the \'magic\' ability, which enables you to cast spells.\n" ++
   "At level 1, you gain the heal ability.\n" ++
   "At level 5, you gain the bless ability.\n" ++
   "At level 10, you gain the bark skin ability.\n" ++
   "At level 20, you gain the giant growth ability.\n" ++
   "At level 30, you gain the stone skin ability.\n" ++
   "At level 50, you gain the bladestorm ability.\n" ++
   "At level 100, you gain the final form ability.\n"
 }
-------------------------------------------------------------------------------
magicianHelp = serializeHelp magicianHelpFile
-------------------------------------------------------------------------------
blessHelpFile :: HelpFile
blessHelpFile =
 HelpFile {
  helpName = "Bless",
  helpText =
   "Bless is a spell accessed via the \'magic\' command.\n" ++
   "It requires at least 5 levels in the Magician power.\n" ++
   "It reserves 100 maximum mana to cast.\n" ++
   "While under its effect, you gain +5 attack and +5 defense for each level in Magician."
 }
-------------------------------------------------------------------------------
blessHelp :: String
blessHelp = serializeHelp blessHelpFile
-------------------------------------------------------------------------------
barkSkinHelpFile :: HelpFile
barkSkinHelpFile =
 HelpFile {
  helpName = "Bark Skin",
  helpText =
   "Bark Skin is a spell accessed via the \'magic\' command.\n" ++
   "It requires at least 10 levels in the Magician power.\n" ++
   "It reserves 250 maximum mana to cast.\n" ++
   "While under its effect, you gain +10 resistance for each level in Magician."
 }
-------------------------------------------------------------------------------
barkSkinHelp :: String
barkSkinHelp = serializeHelp barkSkinHelpFile
-------------------------------------------------------------------------------
giantGrowthHelpFile :: HelpFile
giantGrowthHelpFile =
 HelpFile {
  helpName = "Giant Growth",
  helpText =
   "Giant Growth is a spell accessed via the \'magic\' command.\n" ++
   "It requires at least 20 levels in the Magician power.\n" ++
   "It reserves 500 maximum mana to cast.\n" ++
   "While under its effect, you gain +15 damage for each level in Magician."
 }
-------------------------------------------------------------------------------
giantGrowthHelp :: String
giantGrowthHelp = serializeHelp giantGrowthHelpFile
-------------------------------------------------------------------------------
stoneSkinHelpFile :: HelpFile
stoneSkinHelpFile =
 HelpFile {
  helpName = "Stone Skin",
  helpText =
   "Stone Skin is a spell accessed via the \'magic\' command.\n" ++
   "It requires at least 30 levels in the Magician power.\n" ++
   "It reserves 1000 maximum mana to cast.\n" ++
   "While under its effect, you gain +30 strength and +30 constitution,\n" ++
   "as well as get -100 to attack and defense for each level in Magician." -- have to make sure no divide by zero issues if values go negative, etc.
 }
-------------------------------------------------------------------------------
stoneSkinHelp :: String
stoneSkinHelp = serializeHelp stoneSkinHelpFile
-------------------------------------------------------------------------------
finalFormHelpFile :: HelpFile
finalFormHelpFile =
 HelpFile {
  helpName = "Final Form",
  helpText =
   "Final Form is a spell accessed via the \'magic\' command.\n" ++
   "It requires at least 100 levels in the Magician power.\n" ++
   "Final Form requires 10000 mana to cast.\n" ++
   "While under its effect, for each point of spell amplification (rounded down), you gain the following benefits:\n" ++
   "(1) The bonuses to your critical strike from Legendary Blademaster gain an additional +10% per level, and\n" ++
   "(2) Each point of strength gives you one additional damage."

 }
-------------------------------------------------------------------------------
finalFormHelp = serializeHelp finalFormHelpFile
-------------------------------------------------------------------------------
bladestormHelpFile :: HelpFile
bladestormHelpFile =
 HelpFile {
  helpName = "Bladestorm",
  helpText =
   "Bladestorm is a spell accesssed via the \'magic\' command.\n" ++
   "It requires at least 50 levels in the Magician power.\n" ++
   "Bladestorm is a channel spell, and has no cost to activate.\n" ++
   "While under its effect, each attack drains 100 of your mana, has 100% accuracy, and hits 5 times."
 }
-------------------------------------------------------------------------------
-- critical strikes should also bypass resistance! (huge help against very high resist enemies; otherwise on average another 2x approximately)
-------------------------------------------------------------------------------
data WerewolfPowers =
 WerewolfPowers {
  naturalFortitude :: Int, -- You have additional resistance
  supernaturalHealing :: Int, -- You have additional regeneration
  clawsOfAttack :: Int, -- You have a powerful unarmed attack
  wolfForm :: Int, -- Gain massive resistances and damage; prevents use of most item slots (separate inventory).
  bearSpirit :: Int, -- Gain bonuses at low health; prevents use of other spirit powers
  foxSpirit :: Int, -- Gain additional defense; prevents use of other spirit powers
  sharkSpirit :: Int, -- Your attacks heal you; prevents use of other spirit powers
  penguinSpirit :: Int, -- Gain massive resistances; prevents use of other spirit powers
  shamanism :: Int -- Gain access to a variety of spells
 } deriving Show
-------------------------------------------------------------------------------
foxSpiritHelpFile :: HelpFile
foxSpiritHelpFile =
 HelpFile {
  helpName = "Fox Spirit",
  helpText = "Each point of dexterity now provides two additional defense for each level in this power."
 }
-------------------------------------------------------------------------------
sharkSpiritHelpFile :: HelpFile
sharkSpiritHelpFile =
 HelpFile {
  helpName = "Shark Spirit",
  helpText = "When you successfully attack, you regenerate health equal to your strength times your rank in this power."
 }
-------------------------------------------------------------------------------
sharkSpiritHelp :: String
sharkSpiritHelp = serializeHelp sharkSpiritHelpFile
-------------------------------------------------------------------------------
penguinSpiritHelpFile :: HelpFile
penguinSpiritHelpFile =
 HelpFile {
  helpName = "Penguin Spirit",
  helpText =
   "Each point of wisdom grants additional resistance equal to your spell amplification times your rank in this power."
 }
-------------------------------------------------------------------------------
penguinSpiritHelp :: String
penguinSpiritHelp = serializeHelp penguinSpiritHelpFile
-------------------------------------------------------------------------------
shamanismHelpFile :: HelpFile
shamanismHelpFile =
 HelpFile {
  helpName = "Shamanism",
  helpText =
   "Your connection to the spiritual plane has granted you various abilities.\n" ++
   "You gain the \'magic\' ability, which enables you to cast spells.\n" ++
   "At level 1, you gain the heal ability.\n" ++
   "At level 5, you gain the bless ability.\n" ++
   "At level 10, you gain the bark skin ability.\n" ++
   "At level 20, you gain the giant growth ability.\n" ++
   "At level 30, you gain the stone skin ability.\n" ++
   "At level 50, you gain the lightning shield ability.\n" ++
   "At level 100, you gain the endurance ability.\n"
 }
-------------------------------------------------------------------------------
data VampirePowers =
 VampirePowers {
  bloodShield :: Int, -- Your blood power fuels a massive shield
  chiropteranHearing :: Int, -- You have additional defense
  nightBlade :: Int, -- Summon a 0 rad blade fueled by your blood power
  bloodFanatic :: Int, -- You gain blood power from your kills
  infusion :: Int, -- Divert some of your maximum mana to your damage
  lifeAffinity :: Int, -- Gain a massive bonus to health regen, prevents training of undead form
  undeathAffinity :: Int -- Gain a massive bonus to mana regen, prevents training of living form
 } deriving Show
-------------------------------------------------------------------------------
bloodFanaticHelpFile :: HelpFile
bloodFanaticHelpFile =
 HelpFile {
  helpName = "Blood Fanatic",
  helpText =
   "When you kill an enemy, in addition to gaining experience,\n" ++
   "You also gain blood equal to the experience times your level in this power.\n" ++
   "As you gain blood, you will also gain blood levels, which can be used to fuel various other abilities.\n" ++
   "The amount of blood required for each blood level increases by 10% per level, just as with your normal experience." ++
   "There is no limit to how much blood you can store, but all blood is lost upon death.\n"
 }
-------------------------------------------------------------------------------
bloodFanaticHelp :: String
bloodFanaticHelp = serializeHelp bloodFanaticHelpFile
-------------------------------------------------------------------------------
bloodShieldHelpFile :: HelpFile
bloodShieldHelpFile =
 HelpFile {
  helpName = "Blood Shield",
  helpText =
   "You have developed the ability to redirect damage to your stored reservoir of blood.\n" ++
   "You gain access to the \'shield on\' and \'shield off\' commands.\n" ++
   "While activated, attacks drain your stored blood instead of damaging your health.\n" ++
   "When you receive damage with your shield activated, first all progress on your current blood level will be lost.\n" ++
   "Then, each level of blood absorbs one damage for each level in this power.\n" ++
   "Upon receiving damage corresponding to a fraction of one blood level, an entire blood level is consumed."
 }
-------------------------------------------------------------------------------
bloodShieldHelp :: String
bloodShieldHelp = serializeHelp bloodShieldHelpFile
-------------------------------------------------------------------------------
lifeAffinityHelpFile :: HelpFile
lifeAffinityHelpFile =
 HelpFile {
  helpName = "Life Affinity",
  helpText =
   "Each point of will grants two times your spell amplification in additional health regen for each rank in this power."
 }
-------------------------------------------------------------------------------
lifeAffinityHelp :: String
lifeAffinityHelp = serializeHelp lifeAffinityHelpFile
-------------------------------------------------------------------------------
undeathAffinityHelpFile :: HelpFile
undeathAffinityHelpFile =
 HelpFile {
  helpName = "undeathAffinity",
  helpText =
   "Each point of will grants an additional mana regen equal to your spell amplification for each rank in this power."
 }
-------------------------------------------------------------------------------
undeathAffinityHelp = serializeHelp undeathAffinityHelpFile
-------------------------------------------------------------------------------
infusionHelpFile :: HelpFile
infusionHelpFile =
 HelpFile {
  helpName = "Infusion",
  helpText =
   "Your attacks do additional damage while also draining your mana.\n" ++
   "Each attack consumes 10% of your current mana, rounding up.\n" ++
   "The attack's damage is increase by your level in this power times the mana consumed, times your spell amplification.\n"
 }
-------------------------------------------------------------------------------
infusionHelp :: String
infusionHelp = serializeHelp infusionHelpFile
-------------------------------------------------------------------------------
