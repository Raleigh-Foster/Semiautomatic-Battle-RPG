{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------------------
module Main where
-------------------------------------------------------------------------------
import System.Random
import Player
import Control.Monad
import Control.Concurrent
import System.IO
import Text.Printf
import Text.Read
import Data.List (intersperse)
import Data.Char (isSpace)
import Debug.Trace
-------------------------------------------------------------------------------
{-
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
-}
-------------------------------------------------------------------------------
removeAt :: Int -> [a] -> [a]
removeAt n xs = let (ys, zs) = splitAt n xs in ys ++ tail zs
-------------------------------------------------------------------------------

-- ignoring max radiation for now!!
-------------------------------------------------------------------------------
tryWearHelmet :: MVar Game -> IO ()
tryWearHelmet m = pure ()
-------------------------------------------------------------------------------
tryWearTorso :: MVar Game -> IO ()
tryWearTorso m = pure ()
-------------------------------------------------------------------------------
tryWearPants :: MVar Game -> IO ()
tryWearPants m = pure ()
-------------------------------------------------------------------------------
tryWearRightRing :: MVar Game -> IO ()
tryWearRightRing m = pure ()
-------------------------------------------------------------------------------
tryWearLeftRing :: MVar Game -> IO ()
tryWearLeftRing m = pure ()
-------------------------------------------------------------------------------
tryWearItem :: Game -> Int -> (Game, [String])
tryWearItem g i =
 let j = (length $ inventory $ player g) + 1 in
 if i > j
  then (g, ["You don't even have that many items!"])
  else
   let heldItem = (inventory $ player $ g) !! i in
   case location heldItem of
    Belt ->
     case belt $ player $ g of
      Nothing ->
       let items' = removeAt i $ inventory $ player $ g in
       let p' = refreshPlayer ((player $ g) {belt = Just $ heldItem, inventory = items'}) in
       trace ("CURRENT PLAYER MAX HP: " ++ (show $ maxHp $ p')) $ (g {player = p'}, ["You wrap the belt around your waist."])
      Just _ -> (g, ["You are already wearing a belt."])
    _ -> undefined


{-
 case 
 case belt $ player $ g of
  Nothing -> (g, ["You wrap the belt around your waist."])
  Just _ -> (g, ["You are already wearing a belt."])

-}

-- let maxRadiation = (level $ player $ g) * 10
 
-------------------------------------------------------------------------------
tryWearScarf :: MVar Game -> IO ()
tryWearScarf m = pure ()
-------------------------------------------------------------------------------
tryWearGloves :: MVar Game -> IO ()
tryWearGloves m = pure ()
-------------------------------------------------------------------------------
tryWearSocks :: MVar Game -> IO ()
tryWearSocks m = pure ()
-------------------------------------------------------------------------------
tryWearBoots :: MVar Game -> IO ()
tryWearBoots m = pure ()
-------------------------------------------------------------------------------
tryWearShirt :: MVar Game -> IO ()
tryWearShirt m = pure ()
-------------------------------------------------------------------------------
-- creates one on the ground...
createBelt :: MVar Game -> IO ()
createBelt m = do
 g <- takeMVar m
 let r = rnd g
 let beginBelt = BeginItem {beginLocation = Belt, beginEnchantments = [MkEnchantment StrengthBonus 200, MkEnchantment MaxManaBonus 5], beginDepth = 5}
 let (belt, r') = instantiateItem (beginBelt, r)
 putMVar m (g {rnd = r', nearbyItems = belt:(nearbyItems g)})
 pure ()

-- instantiateItem :: (BeginItem, StdGen) -> (Item, StdGen)
{-
data BeginItem =
 BeginItem {
  beginLocation :: Location,
  beginEnchantments :: [Enchantment],
  beginDepth :: Int -- the depth at which the item was retrieved (affects rad reduction generation!)
 }
-}
{-
do
 g <- takeMVar m
 let r = rnd g

 let helmet = Item Helmet (Enchantments [MkEnchantment StrengthBonus 2, MkEnchantment MaxManaBonus 5]) 0

 let baseRadiation = itemCost helmet
 let baseRadFactor = 1.0

 let (dropRoll :: Double, rnd') = random $ rnd g

 let radFactorAddition = (dropRoll * 2) - 1
 let radFactor = (2 ** radFactorAddition)
 let radiation = round ((fromIntegral baseRadiation) * radFactor)
 putMVar m (g {rnd = rnd', nearbyItems = (Item Helmet (Enchantments [MkEnchantment StrengthBonus 2, MkEnchantment MaxManaBonus 5]) radiation):(nearbyItems g)})
 pure ()
-}



{-main :: IO ()
main = do
 g <- newStdGen
 stepGame $ MkGame g initialPlayer testMob
-}


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
{-
handleCommand :: String -> [String]
handleCommand "heal" = ["\ESC[38;5;254mYou open your hand and cast a healing incantation, healing \ESC[36m"++(show 5)++"\ESC[38;5;254m hp\ESC[0m"]
handleCommand _ = []
-}

listInventory :: [Item] -> String
listInventory [] = ""
listInventory (x:xs) = (show x) ++ "\n" ++ (listInventory xs)

handleCommand' :: Game -> String -> (Game, [String])
handleCommand' game "heal" = (game, ["\ESC[38;5;254mYou open your hand and cast a healing incantation, healing \ESC[36m"++(show 6)++"\ESC[38;5;254m hp\ESC[0m"])
handleCommand' game "enter" = (game {mobSlain=False, mob = (mob game) {damageTaken=0}}, ["\ESC[38;5;254mYou open a door and enter the next room.\ESC[0m"])
handleCommand' game "help legendary blademaster" = (game, ["\ESC[38;5;254m"++(legendaryBlademasterHelp)++"\ESC[0m"])
handleCommand' game "help ethereal blade" = (game, ["\ESC[38;5;254m"++(etherealBladeHelp)++"\ESC[0m"])
handleCommand' game "inventory" = (game, ["\ESC[38;5;254m"++(listInventory $ inventory $ player $ game)++"\ESC[0m"])
handleCommand' game "eq" = (game, ["\ESC[38;5;254m"++(show $ belt $ player $ game)++"\ESC[0m"])
handleCommand' game "get belt" =
 if (length $ nearbyItems $ game) == 0
  then  (game, ["\ESC[38;5;254m"++("No such item nearby")++"\ESC[0m"])
  else  (game {player = (player game) {inventory = (head $ nearbyItems $ game):(inventory $ player $ game)}, nearbyItems = tail $ nearbyItems game}, ["\ESC[38;5;254m"++("You pick up a belt.")++"\ESC[0m"])


-- handleCommand' game "wear belt" = tryWearBelt game


handleCommand' game msg =
 if (take 4 msg) == "wear"
  then
   let mi :: Maybe Int = readMaybe $ trim $ drop 4 msg in
   case mi of
    Nothing -> (game, ["Error parsing " ++ (drop 4 msg) ++ "\nPlease enter a number corresponding to the index in your inventory (0-indexed)"])
    Just i -> tryWearItem game i
  else (game, [])

handleCommands'' :: Game -> [String] -> [String] -> (Game, [String])
handleCommands'' game [] acc = (game, acc)
handleCommands'' game (x:xs) acc =
 let (g', msgs) = handleCommand' game x in
 handleCommands'' g' xs (acc ++ msgs)

handleCommands' :: Game -> [String] -> (Game, [String])
handleCommands' game commands = handleCommands'' game commands []

parseMessage :: String -> String -- don't properly unescape yet
parseMessage "heal" = "\ESC[32mheal\ESC[0m OK"
parseMessage "attack" = "\ESC[32mattack\ESC[0m OK"
parseMessage "enter" = "\ESC[32menter\ESC[0m OK"
parseMessage "help legendary blademaster" = "\ESC[32mhelp legendary blademaster\ESC[0m OK"
parseMessage "help ethereal blade" = "\ESC[32mhelp ethereal blade\ESC[0m OK"
parseMessage "inventory" = "\ESC[32minventory\ESC[0m OK"
parseMessage "get belt" = "\ESC[32mget belt\ESC[0m OK"
parseMessage "eq" = "\ESC[32meq\ESC[0m OK"


parseMessage msg =
 if take 4 msg == "wear" then "\ESC[32mwear\ESC[0m OK"
{-
  then
   let i :: Maybe Int = readMaybe $ trim $ drop 4 msg in
   wearItem 
-}
  else "\ESC[31mUnknown command "++msg++"\ESC[0m"

--legendaryBlademasterHelp

prompt :: Game -> String
prompt game =
 let spaces = 40 in
 (replicate spaces ' ') ++
 (healthBar $ player game) ++
 "  " ++
 (manaBar $ player game) ++
 "    Target " ++
 (healthBar $ mob game)

putProgress :: String -> IO ()
putProgress s = hPutStr stderr $ "\r\ESC[K" ++ s


healthBar :: Player -> String
healthBar player =
 let maxHp = getMaxHp $ baseStats $ player in
 let damage = damageTaken $ player in
 if (damage * 2 > maxHp)
  then
 "HP \ESC[31m" ++ (show (maxHp - damage)) ++ "\ESC[0m/\ESC[31m" ++ (show maxHp) ++ "\ESC[0m"
 else
 "HP \ESC[32m" ++ (show (maxHp - damage)) ++ "\ESC[0m/\ESC[32m" ++ (show maxHp) ++ "\ESC[0m"

manaBar :: Player -> String
manaBar player =
 let maxMana = getMaxMana $ baseStats $ player in
 let used = manaUsed player in
 "Mana \ESC[34m" ++ (show (maxMana - used)) ++ "\ESC[0m/\ESC[34m" ++ (show maxMana) ++ "\ESC[0m"

drawProgressBar :: MVar Game -> [String] -> Int -> Int -> Rational -> IO String
drawProgressBar m gameMessages hp width progress = do
  game <- takeMVar m
  let (g', extraMessages) = handleCommands' game (commandQueue game)
  putMVar m g' {commandQueue = []}
  let messages = {-trace ("in processbar, monster slain is" ++ (show $ mobSlain g')) $-} gameMessages ++ (map parseMessage (commandQueue game)) ++ extraMessages
  if messages == []
   then pure $ prompt g'
   else pure ((concat $ intersperse "\n" messages) ++ "\n" ++ (prompt g'))
  where bars = 0
        spaces = width - bars

drawPercentage :: String -> Rational -> String
drawPercentage currentCommand progress = (printf "%3d%%" (truncate (progress * 100) :: Int)) ++ "\r" ++ currentCommand

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering 
  -- c <- getChar
  m <- newEmptyMVar
  g <- newStdGen
  let game = Game {{-worldLocation=FightingMob,-} rnd=g, player=initialPlayer, mob=testMob, mobSlain=False, currentCommand="",commandQueue=[], messageQueue=[], nearbyItems = []}
  putMVar m $ game
  forkIO $ userInput m
  fightMob m
{-
  forM_ [1..100] $ \i -> do
    let progress = fromIntegral i / 100
    game' <- takeMVar m
    if mobSlain game' then do
    putProgress $ drawProgressBar game' [] i 40 progress ++ " " ++ drawPercentage (currentCommand game') progress
    putMVar m $ game' {commandQueue = []}-- (if length _commands > 0 then tail _commands else _commands)
    threadDelay 250000
    else do
    (game'', messages) <- attacks game'
    putProgress $ drawProgressBar game'' messages i 40 progress ++ " " ++ drawPercentage (currentCommand game'') progress
    putMVar m $ game'' {commandQueue = []}-- (if length _commands > 0 then tail _commands else _commands)
    threadDelay 250000
  putProgress "The air shimmers as you exit the world."
  hPutChar stderr '\n'
-}

fightMob :: MVar Game -> IO ()
fightMob m = do
    -- let progress = fromIntegral i / 100
    _game' <- takeMVar m
    putMVar m _game'
    if mobSlain _game' then do
    s <- drawProgressBar m [] 0 40 0
    game' <- takeMVar m
    putProgress (s ++ " " ++ drawPercentage (currentCommand game') 0)
    putMVar m $ game' -- {commandQueue = []}-- (if length _commands > 0 then tail _commands else _commands)
    handleDrop m
    else do
    (game'', messages) <- attacks _game'
    _ <- takeMVar m
    putMVar m game''
    s <- drawProgressBar m messages 0 40 0
    putProgress (s ++ " " ++ drawPercentage (currentCommand game'') 0)
    --g' <- takeMVar m
    --putMVar m $ g' -- {commandQueue = []}-- (if length _commands > 0 then tail _commands else _commands)
    threadDelay 250000
    fightMob m

idle :: MVar Game -> IO ()
idle m = do
 threadDelay 250000
 g <- takeMVar m
 putMVar m $ g -- {commandQueue = []}
 s <- drawProgressBar m [] 0 40 0
 g' <- takeMVar m
 putProgress (s ++ " " ++ drawPercentage (currentCommand g') 0)
 putMVar m g'
 if mobSlain g'
  then {-trace "mob is dead" $-} idle m
  else {-trace "beginning to fight next mob" $-} fightMob m


handleDrop :: MVar Game -> IO ()
handleDrop m = do
 game <- takeMVar m
 let (dropRoll :: Double, rnd') = random $ rnd game
 putStrLn "The goblin sublimates into a purple cloud of swirling mist."
 putStrLn "As the mist begins to dissipate, some instead coalesces and desublimates into a belt."
 if dropRoll > 0.99 then do
  putStrLn "A rare belt drops to the ground a few feet in front of you."
 else if dropRoll > 0.9 then do
  putStrLn "An uncommon belt drops to the ground a few feet in front of you."
 else do
  putStrLn "A common belt drops to the ground a few feet in front of you."
 putMVar m $ game {rnd = rnd'{-, mobSlain=False, mob = (mob game) {damageTaken=0}-}}
 createBelt m
 putStrLn "should drop a helmet??! (maybe belt right? lol)"
 _g <- takeMVar m
 putStrLn (show $ nearbyItems _g)
 putMVar m _g
 idle m

userInput :: MVar Game -> IO ()
userInput m = do
 -- v <- readLn :: IO String
 v <- getChar
 
 game <- takeMVar m
 let command = currentCommand game
 if v == '\n'
  then if command == ""
   then do
    putMVar m game
    userInput m
   else do
    putMVar m $ game {currentCommand = "", commandQueue = ((trim command):(commandQueue game))}
    userInput m
  else if v == '\DEL'
   then do
    putMVar m $ game {currentCommand = take ((length $ command) - 1) command}
    userInput m
   else do
    putMVar m $ game {currentCommand = command ++ [v]}
    userInput m
 -- putMVar m $ game {currentCommand = (if v /= '\DEL' then command ++ [v] else take ((length command) - 1) command)}
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
data WorldLocation
 = FightingMob
 | KilledMob -- perhaps more to come!?
-------------------------------------------------------------------------------
stepGame :: Game -> IO ()
stepGame g = stepGame' 10 g
-------------------------------------------------------------------------------
stepGame' :: Int -> Game -> IO ()
stepGame' i g =
 if i == 0 then pure ()
 else do
  showPlayer $ player g
  showPlayer $ mob g
  (g',_) <- attacks g
  stepGame' (i - 1) g'
-------------------------------------------------------------------------------
attacks :: Game -> IO (Game, [String])
attacks game = do
 let (attackRoll, g') = random $ rnd game
 if computeAccuracyAttack (player game) (mob game) > attackRoll
  then do
   let damage = round $ computeDamage (player game) (mob game) -- NO DAMAGE ROLL YET, ALWAYS DO FULL DAMAGE!
   let mob' = (mob game) {damageTaken = (damageTaken $ mob game) + damage}
   if damageTaken mob' >= (getMaxHp $ baseStats $ mob game)
    then
     pure (game {rnd = g', mob = mob', mobSlain = True}, ["hit!", "mob slain!"])
    else
     pure (game {rnd = g', mob = mob'}, ["hit!"])
  else pure (game {rnd = g'}, ["miss!"])
-- let (damageRoll, g'') = random g' -- attacks do between 0% and 100% of damage.
-------------------------------------------------------------------------------
