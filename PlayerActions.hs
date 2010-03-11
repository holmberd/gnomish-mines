module PlayerActions where

import GnomishMinesTypes
import WorldFunctions
import World
import List
import IO


-- Converts Just x to x.
remJust (Just x) = x

-- Makes the player choose a direction and gives the location for it
-- if there is a door located between them.
chooseDir :: String -> [(String,Location)]-> World -> Location
chooseDir str [] world = (currentRoom(player(world)))
chooseDir str (locdir:locdir') world
	| str == "North" && fst(locdir) == "North" = remJust(lookup "North" (locdir:locdir'))
	| str == "South" && fst(locdir) == "South" = remJust(lookup "South" (locdir:locdir'))
	| str == "East" && fst(locdir) == "East" = remJust(lookup "East" (locdir:locdir'))
	| str == "West" && fst(locdir) == "West" = remJust(lookup "West" (locdir:locdir'))
	| otherwise = chooseDir str locdir' world

-- Gets an item from a list of items given it's name.
-- If item can't be found it returns Empty.
getItem :: Items -> String -> Item
getItem [] str = Empty
getItem (item:item') str
	| str == (name(item)) && property(item) < 5 = item
	| otherwise = getItem item' str
	
-- Gets an Monster from a list of items given it's name.
-- If Monster can't be found it returns Empty.
getMonster :: Items -> String -> Item
getMonster [] str = Empty
getMonster (item:item') str
	| str == (name(item)) && property(item) >= 5 = item
	| otherwise = getMonster item' str
	
-- Gets an item from a list of items given it's property.
-- If item can't be found it returns Empty.
getItemProp :: Items -> Int -> Item
getItemProp [] n = Empty
getItemProp (item:item') n
	| n == (property(item)) = item
	| otherwise = getItemProp item' n

-- Takes a item from a room and puts it into the inventory,
-- given the items name.
takeItem :: String -> Room -> Player -> (Room,Player)
takeItem str room'@(Room descp items' loc) player'@(Player inv score' currentRoom')
	| (getItem items' str) == Empty = (room',player')
	| otherwise = ((Room { description=descp,items=(delete (getItem items' str) items'), location=loc })
				  , (Player {inventory=([(getItem items' str)] ++ inv), score=score', currentRoom=currentRoom'}))
		  
-- Drops an item from the inventory given the items name.
dropItem :: String -> Player -> Room -> (Room,Player)
dropItem str (Player inv score' currentRoom') (Room descp items' loc)
	| (getItem inv str) == Empty = ((Room descp items' loc),(Player inv score' currentRoom'))
	| otherwise = ((Room { description=descp,items=([(getItem inv str)] ++ items'), location=loc }),
	  (Player {inventory=(delete (getItem inv str) inv), score=score', currentRoom=currentRoom'}))

-- Prints out all the items in the players inventory.
printInvItems :: Player -> IO()
printInvItems player = do putStrLn " "
                          putStrLn ("Your inventory contains: ")
                          putStr (getItemsNames(inventory(player)))

-- Checks if the player input is an action that can be performed in the world.                          
checkPlayerAction :: String -> World -> World
checkPlayerAction str world
	| (words str)!!0 == "drop" 
		= makeWorld (dropItem ((words str)!!1) (player(world)) (getRoom world (currentRoom(player(world))))) world
	| (words str)!!0 == "take"
		= makeWorld (takeItem ((words str)!!1) (getRoom world (currentRoom(player(world)))) (player(world))) world 
	| str == "South" = makeWorld (setCurrentRoom (chooseDir str locdir world) (player(world)) world) world
	| str == "North" = makeWorld (setCurrentRoom (chooseDir str locdir world) (player(world)) world) world
	| str == "East" = makeWorld (setCurrentRoom (chooseDir str locdir world) (player(world)) world) world
	| str == "West" = makeWorld (setCurrentRoom (chooseDir str locdir world) (player(world)) world) world
	| (words str)!!0 == "kill" && (killCheck ((words str)!!1) world) == (True,True) =  makeWorld kill world
	| (words str)!!0 == "kill" && (killCheck ((words str)!!1) world) == (False,True) =  makeWorld reduce world
	| otherwise = world
      where dir = (getDir (getRoom world (currentRoom(player(world)))) world 0)
            locdir = (makeLocDir dir (getRoom world (currentRoom(player(world)))))
            kill = killMonster ((words str)!!1) (player(world)) (getRoom world (currentRoom(player(world))))
            reduce = reduceScore ((words str)!!1) (player(world)) (getRoom world (currentRoom(player(world))))             
            
-- Checks if the player input is a player property input.		
checkPlayerInput :: String -> World -> IO()
checkPlayerInput str world
	| (words str)!!0 == "kill" && (killCheck ((words str)!!1) world) == (True,True) = putStrLn "You have killed the monster"
	| (words str)!!0 == "kill" && (killCheck ((words str)!!1) world) == (False,True) = putStrLn "You couldn't kill the monster with your hands"
	| str == "inventory" = printInvItems ((player(world)))
	| str == "exit" = error "Bye bye"
	| str == "help" = putStr help
	| str == "score" = putStr ("\nYour current score is: " ++ (show (score(player(world)))) ++ "\n")
	| otherwise = putStr "" 

-- Checks if the player have a weapon in inventory when trying to kill
-- a monster. If not the monster wont be killed and the player will loose one in score.
killCheck :: String -> World -> (Bool,Bool)
killCheck str world
	| item == Empty = (False,False)
	| (getItemProp (inventory(player(world))) 4) /= Empty = (True,True)
	| otherwise = (False,True)
	where item = (getMonster (items(getRoom world (currentRoom(player(world))))) str)
	
-- Removes the Monster(item) from the room and gives +1 to score.
killMonster :: String -> Player -> Room -> (Room,Player)
killMonster str (Player inv score' currentRoom') (Room descp items' loc)
	| (getItem items' str) == Empty = ((Room descp items' loc),(Player inv score' currentRoom'))
	| otherwise = ((Room { description=descp,items=(delete (getItem items' str) items'), location=loc })
	,(Player {inventory=(delete (getItem inv str) inv), score=(score'+1), currentRoom=currentRoom'}))
	
-- Reduces the score of the player.
reduceScore :: String -> Player -> Room -> (Room,Player)
reduceScore str (Player inv score' currentRoom') room 
	= (room,(Player {inventory=inv, score=(score'-1), currentRoom=currentRoom'}))




		

	




