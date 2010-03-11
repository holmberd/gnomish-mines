module WorldFunctions where

import GnomishMinesTypes
import World
import List

-- replaces a room with another room in the rooms.
-- x' and y' is counters and should always start on zero.
replaceRoom :: Room -> Rooms -> Int -> Int -> Rooms
replaceRoom room [] x' y' = []
replaceRoom room (x:xs) x' y'
	| y'<=(length (x:xs)) = [(xRooms room x x')] ++ replaceRoom room xs 0 (y'+1)
	| otherwise = []
xRooms :: Room -> [Room] -> Int -> [Room]
xRooms room x x'
	| (x'+1)>(length x) = []
	| (location(x!!x')) == location(room) = [room] ++ xRooms room x (x'+1)
	| otherwise = [(x!!x')] ++ xRooms room x (x'+1)

-- Makes a world given a room to save in the new world and a new player.	
makeWorld :: (Room,Player) -> World -> World
makeWorld (room,player) (World y' x' rooms' player' doorlocs') 
	= World { maxY=y' 
			, maxX = x' 
			, rooms=(replaceRoom (fst(room,player)) rooms' 0 0) 
			, player=(snd(room,player)) 
			, doorLocs=doorlocs'
			} 

-- Returns a room from the world given it's location.	
getRoom :: World -> Location -> Room
getRoom world (Location x y) = ((rooms(world))!!(y-1))!!(x-1)

-- Sets the current Room the player is in given location, player and world.
setCurrentRoom :: Location -> Player -> World -> (Room,Player)
setCurrentRoom loc (Player inv score' currentRoom') world = ((getRoom world loc),(Player {inventory=inv, score=score', currentRoom=loc}))

-- Prints the description of the room.
printDescription :: Room -> IO()
printDescription room = putStr (description(room))

-- Gets all the directions to nearby rooms that you can go into.
-- The int(n) should always be = 0, it's a counter.
getDir :: Room -> World -> Int -> [Location]
getDir room world n
	| n+1>(length (doorLocs(world))) = []
	| (location(room)) == fst((doorLocs(world))!!n) 
		= [(snd((doorLocs(world))!!n))] ++ getDir room world (n+1)
	| otherwise = getDir room world (n+1)
	
-- makes Directions(Locations) to a pair with a location
-- and a direction, that are either North, South, East or West. 	
makeLocDir :: [Location] -> Room -> [(String,Location)]
makeLocDir [] room = []
makeLocDir (loc:loc') room = [((cmpDir loc room),loc)] ++ makeLocDir loc' room

-- Compares a Location with the location in the current room
-- and tells you if the new location is N,S,E,W from you.
cmpDir :: Location -> Room -> String
cmpDir (Location x y) (Room descp items (Location x' y'))
	| y<y' = "North"
	| y>y' = "South"
	| x>x' && y==y' = "East"
	| x<x' && y==y' = "West"
	
-- Check what kind of directions there is in the list.
-- Part of printDir function.
checkDir :: [(String,Location)] -> String
checkDir [] = ""
checkDir (locdir:locdir')
	| fst(locdir) == "North" = "There is a passageway to the North\n" ++ checkDir locdir'
	| fst(locdir) == "South" = "There is a passageway to the South\n" ++ checkDir locdir'
	| fst(locdir) == "East" = "There is a passageway to the East\n" ++ checkDir locdir'
	| fst(locdir) == "West" = "There is a passageway to the West\n" ++ checkDir locdir'

-- Prints all the avaliable directions out of a room, to the screen.	
printDir :: [(String,Location)] -> IO()
printDir locdir = do putStr (checkDir locdir)

-- Returns the Items names in a string.
-- items with a property smaller then 5 is considered a item.
getItemsNames :: Items -> String
getItemsNames [] = []
getItemsNames (item:item')
	| (property(item)) < 5 = "a " ++ (name(item)) ++ "\n" ++ getItemsNames item'
	| otherwise = getItemsNames item'
	
-- Prints out all the items in a room.
printRoomItems :: Room -> IO()
printRoomItems room 
	| (length (getItemsNames (items(room))))>0 = do putStrLn ("There are some items here: ")
	                                                putStr (getItemsNames(items(room)))
	| otherwise = putStr ""

-- Returns the Monsters names in a string.
-- items with a property higher then 4 is considered a monster.
getMonstersNames :: Items -> String
getMonstersNames [] = []
getMonstersNames (item:item')
	| (property(item)) >= 5 = "a " ++ (name(item)) ++ "\n" ++ getMonstersNames item'
	| otherwise = getMonstersNames item'
	
-- Prints out all the Monsters in a room.
printRoomMonsters :: Room -> IO()
printRoomMonsters room  
	| (length (getMonstersNames (items(room))))>0 = do putStrLn ("There are Monsters here: ")
	                                                   putStr (getMonstersNames(items(room)))
	| otherwise = putStr ""
                            

