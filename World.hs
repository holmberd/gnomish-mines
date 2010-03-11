module World where

import GnomishMinesTypes

world :: World
world = World { maxY=2
			  , maxX=2
			  , rooms=[[room1,room2],[room3,room4]]
			  , player = Player {inventory=[item4], score=1, currentRoom=(Location 1 1)}
			  , doorLocs=[((Location 2 2),(Location 2 1)),((Location 2 2),(Location 1 2))
			  ,((Location 1 1),(Location 2 1)),((Location 1 1),(Location 1 2))
			  ,((Location 2 1),(Location 2 2)),((Location 1 2),(Location 2 2))
			  ,((Location 2 1),(Location 1 1)),((Location 1 2),(Location 1 1))]
			  }

rooms1 :: Rooms
rooms1 = [[room1,room2],[room3,room4]]
room1, room2, room3, room4 :: Room
room1 = Room { description="You are standing at the bottom staircase of a dungeon.\n", items=[item1], location=(Location 1 1)}
room2 = Room { description="There is light coming down from a hole in the celing\nand there is a pile of bones in the middle of the room.\n", items=[item4], location=(Location 2 1)}
room3 = Room { description="There is some empty barrels in the corner here\nand the room smells of smoke\n", items=[item3], location=(Location 1 2)}
room4 = Room { description="You have entered a great hall,\nthere is a big fire burning in the middle of the hall.\n", items=[item3,item2], location=(Location 2 2)}

item1, item2, item3, item4 :: Item
item1 = Item { name="Sword", value=0, property=4}
item2 = Item { name="Dragon", value=1, property=10}
item4 = Item { name="Gold", value=1, property=1}
item3 = Item { name="Lantern", value=1, property=3}

help :: String
help = "\n-------------------------------------------------" 
	++ "\nWelcome to the GnomishMines help meny.\n" 
	++ " - Take up an item, type: take item-name\n" 
	++ " - Drop an item, type: drop item-name\n"
	++ " - Move to a different room, type: South, North, East or West\n" 
	++ " - Check inventory, type: inventory\n" 
	++ " - Attack monster/npc, type: kill npc-name\n"
	++ " - Check score, type: score" 
	++ " - Exit game, type: exit\n"
	++ "-------------------------------------------------\n"




