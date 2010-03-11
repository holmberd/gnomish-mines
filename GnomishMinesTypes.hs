module GnomishMinesTypes where


data World = World { maxY, maxX :: Int, rooms :: Rooms, player :: Player, doorLocs :: [(Location,Location)]}
			 deriving (Eq, Show)

data Location = Location { x, y :: Int }
				deriving (Eq, Show)

data Room = Room { description :: String,  items :: Items, location :: Location }
			deriving (Eq, Show)

data Item = Item { name :: String, value :: Int, property :: Int } | Empty
			deriving (Eq, Show)

type Items = [Item]

type Rooms = [[Room]]

data Player = Player {inventory :: Items, score :: Int, currentRoom :: Location}
			  deriving (Eq, Show)
			  
