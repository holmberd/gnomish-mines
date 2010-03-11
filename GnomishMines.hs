module GnomishMines where

import GnomishMinesTypes
import WorldFunctions
import PlayerActions
import World
import List
import Debug.QuickCheck
import IO
import Char

main :: IO()
main = do putStr ""
          let w = world
          run w

run :: World -> IO()
run world = do putStrLn " "
               let (room) = getRoom world (currentRoom(player(world)))
               printDescription(room)
               printRoomItems(room)
               printRoomMonsters(room)
               let location' = getDir (getRoom world (currentRoom(player(world)))) world 0
               let strloc = makeLocDir location' (getRoom world (currentRoom(player(world))))
               printDir strloc
               game(world)
                
game :: World -> IO()

game world = do putStr ">> "
                str <-getLine
                checkPlayerInput str world
                let world' = checkPlayerAction str world
                checkScore(world')

-- Checks the player score, if score is less then 1 then player is Gameover.
-- If score more then 1, player has won.              
checkScore :: World -> IO()
checkScore world
	| (score(player(world))) < 1 = do putStrLn "You have died!"
	                                  putStrLn "Game Over!"
	| (score(player(world))) > 1 = do putStrLn "Congratiulations! You have finished the game"
	                                  putStrLn "There is no place like 127.0.0.1"
	| otherwise = run world

                                    
                                     
                


           
