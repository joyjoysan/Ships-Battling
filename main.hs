-- BattleShip (Single Player)
import Prelude 
import System.IO
import Data.Char
import Data.Int

--Tracks the round of a game
data State = State Int
  deriving (Eq, Show)

initialState :: State
initialState = State 0

data Board = Sea | Boat | Hit | Miss
  deriving (Eq, Show)

--Computer's board (for now, their destroyer is located at (3,3) (3,4))
compBoard :: [[Board]]
compBoard = [[Sea, Sea, Sea, Sea, Sea],
             [Sea, Sea, Sea, Boat, Sea],
             [Sea, Sea, Sea, Boat, Sea],
             [Sea, Sea, Sea, Boat, Sea], 
             [Sea, Sea, Sea, Boat, Sea]]

--This 2d list contains the set of coordinates that the computer will attack
compMoveSet :: [[Int]]
compMoveSet =   [[2,2], [4,0], [2,3], [1,2], [0,4], 
                 [0,0], [4,3], [1,3], [4,2], [0,2], 
                 [2,4], [2,0], [3,0], [3,3], [3,4], 
                 [4,4], [1,0], [0,1], [3,2], [1,4],
                 [0,3], [1,1], [2,1], [3,1], [4,1]]

--Player's board
playerBoard :: [[Board]]
playerBoard = [[Sea, Sea, Sea, Sea, Sea],
               [Sea, Sea, Sea, Sea, Sea],
               [Sea, Boat, Boat, Boat, Boat],
               [Sea, Sea, Sea, Sea, Sea], 
               [Sea, Sea, Sea, Sea, Sea]]


--updateArr lets your change elements inside a 2d list
updateList :: Int -> (a -> a) -> [a] -> [a]
updateList a f (x:xs) |a == 0      = f x : xs 
                      |otherwise   = x : updateList (a-1) f xs

--Intro message that is displayed before a game starts
introMessage :: IO ()
introMessage = do
                putStrLn ("Welcome to BattleShip (SinglePlayer). You are given one 1x4 ship and will be playing on a 5x5 board against the computer.")
                putStr ("\n")

--Improve readability in console when the program is running
separator :: IO ()
separator = putStr ("\n") >> putStr ("\n") >> putStr ("\n") >> putStr ("\n") >> putStr ("\n") >> putStr ("\n") >> putStr ("\n") >> putStr ("\n") >> putStr ("\n")


--Takes in user input for a set of coordinate then runs the function 'validatePlayerMove'
playerMove :: [[Board]] -> IO [[Board]]
playerMove cBoard = do 
                     putStrLn("Type in a 'x' coordinate [0-4]")
                     xCoord1 <- getCoord >>= analyzeCoord
                     putStrLn("Type in a 'y' coordinate [0-4]")
                     yCoord1 <- getCoord >>= analyzeCoord
                     validatePlayerMove cBoard xCoord1 yCoord1
                     
--Checks if the set of coordinates from 'playerMove' are valid
--if coordinates are valid, attack those coordinates by calling 'analyzeBoard' then return the new board
--otherwise, call the function 'playerMove' 
validatePlayerMove :: [[Board]] -> Int -> Int -> IO [[Board]]
validatePlayerMove cBoard x y |x < 0 || y < 0 || x > 4 || y > 4                                      = do 
                                                                                                        putStrLn ("Coordinate values must range from [0-4]. Try again")
                                                                                                        playerMove cBoard
                              |(cBoard !! y) !! x == Miss || (cBoard !! y) !! x == Hit               = do 
                                                                                                        putStrLn ("You have already hit that coordinate, pick another set of coordinates. \n")
                                                                                                        playerMove cBoard 
                              |otherwise                                                             = do
                                                                                                        newBoard <- analyzeBoard x y cBoard
                                                                                                        return newBoard
--This creates a board where the computer's destroyer is hidden. 
--The purpose of this is to return a board that the players can look at to keep track of their moves
compBoardPlayerView :: [[Board]] -> Int -> Int -> IO [[Board]]
compBoardPlayerView cBoard x y |y == -1                                                          = return cBoard 
                               |y > (-1) && x == (-1)                                            = compBoardPlayerView cBoard 4 (y-1)
                               |(x >= 0 && y >= 0) && ((cBoard !! y) !! x == Boat)               = do
                                                                                                  newBoard <- return (updateList y (updateList x (\ x -> Sea)) cBoard)
                                                                                                  compBoardPlayerView newBoard (x-1) y        
                               |(x >= 0 && y >= 0) && ((cBoard !! y) !! x == Sea)                = compBoardPlayerView cBoard (x-1) y   
                               |(x >= 0 && y >= 0) && ((cBoard !! y) !! x == Hit)                = compBoardPlayerView cBoard (x-1) y 
                               |(x >= 0 && y >= 0) && ((cBoard !! y) !! x == Miss)               = compBoardPlayerView cBoard (x-1) y 

--Attack the player board according to the coordinates from 'compMoveSet'
compMove :: [[Board]] -> Int -> IO [[Board]] --counter will always be 24
compMove pBoard counter |counter == -1                                                                            = return pBoard
                        |(pBoard !! ((compMoveSet !! counter) !! 1)) !! ((compMoveSet !! counter) !! 0) == Sea    = do
                                                                                                                     putStr("\nComputer attacked (")
                                                                                                                     putStr . show $ ((compMoveSet !! counter) !! 0)
                                                                                                                     putStr(", ")
                                                                                                                     putStr . show $ ((compMoveSet !! counter) !! 1)
                                                                                                                     putStr(")\n")
                                                                                                                     xCoord <- return ((compMoveSet !! counter) !! 0)
                                                                                                                     yCoord <- return ((compMoveSet !! counter) !! 1)
                                                                                                                     analyzeBoard xCoord yCoord pBoard
                        |(pBoard !! ((compMoveSet !! counter) !! 1)) !! ((compMoveSet !! counter) !! 0) == Boat   = do
                                                                                                                     putStr("\nComputer attacked (")
                                                                                                                     putStr . show $ ((compMoveSet !! counter) !! 0)
                                                                                                                     putStr(", ")
                                                                                                                     putStr . show $ ((compMoveSet !! counter) !! 1)
                                                                                                                     putStr(")\n")
                                                                                                                     xCoord <- return ((compMoveSet !! counter) !! 0)
                                                                                                                     yCoord <- return ((compMoveSet !! counter) !! 1)
                                                                                                                     analyzeBoard xCoord yCoord pBoard
                        |otherwise                                                                                 = compMove pBoard (counter -1)

--Counts the hits in a board
hitCounter :: [[Board]] -> Int -> Int -> Int -> IO Int
hitCounter board x y hit |y == -1                                                      = return hit
                         |y > (-1) && x == (-1)                                        = hitCounter board 4 (y-1) hit
                         |(x >= 0 && y >= 0) && ((board !! y) !! x == Hit)             = hitCounter board (x-1) y (hit+1)
                         |otherwise                                                    = hitCounter board (x-1) y hit

--This function applies changes to a board then returns it
analyzeBoard x y board |((board !! y) !! x) == Sea        = do 
                                                             newBoard <- return (updateList y (updateList x (\ x -> Miss)) board)
                                                             putStrLn ("MISS!")
                                                             return newBoard
                       |((board !! y) !! x) == Boat       = do 
                                                             newBoard <- return (updateList y (updateList x (\ x -> Hit)) board)
                                                             putStrLn ("HIT!")
                                                             return newBoard
                       |otherwise                         = return board
                                                                               
--gets user input 
getCoord :: IO Char
getCoord = do
            result <- getChar 
            getChar -- get '\n'
            return result
--analyzes the user input and returns an IO Int value
analyzeCoord :: Char -> IO Int
analyzeCoord x |x == '0'     = return 0
               |x == '1'     = return 1
               |x == '2'     = return 2
               |x == '3'     = return 3
               |x == '4'     = return 4
               |otherwise    = return 5

--Prints a board that is readable to the user
printBoard :: [[Board]] -> IO ()
printBoard board = do 
                    putStrLn("    0   1   2   3   4")
                    putStr("0 ")
                    return (board !! 0) >>= print
                    putStr("1 ")
                    return (board !! 1) >>= print
                    putStr("2 ")
                    return (board !! 2) >>= print
                    putStr("3 ")
                    return (board !! 3) >>= print
                    putStr("4 ")
                    return (board !! 4) >>= print

--type 'main' into console to run the program
main :: IO ()
main = mainLoop 0 playerBoard compBoard 0 0

--Recursive loop that makes the entire game run
mainLoop :: Int -> [[Board]] -> [[Board]] -> Int -> Int -> IO ()
mainLoop state pBoard cBoard pHit cHit |state == 0   = do 
                                                        introMessage
                                                        showCBoard <- compBoardPlayerView cBoard 4 4
                                                        putStrLn("Here's your board: ")
                                                        printBoard pBoard
                                                        putStrLn("Here is your opponents board (Their destroyer is not visible to you):")
                                                        putStrLn("(It's purpose is for you to check which coordinates you haven't yet attacked)")
                                                        printBoard showCBoard
                                                        separator
                                                        mainLoop (state + 1) pBoard cBoard pHit cHit
                                       |pHit == 4    = putStrLn ("Computer wins!")
                                       |cHit == 4    = putStrLn ("You win!")
                                       |otherwise    = do 
                                                        putStrLn("Your turn!")
                                                        newCBoard   <- playerMove cBoard
                                                        showCBoard  <- compBoardPlayerView newCBoard 4 4 
                                                        putStrLn("Here is the computer's board: ")
                                                        printBoard showCBoard
                                                        putStrLn("Computer's turn!")
                                                        newPBoard   <- compMove pBoard 24
                                                        putStrLn("Here is your board:")
                                                        printBoard newPBoard
                                                        compHit     <- hitCounter newCBoard 4 4 0
                                                        playerHit   <- hitCounter newPBoard 4 4 0
                                                        separator
                                                        mainLoop (state + 1) newPBoard newCBoard playerHit compHit
