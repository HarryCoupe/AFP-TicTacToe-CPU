G52AFP Coursework 1 - Connect 4 Game
   
Ryan Webb, Harry Coupe
psyrlw@nottingham.ac.uk, psyhc5@nottingham.ac.uk



> import Data.List

----------------------------------------------------------------------

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int


Set depth to 2 when using ghci, can put back to 7 when compiling

> depth = 7

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]


In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

> data Tree a = Node a [Tree a]
>				deriving (Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['0'..]


> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

----------------------------------------------------------------------
Game Board Creation

Create the game board

> gameBoard :: Board
> gameBoard = replicate rows (replicate cols B)



Swaps the current player, can hard code as there are only two options

> swapPlayer :: Player -> Player
> swapPlayer O = X
> swapPlayer X = O

------------------------------------------------------------------------------
Board Manipulation Functions

Transpose the columns into rows so that hasRow can be used to determine if a 
player has won

> colsAsRows :: Board -> [Row]
> colsAsRows = transpose


Gets a single diagonal from the board, starting at (0,0) and goes the end of
the board until (n, n)

> diag :: Board -> (Int, Int) -> Row
> diag b (row, col)
>	| row < length b && col < length (b!!0) = b!!row!!col : diag b (row+1, col+1)
> 	| otherwise = []


Gets a quarter of the diagonals by tailing the board to remove the top rows then
recursivly calls itself until the board is empty

> quartDiag :: Board -> [Row]
> quartDiag [] = [[]]
> quartDiag b = diag b (0, 0) : quartDiag (tail b)


Gets all diagonals using quartDiag. It calls quartDiag using the board, the
board transposed, the board reversed and the board reversed and transposed.

> allDiags :: Board -> [Row]
> allDiags b = quartDiag b ++
>			   quartDiag (transpose b) ++
>			   quartDiag (reverse b) ++
>			   quartDiag (transpose (reverse b))


Takes a board and returns whose turn it is, it flattens the board into a 1D
array then counts the number of players in the list. If there are less xs
than os then it is X's go, otherwise it is O's go

> turn :: Board -> Player
> turn b = if xs < os then X else O
>          where
>              xs = length (filter (==X) (concat b))
>              os = length (filter (==O) (concat b))

------------------------------------------------------------------------------
Winning Functions

Checks if there are four in a row for the specified player, uses countConseq
to check

> hasRow :: Player -> Row -> Bool
> hasRow player row = countConseq 0 player row >= win


Called from hasRow, recursivly scans the row to see find the longest run of
consecutive characters. Will keep adding 1 to the count while the character 
matches, otherwise will start again and return the maximum result

> countConseq :: Int -> Player -> Row -> Int
> countConseq count _ [] = count
> countConseq count player (x:xs)
> 	| x == player = countConseq (count + 1) player xs
>	| otherwise = max count (countConseq 0 player xs)


Checks if there are 4 players connected, uses the hasRow function of all rows,
columns and diagonals.

> hasWon :: Player -> Board -> Bool
> hasWon player b = or([ hasRow player row | row <- b] ++
>					   [ hasRow player row | row <- colsAsRows b ] ++
>					   [ hasRow player row | row <- allDiags b])


Checks if there is a draw, there is a draw if there are no empty spaces left on 
the board. Works by flattening the board into a 1D array, then filtering out the
B players. If the resulting list is empty then there is a draw.

> hasDraw :: Board -> Bool
> hasDraw b = length (filter (==B) (concat b)) == 0

------------------------------------------------------------------------------
Turn Functions

Places the counter in the column. Works by splitting the column into the blanks and
dropped counters. It then takes away one blank then joins the list back up with the
counter placed on top of any other counters.

> dropCounter :: Player -> Row -> Row
> dropCounter player col = (tail (takeWhile (== B) col) ) ++ [player] ++ (dropWhile (== B) col)


Makes a players move

> move :: Player -> Int -> Board -> Board
> move player col b = colsAsRows (moveRec player col (colsAsRows b))
> 	where
> 		moveRec _ _ [[]] = [[]]
> 		moveRec player col (x:xs)
>			| col == 0 = (dropCounter player x) : xs
>			| otherwise = x : (moveRec player (col - 1) xs)


-----------------------------------------------------------------------------------------
Xavier's Brain (MinMax algorithm)


Valid col returns true if a counter can be placed in the specified column. This checks if
there are any B players left in that column.

> validCol :: Board -> Int -> Bool
> validCol b col = length (filter (==B) ((transpose b)!!col)) > 0


Make tree calls compute tree with the inital depth being 0

> makeTree :: Board -> Tree Board
> makeTree b = computeTree X b 0


Compute tree recursivly calls itself until the depth is reached, it uses allMoves to create
all the possible moves that a player could take

> computeTree :: Player -> Board -> Int -> Tree Board
> computeTree p b d = Node b [ computeTree (swapPlayer p) b' (d + 1) | b' <- allMoves b p d ]


All moves returns a list of all the possible boards a player could take, it only computes the
moves if no one has one and the board isn't empty

> allMoves :: Board -> Player -> Int -> [Board]
> allMoves b p d
>	| d == depth = []
> 	| hasWon p b = []
> 	| hasWon (swapPlayer p) b = []
>	| hasDraw b  = []
>	| otherwise  = [ move p col b | col <- [0..(cols - 1)], validCol b col ]


If there node is a leaf, evalate it to get the winning player. If it is an internal node
minMax all its children then, take either the minimum or maximum player from their resulting
states

> minMax :: Tree Board -> Tree (Board, Player)
> minMax (Node b []) = evalBoard b
> minMax (Node b bs)
>	| turn b == X = Node (b, maximum ps) bs'
>	| turn b == O = Node (b, minimum ps) bs'
>		where
>			bs' = map minMax bs
>			ps = [ p | Node (_, p) _ <- bs' ]
 
 
Eval board takes a leaf, then evaluates the board and returns a node with the winner in
 
> evalBoard :: Board -> Tree (Board, Player)
> evalBoard b
> 	| hasWon O b = Node (b, O) []
>	| hasWon X b = Node (b, X) []
>	| otherwise  = Node (b, B) []


The move for Xavier to take, uses MinMax to get the best move in a node, then strips out
the board 

> xavierMove :: Board -> Board
> xavierMove b = head [ b' | Node (b', p') _ <- ts, p' == best ]
>     where
>         Node (_, best) ts = minMax (makeTree b)

----------------------------------------------------------------------------------------
IO and Game Loop

Get an input from the command line

> getInput :: IO Int
> getInput =
>	do
>		line <- getLine
>		return (read line::Int)


The main game loop

> main :: IO ()
> main = mainLoop gameBoard O

> mainLoop :: Board -> Player -> IO ()
> mainLoop b player = do
>	showBoard b
>	if (hasWon (swapPlayer player) b) then
>		do
>			putStr (show (swapPlayer player))
>			putStrLn " has won"
>	else
>		if (hasDraw b) then
>			do
>				putStrLn "Game is a draw"
>		else
>			do
>				putStr (show player)
>				putStrLn " your turn"
>				if (player == X) then
>					do
>						putStrLn "Xavier is thinking..."
>						mainLoop (xavierMove b) O
>				else
>					do
>						putStrLn "Enter column number: "
>						colNum <- getInput
>						mainLoop (move O colNum b) X
