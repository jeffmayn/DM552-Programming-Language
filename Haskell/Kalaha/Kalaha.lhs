---
toc: 1
numbersections: true
geometry: margin=2.5cm
title: Programming Languages (Project 1)
author: Jeff Gyldenbrand (jegyl16)
date: November 17, 2017
abstract: |
    The goal of this project is ... to eat cake
---

\newpage


The Kalaha game with parameters $(n,m)$
====
\begin{code}
module Kalaha where

import Data.List

type PitCount   = Int
type StoneCount = Int
data Kalaha     = Kalaha PitCount StoneCount deriving (Show, Read, Eq)

type KPos       = Int
type KState     = [Int]
type Player     = Bool
\end{code}

The function `startStateImpl`
----
This function is giving with a 'Kalaha'-game: Kalaha,
which takes a pit-count and stone-count as parameters.

By replicating the pit-count and stone-count, and then append a zero, we get
half the list, so we simply add the list to itself to return the initial state
of the Kalaha-game.

\begin{code}
startStateImpl :: Kalaha -> KState
startStateImpl (Kalaha pitC stoneC) = replicate' ++ replicate'
  where
    replicate' = replicate pitC stoneC ++ [0]
\end{code}


The function `movesImpl`
----
In \textbf{movesImpl} which, besides the 'Kalaha'-game parameters, now take as
parameters a player; False or True, and a state for the game. Then returns the
pits which has a positive count of stones of a given player.

This is done by making two guards: one for each player.
If it is player False we simply use the \textbf{findIndices} function which, in our
case, returns all indices greater than zero. This we can do because we split
the game state into a tuble, and define player False to be the first element
in the tuble. We use init to exclude player False's own kalaha pit.

To find same indices only for player True, we define our own \textbf{findIndices'}
function, that recursively searches the other element in the before mentioned
tuble.

\begin{code}
movesImpl :: Kalaha -> Player -> KState -> [KPos]
movesImpl (Kalaha pitC stoneC) p gState
  | p == False = findIndices (>0) (pFalse)
  | p == True = findIndices' (pitC+1) (pTrue)
    where
      pFalse = init(fst(splitAt (pitC+1) gState))
      pTrue = init(snd(splitAt (pitC+1) gState))
      findIndices' _ [] = []
      findIndices' pitC (x:gState)
        | (x>0) = pitC : findIndices' (pitC+1) gState
        | otherwise = findIndices' (pitC+1) gState
\end{code}


The function `valueImpl`
----
In \textbf{valueImpl} which, besides the 'Kalaha'-game parameters, now takes as
parameters the game state. Then returns True's kalaha pit subtracted from
False's kalaha pit as a double.

As in \textbf{movesImpl} this is done by splitting the game state into two elements
in a tuble. Then assigning True's kalaha pit to the last element in the first
element of the tuble, and False's kalaha pit to the last element in the second
element of the tuble. Then we simply subtract the two kalaha pits.

\begin{code}
valueImpl :: Kalaha -> KState -> Double
valueImpl (Kalaha pitC stoneC) gState = fromIntegral (pitTrue - pitFalse)
  where
    pitFalse = last(fst(splitAt(pitC+1) gState))
    pitTrue = last(snd(splitAt(pitC+1) gState))
\end{code}

The function `moveImpl`
----
In \textbf{moveImpl} we take all the same parameters as in \textbf{movesImpl} only now we
want to return the the logic of a move, meaning the next player and the new
state, in a tuble.

To accomplish this, several help functions is developed: \textbf{letsMove},
\textbf{pickUpStones}, \textbf{incVal}, \textbf{emptyPit}, \textbf{emptySpecificPit}, \textbf{lastMove},
\textbf{endCheck}, and \textbf{sweapBoard}.

All these help functions are needed in order to define a ruleset of the move,
and will be explained in more details in the beginning of each. As for the main
function it will read the given parameters, and will call the function
\textbf{pickUpStones} to pick up the stones at giving index, before making the first
move called by the function \textbf{letsMove}

\begin{code}
moveImpl :: Kalaha -> Player -> KState -> KPos -> (Player,KState)
moveImpl (Kalaha pitC stoneC) p gState pitIndex = nextTurn
 where
   nextTurn = letsMove (Kalaha pitC stoneC) p updatedState (pitIndex+1) pitVal
   pitVal = gState!!pitIndex
   updatedState = pickUpStones pitIndex gState
\end{code}

The function `letsMove`
----
\textbf{letsMove} is where the primary action is happening. As parameters it takes a
kalaha game, a player, a newGameState, the current index and its value.
With guards we check for some condition, that will determine the next
legal move:

1. if we have zero stones left in the hand, we check for the other rules.
This is explained in more detailts in the help-function \textbf{endCheck}.

2. As long as we have stones in the hand we move.. and to prevent a move to
reach past the last index in the list, we jump back at the beginning
(index zero), and drop a stone.

3. If player false lands in player trues kalaha, we skip it, and land at player
falses start pit, and drop a stone.

4. if player true lands in player falses kalaha, we skip it, and land at player
trues start pit, and drop a stone.

5. if none of above rules are violated, we move to next pit and drop a stone.

\begin{code}
letsMove (Kalaha pitC stoneC) p gState pIndex stones
 | (stones == 0) = emptyCheck (Kalaha pitC stoneC) p (pIndex-1) gState     -- 1
 | (stones > 0) && (pIndex > pitC*2+1) =                                   -- 2
   letsMove (Kalaha pitC stoneC) p beyond 1 (stones-1)
 | (p == False) && (pIndex == 2*pitC+1) =                                  -- 3
   letsMove (Kalaha pitC stoneC) p skipTrue 1 (stones-1)
 | (p == True) && (pIndex == pitC) =                                       -- 4
   letsMove (Kalaha pitC stoneC) p skipFalse (pIndex+2) (stones-1)
 | otherwise = letsMove (Kalaha pitC stoneC) p nextMove (pIndex+1) (stones-1)--5
 where
  nextMove = incVal pIndex gState 1
  beyond = incVal 0 gState 1
  skipTrue = incVal 0 gState 1
  skipFalse = incVal (pIndex + 1) gState 1
\end{code}

The help-function `pickUpStones`
----
We start a move by picking up all the stones from a chosen pit. We look at the
current game state and changes the giving index's value to zero.

We can't just change it directly, so with a little work-a-round by splitting
the game state into a tuble, appending a zero (which is the replacement for the
actual value) and creating a new list, we get the new game state which is
returned to

\begin{code}
pickUpStones pitIndex gState = newgState
 where
   removeIndexValue = init(fst(splitAt(pitIndex + 1) gState))
   restOfList = snd(splitAt(pitIndex + 1) gState)
   newgState = (removeIndexValue ++ 0 : restOfList)
\end{code}

The help-function `incVal`
----
This function takes as argument a pit index, game state and a value n.
When ever this function is invoked, the value at giving index is incremented
by n, and returns the new game state to the function it is invoked by.

\begin{code}
incVal pitIndex gState n = updateState
 where
   pitValue = gState!!pitIndex
   pitsFalse = init(fst(splitAt(pitIndex + 1) gState))
   pitsTrue = snd(splitAt (pitIndex + 1) gState)
   updateState = (pitsFalse ++ (pitValue + n) : pitsTrue)
\end{code}

The help-function `stealOpposite`
----
In the case where a player drops hes last stone in one of hes own empty pits,
he will steal all the stones from the opposite pit (from the opponent) plus one
(that last stone he dropped in hes own pit). All these stones will go into
the players kalaha.

So based on whether we are player False or True, we invoke function
\textbf{stealFromTrue} or \textbf{stealFromFalse} which invokes the functions \textbf{incVal} and
\textbf{pickUpStones} which updates the state of the game with help from:

1. \textbf{totalStones} which adds the opposit pits stones with one (hence stealing the
stones from the opponent plus our last dropped stone)

2. \textbf{emptyLast} which make sure to empty the pit, where we dropped the last stone.

3. \textbf{emptyOpposite} which empty the opposite pit for stones.

\begin{code}
stealOpposite (Kalaha pitC stoneC) player index gState
 | player == False = stealFromTrue
 | player == True = stealFromFalse
 where
   totalStones = (gState!!(index+((2*pitC)-(index*2)))) + 1
   emptyLast = pickUpStones index gState
   emptyOpposite = pickUpStones (index+((2*pitC)-(index*2))) emptyLast

   stealFromTrue = incVal pitC emptyOpposite totalStones
   stealFromFalse = incVal ((pitC*2)+1) emptyOpposite totalStones
\end{code}

The help-function `emptySpecificPit`
----
This function is called by sweapBoard to empty a players remaining stones
one pit at a time, and add it to the this players kalaha.

\begin{code}
emptySpecificPit (Kalaha pitC stoneC) player index gState
 | player == False = nextFalse
 | otherwise = nextTrue
 where
  stones = gState!!index
  newState = pickUpStones index gState
  nextFalse = incVal pitC newState stones
  nextTrue = incVal (pitC*2+1) newState stones
\end{code}

The help-function `lastMove`
----
In this function we check for the other rules:

1. if player False lands in an empty pit
2. if player True lands in an empty pit
3. if player False lands in player own kalaha
4. if player True lands in player own kalaha
5. nothing happens, and returns state and its next players turn

\begin{code}
lastMove (Kalaha pitC stoneC) p pitIndex gState
 | (p == False) && (gState!!pitIndex == 1) && (pitIndex < pitC) = case1
 | (p == True) && (gState!!pitIndex == 1) && (pitIndex > pitC) && (pitIndex < ((pitC*2)+1)) = case2
 | (p == False) && (pitIndex == pitC) = case3
 | (p == True) && (pitIndex == pitC*2+1) = case4
 | otherwise = (not p, gState)
  where
  emptyFalseOpposite = (stealOpposite (Kalaha pitC stoneC) False pitIndex gState)
  emptyTrueOpposite = (stealOpposite (Kalaha pitC stoneC) True pitIndex gState)
  case1 = (True, emptyFalseOpposite)
  case2 = (False, emptyTrueOpposite)
  case3 = (False, gState)
  case4 = (True, gState)
\end{code}

The help-function `emptyCheck`
----
In this function we check if the pits of a player are empty.
In that case the oppenent will collect all hes own stones to hes kalaha,
and the game will end.

We do this by looking for the case where we get the value 'Nothing' from
finding indices greater than zero in both player true and false's pits.
So if a players pits are empty, we invoke the `sweapBoard` function for the
opposite player which then collects hes own stones.

\begin{code}
emptyCheck (Kalaha pitCount stoneCount) player pitIndex gameState
 | (findIndex (>0) (fst(splitAt pitCount gState)) == Nothing) = (swap, tCollect)
 | (findIndex (>0) (init(snd(splitAt (pitCount+1) gState))) == Nothing) = (swap, fCollect)
 | otherwise = lastMove (Kalaha pitCount stoneCount) player pitIndex gameState
  where
   swap = not player
   gState = snd(lastMove (Kalaha pitCount stoneCount) player pitIndex gameState)
   listOfindexes = findIndices (>0) gState
   tCollect = sweapBoard (Kalaha pitCount stoneCount) True gState listOfindexes ((length listOfindexes) -1)
   fCollect = sweapBoard (Kalaha pitCount stoneCount) False gState listOfindexes ((length listOfindexes) -1)
\end{code}

The help-function `sweapBoard`
----
So the sweapBoard function is invoked by emptyCheck, and recursively extracts
the values in the pits into the correct players kalaha, by invoking the
function emptySpecificPit. Ultimately it will return the final game state.

\begin{code}
sweapBoard (Kalaha pitC stoneC) p gState indexList pitIndex
 | (pitIndex<0) = gState
 | (extractVal == pitC) = sweapBoard (Kalaha pitC stoneC) p gState indexList (pitIndex-1)
 | (extractVal == pitC*2+1) = sweapBoard (Kalaha pitC stoneC) p gState indexList (pitIndex-1)
 | otherwise = sweapBoard (Kalaha pitC stoneC) p execute indexList (pitIndex-1)
 where
    extractVal = indexList!!pitIndex
    execute = emptySpecificPit (Kalaha pitC stoneC) p extractVal gState
\end{code}

The function `showGameImpl`
----
For a given game and state we want a pretty output of the game state.
For this we define tree lines, one for player true, one for the two kalahas,
and one for player false. We use unlines to join the three lines with newlines
appended, and also we map unwords to the three lines to seperate them by spaces.

With some help function \textbf{pad} and \textbf{part}, we align numbers perfectly. And by
defining some empty space from the length of our list, we push line1 and line2 to their
proper place.

\begin{code}
showGameImpl :: Kalaha -> KState -> String
showGameImpl g@(Kalaha pitC stoneC) gameState =
  unlines $ map unwords [line1, line2, line3]
  where
    maxLen = length $ show $ 2*pitC*stoneC
    empty = replicate maxLen ' '

    gameState' = map (pad maxLen) $ map show gameState
    (line1, line3) = (empty : (reverse $ part(pitC+1, 2*pitC+1) gameState'), empty : (part(0,pitC) gameState'))
    line2 = last gameState' : (replicate pitC empty ++ [gameState'!!pitC])

pad :: Int -> String -> String
pad pitC s = replicate (pitC - length s) ' ' ++ s

part :: (Int, Int) -> [a] -> [a]
part (x,y) l = drop x $ take y l
\end{code}

Trees
====
\begin{code}
data Tree m v  = Node v [(m,Tree m v)] deriving (Eq, Show)
\end{code}

Test tree
----
\begin{code}
testTree :: Tree Int Int
testTree = Node 3 [(0, Node 4
    [(0, Node 5 []),(1, Node 6 []), (2, Node 7 [])])
    ,(1, Node 9
      [(0, Node 10[])])
    ]
\end{code}


The function `takeTree`
----
This function takes as parameters a depth, where we want to cut the children of
the tree, a kalaha-game (number of pits and stones), a player and a game state.

Base case zero is the empty list, otherwise we recursively map the tree to the
list.

\begin{code}
takeTree :: Int -> Tree m v -> Tree m v
takeTree n (Node v list)
 | (n==0) = (Node v [])
 | otherwise = (Node v (map tree' list))
  where
   tree' (m,t) = (m, takeTree (n-1) t)
\end{code}


The Minimax algorithm
====
\begin{code}
data Game s m = Game {
    startState    :: s,
    showGame      :: s -> String,
    move          :: Player -> s -> m -> (Player,s),
    moves         :: Player -> s -> [m],
    value         :: Player -> s -> Double}

kalahaGame :: Kalaha -> Game KState KPos
kalahaGame k = Game {
    startState = startStateImpl k,
    showGame   = showGameImpl k,
    move       = moveImpl k,
    moves      = movesImpl k,
    value      = const (valueImpl k)}

startTree :: Game s m -> Player -> Tree m (Player,Double)
startTree g p = tree g (p, startState g)

\end{code}
The function `tree`
----


\begin{code}
tree :: Game s m -> (Player, s) -> Tree m (Player, Double)
tree game (player, state) = Node (player, treeValue) treeMove
    where
    treeValue = value game player state
    treeMoves = moves game player state
    treeMove = [(m, tree game (move game player state m)) | m <- treeMoves]
\end{code}

The function `minimax`
----


\begin{code}
minimax   :: Tree m (Player, Double) -> (Maybe m, Double)
minimax (Node (_,treeValue) []) =  (Nothing, treeValue)
minimax (Node (p, v) ch)
  | p == True = maximumSnd [ (Just path, snd $ minimax subtree) | (path, subtree) <- ch]
  | otherwise = minimumSnd [ (Just path, snd $ minimax subtree) | (path, subtree) <- ch]

maxSnd :: (a,Double) -> (a, Double) -> (a, Double)
maxSnd a@(_,v1) b@(_,v2) | v1 >= v2 = a| otherwise = b

minSnd :: (a, Double) -> (a, Double) -> (a, Double)
minSnd a@(_,v1) b@(_,v2) | v1 <= v2 = a| otherwise = b

maximumSnd :: [(a, Double)] -> (a, Double)
maximumSnd [] = error "undefined for empty list"
maximumSnd (x:xs) = foldl maxSnd x xs

minimumSnd :: [(a, Double)] -> (a, Double)
minimumSnd [] = error "undefined for empty list"
minimumSnd (x:xs) = foldl minSnd x xs
\end{code}

The function `minimaxAlphaBeta`
----
\begin{code}
type AlphaBeta = (Double,Double)

minimaxAlphaBeta :: AlphaBeta -> Tree m (Player, Double) -> (Maybe m, Double)
minimaxAlphaBeta = undefined
\end{code}

Testing and sample executions
====
We test the function \textbf{startStateImpl} for tree cases: one for a kalaha
game with six pits and six stones in each, one for six pits and four stones,
and finally one for four pits and four stones in each. See picture X for result.

\includegraphics[width=0.7\textwidth]{testing/startStateImpl/startStateImpl.png}

As seen in picture X, we test the function \textbf{valueImpl} with two
different cases: one for a kalaha game with six pits, and one with only two
pits. In both cases we see that player True's kalaha is subtracted from player
False's kalaha, with the output 7.0 and -1.0

\includegraphics[width=0.7\textwidth]{testing/valueImpl/valueImpl.png}

As seen in picture X, we test the function \textbf{movesImpl} with three
different cases: one for player False, where we return hes pits which has
positive elements, we test the same case only for player True, and one case where player
False has zero positive elements, hence returning an empty list.

\includegraphics[width=0.7\textwidth]{testing/movesImpl/movesImpl.png}

For the function \textbf{moveImpl} we look at three cases, one for a regular
move for player True, as shown in picture X, one for the case where all player
False's pits become empty, so player True collects the remaining
stones to hes kalaha, as shown in picture X, and lastly, the case where player
False lands in one of hes own empty pits, and steals the stones from the
opposite side, as shown in picture X.

\includegraphics[width=0.7\textwidth]{testing/moveImpl/moveImpl_regular_move.png}

\includegraphics[width=0.7\textwidth]{testing/moveImpl/moveImpl_falses_becomes_empty.png}

\includegraphics[width=0.7\textwidth]{testing/moveImpl/moveImpl_steals_opposite.png}

In the function \textbf{showGameImpl} we test for two cases to make sure the
output is pretty and aligned, so case one is for a kalaha with six pits, and
second case for a kalaha with 4 pits. Both cases with number of one and two
digits.

\includegraphics[width=0.7\textwidth]{testing/showGameImpl/showGameImpl.png}

In the \textbf{treeImpl} we get the complete tree for all possible moves for
a given player and kalaha state. We only test for a tree with two pits and two
stones in each, as shown in picture X, otherwise the output tree would be too
big.

\includegraphics[width=0.9\textwidth]{testing/tree/treeImpl_False_2_2.png}

In the function \textbf{takeTree} we test with the same tree as in \textbf{treeImpl}
only now we cut it off in depth two.

\includegraphics[width=0.9\textwidth]{testing/tree/takeTree_of_tree_False_2_2.png}

Just so see the output of depth zero and one, we cut the hard-coded test tree
at zero with the result of an empty tree, and one, as shown in picture X.

\includegraphics[width=0.4\textwidth]{testing/takeTree/takeTree_testtree.png}

minimax

\includegraphics[width=1.0\textwidth]{testing/minimax/minimax1.png}

All test

\includegraphics[width=0.7\textwidth]{testing/all/all.png}

minimax test

\includegraphics[width=0.7\textwidth]{testing/all/minimax_all.png}
