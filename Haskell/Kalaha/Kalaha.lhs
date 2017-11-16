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
In `movesImpl` which, besides the 'Kalaha'-game parameters, now take as
parameters a player; False or True, and a state for the game. Then returns the
pits which has a positive count of stones of a given player.

This is done by making two guards: one for each player.
If it is player False we simply use the 'findIndices' function which, in our
case, returns all indices greater than zero. This we can do because we split
the game state into a tuble, and define player False to be the first element
in the tuble. We use init to exclude player False's own kalaha pit.

To find same indices only for player True, we define our own findIndices'
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
In `valueImpl` which, besides the 'Kalaha'-game parameters, now takes as
parameters the game state. Then returns True's kalaha pit subtracted from
False's kalaha pit as a double.

As in `movesImpl` this is done by splitting the game state into two elements
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
In `moveImpl` we take all the same parameters as in `movesImpl` only now we
want to return the the logic of a move, meaning the next player and the new
state, in a tuble.

To accomplish this, several help functions is developed: `letsMove`,
`pickUpStones`, `incrementMove`, `emptyPit`, `emptySpecificPit`, `lastMove`,
`endCheck`, `sweapBoard`.

All these help functions are needed in order to define a ruleset of the move,
and will be explained in more details in the beginning of each. As for the main
function it will read the given parameters, and will call the function
`pickUpStones` to pick up the stones at giving index, before making the first
move called by the function `letsMove`

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
`letsMove` is where the primary action is happening. As parameters it takes a
kalaha game, a player, a newGameState, the current index and its value.
With guards we check for some condition, that will determine the next
legal move:

1. if we have zero stones left in the hand, we check for the other rules.
This is explained in more detailts in the help-function `endCheck`.)

2. As long as we have stone in the hand .. and
To prevent a move to reach past the last index in the list, we jump back at
the beginning (index zero), and drop a stone.

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
`stealFromTrue` or `stealFromFalse.` which invoke the functions `incVal` and
`pickUpStones` which updates the state of the game with help from:

1. totalStones which adds the opposit pits stones with one (hence stealing the
stones from the opponent plus our last dropped stone)

2. emptyLast which make sure to empty the pit, where we dropped the last stone.

3. emptyOpposite which empty the opposite pit for stones.

\begin{code}
stealOpposite (Kalaha pitC _) player index gState
 | player == False = stealFromTrue
 | player == True = stealFromFalse
 where
   stealFromTrue = incVal pitC emptyOpposite totalStones
   stealFromFalse = incVal ((pitC*2)+1) emptyOpposite totalStones
   totalStones = (gState!!(index+((2*pitC)-(index*2)))) + 1
   emptyLast = pickUpStones index gState
   emptyOpposite = pickUpStones (index+((2*pitC)-(index*2))) emptyLast
\end{code}

The help-function `emptySpecificPit`
----
This function is called by sweapBoard to empty one index at a time, and add
it to the right kalaha.

\begin{code}
emptySpecificPit (Kalaha pitC _) player o gState
 | player == False = allEmptyFalse'
 | otherwise = allEmptyTrue'
 where
  val = gState!!o
  k = pickUpStones o gState
  allEmptyFalse' = incVal pitC k val
  allEmptyTrue' = incVal (pitC*2+1) k val
\end{code}

The help-function `lastMove`
----
In this function we check for the other rules:

1. if player false lands in an empty pit
2. if player true lands in an empty pit
3. if player false lands in player own kalaha
4. if player true lands in player own kalaha
5. nothing happens, and returns state and its next players turn
----
\begin{code}
lastMove (Kalaha pitC stoneC) p pitIndex gState
 | (p == False) && (gState!!pitIndex == 1) && (pitIndex < pitC) = lastEF'
 | (p == True) && (gState!!pitIndex == 1) && (pitIndex > pitC) && (pitIndex < ((pitC*2)+1)) = lastET'
 | (p == False) && (pitIndex == pitC) = lastKF'
 | (p == True) && (pitIndex == pitC*2+1) = lastKT'
 | otherwise = (not p, gState)
  where
  emptyFalseOpposite = (stealOpposite (Kalaha pitC stoneC) False pitIndex gState)
  emptyTrueOpposite = (stealOpposite (Kalaha pitC stoneC) True pitIndex gState)
  -- last empty pit player false
  lastEF' = (True, emptyFalseOpposite)
  -- last empty pit player true
  lastET' = (False, emptyTrueOpposite)
  -- last kalaha false
  lastKF' = (False, gState)
  -- last kalaha true
  lastKT' = (True, gState)
\end{code}

The help-function `emptyCheck`
----
In this help-function we check if the pits of a player are empty.
In that case the oppenent will collect all hes own stones to hes kalaha,
and the game will end.

We do this by looking for the case where we get the value 'Nothing' from
finding indices greater than zero in both player true and false's pits.
So if a players pits are empty, we invoke the `sweapBoard` function for the
opposite player which then collects hes own stones.

\begin{code}
emptyCheck (Kalaha pitC stoneC) p pitIndex gState
 | (findIndex (>0) (fst(splitAt pitC newState)) == Nothing) = swap'
 | (findIndex (>0) (init(snd(splitAt (pitC+1) newState))) == Nothing) = swap'
 | otherwise = lastMove (Kalaha pitC stoneC) p pitIndex gState
  where
    swap = not p
    swap' = (swap, collect)
    newState = snd(lastMove (Kalaha pitC stoneC) p pitIndex gState)
    collect
     | p == False = sweapBoard (Kalaha pitC stoneC) True newState indexList l
     | p == True = sweapBoard (Kalaha pitC stoneC) False newState indexList l
     where
      l = ((length indexList) -1)
      indexList = findIndices (>0) newState
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

\begin{code}
showGameImpl :: Kalaha -> KState -> String
showGameImpl g@(Kalaha pitCount stoneCount) gameState =
  unlines $ map unwords [line1, line2, line3]
  where
    maxLen = length $ show $ 2*pitCount*stoneCount
    empty = replicate maxLen ' '

    gameState' = map (pad maxLen) $ map show gameState
    (line1, line3) = (empty : (reverse $ part(pitCount+1, 2*pitCount+1) gameState'), empty : (part(0,pitCount) gameState'))
    line2 = last gameState' : (replicate pitCount empty ++ [gameState'!!pitCount])

pad :: Int -> String -> String
pad pitCount s = replicate (pitCount - length s) ' ' ++ s

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

\begin{code}
takeTree :: Int -> Tree m v -> Tree m v
takeTree n (Node v list)
 | (n==0) = (Node v [])
 | otherwise = (Node v (map theTree list))
  where
   theTree (m,t) = (m, takeTree (n-1) t)
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

The function `minimaxAlphaBeta`
----
\begin{code}
type AlphaBeta = (Double,Double)

maxValue :: (Double,Double) -> (Maybe m, Double) ->  [(m, Tree m (Player, Double))] -> (Maybe m, Double)
maxValue (a,b) (m0,v0) [] = (m0,v0)
maxValue (a,b) (m0,v0) ((m1,c) : ch)
 | (v >= b) = (Just m1, v)
 | otherwise = maxValue (maxAlpha,b) (m,v) (ch)
  where
   v1       = snd $ minimaxAlphaBeta (a,b) c
   (m,v)    = maxSnd (m0,v0) (Just m1,v1)
   maxAlpha = (max a v)



minValue :: (Double,Double) -> (Maybe m, Double) ->  [(m, Tree m (Player, Double))] -> (Maybe m, Double)
minValue (a,b) (m0,v0) [] = (m0,v0)
minValue (a,b) (m0,v0) ((m1,c) : ch)
 | (v <= a) = (Just m1, v)
 | otherwise = minValue (a,minBeta) (m,v) (ch)
  where
   v1      = snd $ minimaxAlphaBeta (a,b) c
   (m,v)   = minSnd (m0,v0) (Just m1,v1)
   minBeta = (min b v)



minimaxAlphaBeta :: AlphaBeta -> Tree m (Player, Double) -> (Maybe m, Double)
minimaxAlphaBeta (a,b) (Node (_,treeValue) []) =  (Nothing, treeValue)
minimaxAlphaBeta (a,b) (Node (p, v) ch)
 | p == True       = maxValue (a,b) (Nothing, (-1/0)) (ch)
 | otherwise       = minValue (a,b) (Nothing, (1/0)) (ch)

\end{code}

Testing and sample executions
====
