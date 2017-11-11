s---
toc: 1
numbersections: true
geometry: margin=2.5cm
title: Programming Languages (Project 1)
author: Jeff Gyldenbrand (jegyl16)
date: November 13, 2017
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
We are giving the declaretion `startStateImpl` with a 'Kalaha'-game: Kalaha,
which takes a pit-count and stone-count as parameters and returns the initial state
of the game in a list: KState.

By replicating the pit-count and stone-count, and then append a zero, we get half the
list, so we simply do this twice to return the complete state.

\begin{code}
startStateImpl :: Kalaha -> KState
startStateImpl (Kalaha pitCount stoneCount) =
  replicate pitCount stoneCount ++ [0]
  ++ replicate pitCount stoneCount ++ [0]
\end{code}


The function `movesImpl`
----
In `movesImpl` which, besides the 'Kalaha'-game parameters, now take as parameters a player; False or True,
and a state for the game. Then returns the pits of given player's pits with has a positive number of
elements.

This is done by making two guards: one for each player.
If it is player False we simply use the 'findIndices' function which, in our case, returns all
indices greater than zero. This we can do because we split the game state into a tuble, and
define player False to be the first element in the tuble. We use init to exclude player False's
own kalaha pit.

To find same indices only for player True, we define our own findIndices' functions, that recursively
searches the other element in the before mentioned tuble.

\begin{code}
movesImpl :: Kalaha -> Player -> KState -> [KPos]
movesImpl (Kalaha pitCount stoneCount) player gameState
  | player == False = findIndices (>0) (pFalse)
  | player == True = findIndices' (pitCount+1) (pTrue)
    where
      pFalse = init(fst(splitAt (pitCount+1) gameState))
      pTrue = init(snd(splitAt (pitCount+1) gameState))
      findIndices' _ [] = []
      findIndices' pitCount (x:gameState)
        | (x>0) = pitCount : findIndices' (pitCount+1) gameState
        | otherwise = findIndices' (pitCount+1) gameState
\end{code}


The function `valueImpl`
----
In `valueImpl` which, besides the 'Kalaha'-game parameters, now takes as parameters the game state.
Then returns True's kalaha pit subtracted from False's kalaha pit as the double datatype.

As in `movesImpl` this is done by splitting the game state into two elements in a tuble.
Then assigning True's kal\begin{code}
movesImpl :: Kalaha -> Player -> KState -> [KPos]
movesImpl (Kalaha n m) p s = map (+ (if p then n + 1 else 0) )
                           $ findIndices (>0) $ init $ f $ splitAt (n + 1) s
                           where
                             f = if p then fst else snd
\end{code}aha pit to the last element in the first element of the tuble,
and False's kalaha pit to the last element in the second element of the tuble.
Then we simply subtract the two kalaha pits.

\begin{code}
valueImpl :: Kalaha -> KState -> Double
valueImpl (Kalaha pitCount stoneCount) gameState = fromIntegral (pitTrue - pitFalse)
  where
    pitTrue = last(fst(splitAt(pitCount+1) gameState))
    pitFalse = last(snd(splitAt(pitCount+1) gameState))
\end{code}

The function `moveImpl`
----

\begin{code}
moveImpl :: Kalaha -> Player -> KState -> KPos -> (Player,KState)
moveImpl (Kalaha pitCount stoneCount) player gameState pitIndex = nextTurn
 where
   nextTurn = letsMove (Kalaha pitCount stoneCount) player newGameState (pitIndex+1) pitValue
   pitValue = gameState!!pitIndex
   newGameState = initMove pitIndex gameState
\end{code}

The function `letsMove`
----

\begin{code}
letsMove (Kalaha pitCount stoneCount) player gameState pitIndex pitValue
-- case 1: if giving player lands in own pit and its zero:
 | (pitValue == 0) = endCheck (Kalaha pitCount stoneCount) player (pitIndex-1) gameState
-- case 3: if current pit value is greater than zero, but index is greater than actual board
 | (pitValue > 0) && (pitIndex > pitCount*2+1) = letsMove (Kalaha pitCount stoneCount) player reachedBeyoundEndPit 1 (pitValue-1)
-- case 4: if giving player
 | (player == False) && (pitIndex == 2*pitCount+1) = letsMove (Kalaha pitCount stoneCount) player k0 1 (pitValue-1)
-- case 5: if it's player True
 | (player == True) && (pitIndex == pitCount) = letsMove (Kalaha pitCount stoneCount) player kz2 (pitIndex+2) (pitValue-1)
-- case 6: if giving player
 | otherwise = letsMove (Kalaha pitCount stoneCount) player k (pitIndex+1) (pitValue-1)
 where
   -- add one stone to current pit
  k = incrementMove pitIndex gameState 1
   -- add one stone to first pit
  k0 = incrementMove 0 gameState 1
   -- add one stone to first pit
  reachedBeyoundEndPit = incrementMove 0 gameState 1
   -- add one stone to next pit
  kz2 = incrementMove (pitIndex + 1) gameState 1
\end{code}

The help-function `initMove`
----
This function assist the main-function `moveImpl`, and ...

\begin{code}
initMove pitIndex gameState = newGameState
 where
   pitsFalse = init(fst(splitAt(pitIndex + 1) gameState))
   pitsTrue = snd(splitAt(pitIndex + 1) gameState)
   newGameState = (pitsFalse ++ 0 : pitsTrue)
\end{code}

The help-function `incrementMove`
----
\begin{code}
incrementMove pitIndex gameState incrementValue = newGameState
 where
   pitValue = gameState!!pitIndex
   pitsFalse = init(fst(splitAt(pitIndex + 1) gameState))
   pitsTrue = snd(splitAt (pitIndex + 1) gameState)
   newGameState = (pitsFalse ++ (pitValue + incrementValue) : pitsTrue)
\end{code}

The help-function `emptyPit`
----

\begin{code}
emptyPit (Kalaha pitCount stoneCount) p q s
 | p == False = emptyF'
 | p == True = emptyT'
 where
   -- Modsat pit + 1
   op = (s!!(q+((2*pitCount)-(q*2)))) + 1
   -- tømmer index for tomt slut pit
   k2 = initMove q s
   -- tømmer modsat pit
   k3 = initMove (q+((2*pitCount)-(q*2))) k2
   -- false
   emptyF' = incrementMove pitCount k3 op
   -- true
   emptyT' = incrementMove ((pitCount*2)+1) k3 op
\end{code}

The help-function `emptyAll`
----

\begin{code}
emptyAll (Kalaha n m) p s ind c
 | (c<0) = s
 | (o == n) = emptyAll (Kalaha n m) p s ind (c-1)
 | (o == n*2+1) = emptyAll (Kalaha n m) p s ind (c-1)
 | otherwise = emptyAll (Kalaha n m) p k ind (c-1)
 where
    o = ind!!c
    k = emptySpecificPit (Kalaha n m) p o s
\end{code}

The help-function `emptySpecificPit`
----
\begin{code}
emptySpecificPit (Kalaha n m) p o s
 | p == False = allEmptyFalse'
 | otherwise = allEmptyTrue'
 where
  val = s!!o
  k = initMove o s
  allEmptyFalse' = incrementMove n k val
  allEmptyTrue' = incrementMove (n*2+1) k val
\end{code}

The help-function `lastMove`
----
\begin{code}
lastMove (Kalaha n m) p q s
 | (p == False) && (s!!q == 1) && ( q < n) = lastEF'                      -- empty pit player false
 | (p == True) && (s!!q == 1) && (q > n) && (q < ((n*2)+1)) = lastET'     --
 | (p == False) && (q == n) = lastKF'                                     --
 | (p == True) && (q == n*2+1) = lastKT'                                  --
 | otherwise = (not p, s)                                                 --
  where
  bo = (emptyPit (Kalaha n m) False q s)
  biver = (emptyPit (Kalaha n m) True q s)
  lastEF' = (True, bo)
  lastET' = (False, biver)
  lastKF' = (False, s)
  lastKT' = (True, s)
\end{code}

The help-function `endCheck`
----
\begin{code}
endCheck (Kalaha pitCount stoneCount) player pitIndex gameState
--  hvis index > 0
 | (findIndex (>0) (fst(splitAt pitCount k)) == Nothing) = (not player, lastNF')
 | (findIndex (>0) (init(snd(splitAt (pitCount+1) k))) == Nothing) = (not player, lastNT')
 | otherwise = lastMove (Kalaha pitCount stoneCount) player pitIndex gameState
  where
   k = snd(lastMove (Kalaha pitCount stoneCount) player pitIndex gameState)
   ind = findIndices (>0) k
   lenL = ((length ind) -1)
   lastNF' = emptyAll (Kalaha pitCount stoneCount) True k ind lenL
   lastNT' = emptyAll (Kalaha pitCount stoneCount) False k ind lenL
\end{code}

The function `showGameImpl`
----

\begin{code}
showGameImpl :: Kalaha -> KState -> String
showGameImpl g@(Kalaha n m) xs = total
   where
     maxLen = length(show(2*n*m))

     newLine = "\n"
     pitSpace = unwords $(replicate (n+3) " ")
     emptySpace = unwords $(replicate 3 " ")

     -- players
     pFalse = init(fst(splitAt (n+1) xs))
     pTrue = init(snd(splitAt (n+1) xs))

     -- Kalaha pits
     pitTrue = drop n (fst(splitAt (n+1) xs))
     pitFalse = drop n (snd(splitAt (n+1) xs))

     -- print out: Player True's pits
     trueOut = unwords $(map show(reverse(pTrue)))
     -- print out: Player False's pit
     falseOut = unwords $(map show(pFalse))

     -- print out: Player True's pit
     truePitOut = unwords $(map show(pitTrue))
     -- print out: Player False's pit
     falsePitOut = unwords $(map show(pitFalse))

     -- print out: complete kalaha
     total = emptySpace
           ++ trueOut
           ++ newLine
           ++ falsePitOut
           ++ pitSpace
           ++ truePitOut
           ++ newLine
           ++ emptySpace
           ++ falseOut

\end{code}


Trees
====

\begin{code}
data Tree m v  = Node v [(m,Tree m v)] deriving (Eq, Show)
\end{code}

Test tree hjælpefunktion
----
\begin{code}
testTree :: Tree Int Int
testTree = Node 3 [(0, Node 4[(0, Node 5 []),(1, Node 6 []), (2, Node 7 [])]), (1, Node 9[(0, Node 10[])])]
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
minimax = undefined
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
