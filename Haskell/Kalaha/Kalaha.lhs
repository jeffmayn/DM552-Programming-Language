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

\begin{code}
startStateImpl :: Kalaha -> KState
startStateImpl (Kalaha n m) = replicate n m ++ [0]
                           ++ replicate n m ++ [0]
\end{code}


The function `movesImpl`
----

\begin{code}
movesImpl :: Kalaha -> Player -> KState -> [KPos]
movesImpl (Kalaha n m) p s
  | p == False = findIndices (>0) (pFalse)
  | p == True = findIndices' (n+1) (pTrue)
    where
      pFalse = init(fst(splitAt (n+1) s))
      pTrue = init(snd(splitAt (n+1) s))
      findIndices' _ [] = []
      findIndices' n (x:s)
        | (x>0) = n : findIndices' (n+1) s
        | otherwise = findIndices' (n+1) s
\end{code}


The function `valueImpl`
----


\begin{code}
valueImpl :: Kalaha -> KState -> Double
valueImpl (Kalaha n m) s = fromIntegral (pitTrue - pitFalse)
  where
    pitTrue = last(fst(splitAt(n+1) s))
    pitFalse = last(snd(splitAt(n+1) s))
\end{code}

The function `moveImpl`
----


\begin{code}
moveImpl :: Kalaha -> Player -> KState -> KPos -> (Player,KState)
moveImpl (Kalaha n m) p s xs = bushit
 where
   bushit = recursiveFunction (Kalaha n m) p k (xs+1) x
   x = s!!xs
   k = firstMove xs s

\end{code}




\begin{code}
----------------------------------
firstMove q s = k
 where
   hej = splitAt (q+1) s
   first = init(fst(hej))
   second = snd(hej)

   k = (first ++ 0 : second)
----------------------------------
\end{code}

\begin{code}
----------------------------------
incrementMove q s l = k
 where
   h = s!!q
   hej = splitAt (q+1) s
   first = init(fst(hej))
   second = snd(hej)

   k = (first ++ (h+l) : second)
----------------------------------
\end{code}


\begin{code}
----------------------------------
emptyPit (Kalaha n m) p q s
 | p == False = emptyF'
 | p == True = emptyT'
 where
   -- Modsat pit + 1
   op = (s!!(q+((2*n)-(q*2)))) + 1
   -- tømmer index for tomt slut pit
   k2 = firstMove q s
   -- tømmer modsat pit
   k3 = firstMove (q+((2*n)-(q*2))) k2
   -- false
   emptyF' = incrementMove n k3 op
   -- true
   emptyT' = incrementMove ((n*2)+1) k3 op
----------------------------------
\end{code}


\begin{code}
----------------------------------
emptyAll (Kalaha n m) p s ind c
 | (c<0) = s
 | otherwise = emptyAll (Kalaha n m) p k ind (c-1)
 where
    o = ind!!c
    k = emptySpecificPit (Kalaha n m) p o s
----------------------------------
\end{code}

\begin{code}
----------------------------------
emptySpecificPit (Kalaha n m) p o s
 | p == False = allEmptyFalse'
 | otherwise = allEmptyTrue'
 where
  val = s!!o
  k = firstMove o s
  allEmptyFalse' = incrementMove n k val
  allEmptyTrue' = incrementMove (n*2+1) k val


----------------------------------
\end{code}

\begin{code}
----------------------------------
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


---------------------------------------
\end{code}


\begin{code}
----------------------------------
endCheck (Kalaha n m) p q s
 | (findIndex (>0) (fst(splitAt n k)) == Nothing) = (not p, lastNF')
 | (findIndex (>0) (init(snd(splitAt (n+1) k))) == Nothing) = (not p, lastNT')
 | otherwise = lastMove (Kalaha n m) p q s
  where
   k = snd(lastMove (Kalaha n m) p q s)
   ind = (findIndices (>0) (fst(splitAt n k))) ++(findIndices (>0) (init(snd(splitAt (n+1) k))))
   lenL = ((length ind) -1)
   lastNF' = emptyAll (Kalaha n m) True k ind lenL
   lastNT' = emptyAll (Kalaha n m) False k ind lenL
----------------------------------
\end{code}


\begin{code}
---------------------------------------
recursiveFunction (Kalaha n m) p s q x
 | (x==0) = endCheck (Kalaha n m) p (q-1) s
 | (x>0) && (q > n*2+1) = recursiveFunction (Kalaha n m) p kz 1 (x-1)
 | (p == False) && (q == 2*n+1) = recursiveFunction (Kalaha n m) p k0 1 (x-1)
 | (p == True) && (q == n) = recursiveFunction (Kalaha n m) p kz2 (q+2) (x-1)
 | otherwise = recursiveFunction (Kalaha n m) p k (q+1) (x-1)
 where
  k = incrementMove q s 1
  k0 = incrementMove (0) s 1
  kz = incrementMove (0) s 1
  kz2 = incrementMove (q+1) s 1
---------------------------------------
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
tree      :: Game s m -> (Player, s) -> Tree m (Player, Double)
tree (Game startState showGame move moves value) (p,s) = undefined
\end{code}

tree (kalahaGame (Kalaha 6 6)) (False, [6,6,6,6,6,6,0,6,6,6,6,6,6,0])


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
