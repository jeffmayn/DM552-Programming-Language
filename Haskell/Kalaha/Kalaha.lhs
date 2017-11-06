---
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
        | (x > 0)   = n : findIndices' (n+1) s
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
moveImpl (Kalaha n m) p s = undefined
--  | pFalse =
--  | pTrue =

\end{code}

The function `showGameImpl`
----


\begin{code}
showGameImpl :: Kalaha -> KState -> String
showGameImpl g@(Kalaha n m) xs = undefined
\end{code}


Trees
====

\begin{code}
data Tree m v  = Node v [(m,Tree m v)] deriving (Eq, Show)
\end{code}

The function `takeTree`
----

\begin{code}
takeTree :: Int -> Tree m v -> Tree m v
takeTree = undefined
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
tree = undefined

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
