{- Function that returns string "eeny" for even inputs, and "meeny" for odd inputs-}
eeny :: Integer -> String
eeny x = if mod x 2 == 0
  then "eeny"
  else "meeny"

{-fizzbuzz :: Integer -> String
fizzbuzz x = if mod x 3 == 0 || 5 == 0
-}