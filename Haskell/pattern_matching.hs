-- // PATTERN MATCHING
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

-- recursively factorial function
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

-- pattern matching when failing
-- problem is that we dont catch-all patterns => crash of function
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- adding vectors without patter matching
--  addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
--  addVectors a b = (fst a + fst b, snd a + snd b)

-- with pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- fst, snd and thrd of tribles
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- our own implementation of the head function
head' :: [a] -> a
head' [] = error "Can't call head on empty list!"
head' (x:_) = x

-- triviel function to tell elements in the list
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- our own implementation of length
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- our own implementation of sum
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--dirty patterns
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- find my BMI
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight"
  | weight / height ^ 2 <= 25.0 = "You're normal"
  | weight / height ^ 2 <= 30.0 = "You're fat"
  | otherwise = "You're whale!"

evenBetterBmiTell :: (RealFloat a) => a -> a -> String
evenBetterBmiTell weight height
    | bmi_2 <= skinny = "You're underweight, you emo, you!"
    | bmi_2 <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi_2 <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi_2 = weight / height ^ 2  
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- our own implementation of max
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- our own implemenation of compare
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT
