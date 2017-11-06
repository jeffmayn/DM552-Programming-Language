import Data.List
-- :t intercalate :: [a] -> [[a]] -> [a]
a = intercalate ":" ["/path/to/dir0", "/path/to/dir1"]
-- we took a list of lists, and made one list total, with : between them.

formatList s e sep xs = s ++ (intercalate sep (map show xs)) ++ e
b = formatList "(" ")" "," [1,2,3,4]

-- composing functions with dot
doubleIt x = x * 2
addTen x = x + 10
comp = (addTen . doubleIt) 5 -- here we are composing the two functions applied to 5
f = show . addTen . doubleIt
f2 = map f [10, 11, 12, 13, 14] --now we are mapping the function f to this List

-- lambda expression
parenthesizeWords s = unwords $ map parenthesizeWord (words s)
 where parenthesizeWord s = "(" ++ s ++ ")" -- this is without lambda

-- now with lambda expression
parenthesizeWords_1 s = unwords $ map (\s -> "(" ++ s ++ ")") (words s)

-- filter less than five with lambda
f3 = filter (\x -> x < 5) [1..10]

-- same, but without lambda
f4 = filter (<5) [1..10]
