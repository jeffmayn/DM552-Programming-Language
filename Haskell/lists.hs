lostNumbers = [4,8,15,16,23,42]
foo = [1,2,3,4] ++ [9,10,11,12]
hello = "hello" ++ " " ++ "world"
woot = ['w','o'] ++ ['o','t']

cat = 'A':" SMALL CAT"                          -- instantly append 'A' to ' SMALL CAT'
                                                -- if we use ++ and it was on big lists, it would
                                                -- take forever.
numb = 5:[1,2,3,4,5]
numb2 = 1:2:3:[]                                -- same as [1,2,3]

steve = "Steve buscemi" !! 6                    -- outputs the 7th character (0 indexed)
numb3 = [9.4,33.2,96.2,11.2,23.25] !! 1
numb4 = [3,4,5]                                 -- numb 4 > numb5 = True
numb5 = [2,4]

numb4_head = head numb4                         -- head of numb 4 = [3]
numb4_tail = tail numb4                         -- tail of numb 4 = [4,5]
numb4_last = last numb4                         -- last of numb 4 = [5]
numb4_init = init numb4                         -- init of numb 4 = [3,4]
numb4_length = length numb4                     -- length of numb 4 = 3

numb5_null = null [1,2,3]                       -- False
numb6 = []
numb6_null = null numb6                         -- True

numb4_reverse = reverse numb4                   -- numb4 = [3,4,5], reversed = [5,4,3]

numb4_take_2 = take 2 numb4                     -- [3,4]
numb4_take_0 = take 0 numb4                     -- []

numb4_drop_2 = drop 2 numb4                     -- [5]
numb4_drop_0 = drop 0 numb4                     -- [3,4,5]

numb4_max = maximum numb4                       -- 5
numb4_min = minimum numb4                       -- 3

numb4_sum = sum numb4                           -- 12
numb4_product = product numb4                   -- 60

numb4_element_4 = 4 `elem` numb4                -- True
numb4_element_8 = 8 `elem` numb4                -- False

-- RANGES: ------------------------------------------
range1 = [1..20]                                -- lists 1 to 20
range2 = ['a'..'z']                             -- lists a to z
range3 = ['A'..'Z']                             -- lists A to Z
range4 = [2,4..20]                              -- 2-tabellen op til 20
range5 = [3,6..20]                              -- 3-tabellen op til 20
range6 = [0.1, 0.3..1]                          -- NB! funky results on floats in lists

--CYCLES: --------------------------------------------
range7 = take 10 (cycle numb4)                  -- Cycles through numb4 until 10 numbers are reached.
range8 = take 12 (cycle "LOL ")                 -- outputs: "LOL LOL LOL "

--REPEAT: ---------------------------------------------------
range9 = take 10 (repeat 5)                     -- repeats '5' 10 times
range10 = take 10 (repeat numb4)                -- repeats numb4 10 times

--REPLICATE: ----------------------------------------------
range11 = replicate 3 10                        -- returns [10,10,10]
range12 = replicate 3 numb4                     -- returns numb4 3 times

--FUNCTIONS: ------------------------------------------
range13 = [x*2 | x <- [1..10]]                  -- without condition
range14 = [x*2 | x <- [1..10], x*2 >= 12]       -- with condition >= 12
range15 = [x | x <- [50..100], x `mod` 7 == 3]  -- all numbers from 50-100 whos remainder when divided by 7 is 3

--COMPREHENSION: ---------------------------------------
-- // replaces each odd number > 10 with 'BANG' and < 10 with 'BOOM', everything even is thrown away.
-- boomBangs [7..13]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]


-- // we want all numbers from 10 to 20 that are not 13, 15 or 19
someList1 = [x | x <- [10..20], x /= 13, x /= 15, x /= 19]

-- // all possible combinations of two list multiplied:
someList2 = [x*y | x <- [2,5,10], y <- [8,10,11]]

-- // same, but only > 50
someList3 = [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

-- // combine adjectives and a list of nouns
nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]
epic_hilarity = [adjectives ++ " " ++ noun | adjectives <- adjectives, noun <- nouns]

-- // "_" is used instead of variabel
length' xs = sum[1 | _ <- xs]                   -- eg. "Hello" return 5

-- // removes all non uppercase-letters from a string
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

--TUBLES: -------------------------------------------
-- // a list of numbers is a list of numbers (same type)
-- // but tubles can be a different types
tuble1 = [(1,2),(8,11),(4,5)]                   -- legal tuble
-- tuble2 = [(1,2),(8,11,3),(4,5)]              -- illegal tuble
tuble2 = ("Christopher","Walken",55)
tuble3 = fst (8,11)                             -- returns first element in list
tuble4 = snd ("Wow", False)                     -- returns second element in list
-- // produce a list of pairs with zip
tuble5 = zip [1,2,3,4,5] [5,5,5,5,5]            -- returns [(1,5),(2,5),(3,5),(4,5),(5,5)]
tuble6 = zip [1..5] ["one", "two", "three", "four", "five"]
tuble7 = zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]      -- returns [(5,"im"),(3,"a"),(2,"turtle")]
tuble8 = zip [1..] ["apple", "orange", "cherry", "mango"]     -- returns [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")] because its lazy
-- // combine tuples and list comprehensions
triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]] -- returns all triangles with sides equal to or smaller than 10
-- // now, let condition be: all have to be right triangles, b <= hypothenuse, a <= side b
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
-- // now, we want ones where the perimeter is 24
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

--SOMETHING COMPLETELY DIFFERENT: ----------------------------------
-- // Members of Show can be represented as strings
showMe = show 3                                 -- output the string "3"
showMeMore = show True                          -- "True"
-- // Read

-- // Read takes a string and returns a type which is member of read
readMe = read "8.2" + 3.8                       -- output the float 12.0
readMeMore = read "[1,2,3,4]" ++ [3]            -- output the list [1,2,3,4,3]
-- // but if we only read one parameter, it doesnt know what type to be
-- readMeWrong = read "4"
-- // but we can force it to be the type we want
readMeForced = read "4" :: Int                  -- outputs 4 as integer
readMeForced2 = read "4" :: Float               -- outputs 4.0 as float
readMeForced3 = (read "4" :: Float) * 4         -- outputs 16.0 as float
readMeForced4 = read "(3, 'a')" :: (Int, Char)  -- outputs 3 as integer and 'a' as Char
