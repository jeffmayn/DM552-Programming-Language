{- AND'ing to inputs, test with:
    and1 False False
    and1 False True
    and1 True False
    and1 True True
 -}
and1 :: Bool -> Bool -> Bool
and1 False False = False
and1 False True = False
and1 True False = False
and1 True True = True

{- now we are just using wildcards to
recognize the pattern. -}
and2 :: Bool -> Bool -> Bool
and2 True True = True
and2 _ _ = False

{- negation with pattern recogn. -}
not1 :: Bool -> Bool
not1 True = False
not1 _ = True

{- OR'ing with pattern recogn. -}
or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 _ _ = True

{- XOR'ing with pattern recogn. -}
xor1 :: Bool -> Bool -> Bool
xor1 True True = False
xor1 False False = False
xor1 _ _ = True

{- NAND'ing with pattern recogn. -}
nand1 :: Bool -> Bool -> Bool
nand1 True True = False
nand1 _ _ = True
