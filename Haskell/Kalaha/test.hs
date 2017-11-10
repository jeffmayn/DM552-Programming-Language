s = [1,2,3,4,5,6,0,1,2,3,4,5,6,0]

firstMove m s = k
 where
   hej = splitAt m s
   first = fst(hej)
   second = snd(hej)

   k = (first ++ 0 : second)


--    let (x,_:ys) = splitAt m s
--	s = (x ++ 0 : ys)



--  firstMove m s = s
--    where
--      let (x,_:ys) = splitAt m s
--  	s = (x ++ 0 : ys)


--  incrementMove m s = s
--    where
--      h = s!!m
--  	x = fst(splitAt m s)
--  	ys = snd(splitAt m s)
--  	--let (x,_:ys) = splitAt m s
--  	s = x ++ (h+1) : ys
