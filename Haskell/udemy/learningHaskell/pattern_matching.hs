-- simpel pattern matching
func x y z = x + y + z

--------------------------------------------
data Colour = RGB Int Int Int deriving Show

red :: Colour -> Int
red(RGB r _ _) = r

green :: Colour -> Int
green(RGB _ g _) = g

blue :: Colour -> Int
blue(RGB _ _ b) = b
--------------------------------------------
data Pixel = Pixel Int Int Int Colour

pixelRed :: Pixel -> Int
pixelRed (Pixel _ _ _ (RGB r _ _)) = r
---------------------------------------------
main :: IO ()
main = do
  let c = RGB 10 20 30
  print $ red c
  print $ blue c
  print $ green c

 --print Pixel
  let doh = Pixel 100 100 100 (RGB 10 20 30)
  print $ pixelRed doh
