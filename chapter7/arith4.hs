module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

main = do
  print (roundTrip 4)
  print (roundTrip True)
  print (roundTrip 'b')
  print (roundTrip "name")
  print (roundTrip ('x', 10.2, "text here"))
  print (roundTrip' 4)
  print (roundTrip' True)
  print (roundTrip' 'b')
  print (roundTrip' "name")
  print (roundTrip' ('x', 10.2, "text here"))
  print (roundTrip'' 4 :: Int)
  print (roundTrip'' True :: Bool)
  print (roundTrip'' 'b' :: Char)
  print (roundTrip'' "name" :: String)
  print (roundTrip'' ('x', 10.2, "text here") :: (Char, Float, String))
  print (id 4)
