
import Data.Monoid (Monoid, mappend, mempty)
import Data.Semigroup

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))

  mappend (Mem f) (Mem g) = Mem (\x -> let (f1, x')  = f x
                                           (g1, x'') = g x'
                                        in (mappend f1 g1, x''))

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

g' :: Mem Int String
g' = Mem $ \s -> ("ho", s * 2)

main :: IO ()
main = do
  let rmzero  = runMem mempty 0
      rmleft  = runMem (mappend f' mempty) 0
      rmright = runMem (mappend mempty f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
