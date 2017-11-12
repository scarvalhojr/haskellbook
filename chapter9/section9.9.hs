
import Data.Bool

result = map (\x -> bool x (-x) (x == 3)) [1..10]
