
import Control.Exception

data EATD = NotEven Int
          | NotDivThree Int
  deriving (Eq, Show)

instance Exception EATD

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0  = throwIO (NotDivThree i)
  | odd i         = throwIO (NotEven i)
  | otherwise     = return i

type EA e = IO (Either e Int)

-- > try (evenAndThreeDiv 3) :: EA EATD
-- Left (NotEven 3)
-- > try (evenAndThreeDiv 4) :: EA EATD
-- Left (NotDivThree 4)
-- > try (evenAndThreeDiv 6) :: EA EATD
-- Right 6
