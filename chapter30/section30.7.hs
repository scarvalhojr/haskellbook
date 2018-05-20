
import Control.Exception

data NotDivThree = NotDivThree Int
  deriving (Eq, Show)

instance Exception NotDivThree

data NotEven = NotEven Int
  deriving (Eq, Show)

instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0  = throwIO (NotDivThree i)
  | odd i         = throwIO (NotEven i)
  | otherwise     = return i

evenAndThreeDiv' :: Int -> Int
evenAndThreeDiv' i
  | rem i 3 /= 0  = throw (NotDivThree i)
  | odd i         = throw (NotEven i)
  | otherwise     = i

---

catchNotDivThree :: IO Int -> (NotDivThree -> IO Int) -> IO Int
catchNotDivThree = catch

catchNotEven :: IO Int -> (NotEven -> IO Int) -> IO Int
catchNotEven = catch

-- > catchNotDivThree (evenAndThreeDiv 8) (\(NotDivThree i) -> return (-1))
-- -1
-- > catchNotDivThree (evenAndThreeDiv 3) (\(NotDivThree i) -> return (-1))
-- *** Exception: NotEven 3
-- > catchNotDivThree (evenAndThreeDiv 6) (\(NotDivThree i) -> return (-1))
-- 6

-- > catchNotEven (evenAndThreeDiv 3) (\(NotEven i) -> return (-2))
-- -2
-- > catchNotEven (evenAndThreeDiv 4) (\(NotEven i) -> return (-2))
-- *** Exception: NotDivThree 4
-- > catchNotEven (evenAndThreeDiv 6) (\(NotEven i) -> return (-2))
-- 6

type EA e = IO (Either e Int)

-- > try (evenAndThreeDiv 3) :: EA NotEven
-- Left (NotEven 3)
-- > try (evenAndThreeDiv 6) :: EA NotEven
-- Right 6
-- > try (evenAndThreeDiv 8) :: EA NotEven
-- *** Exception: NotDivThree 8
--
-- > try (evenAndThreeDiv 8) :: EA NotDivThree
-- Left (NotDivThree 8)
-- > try (evenAndThreeDiv 3) :: EA NotDivThree
-- *** Exception: NotEven 3
-- > try (evenAndThreeDiv 12) :: EA NotDivThree
-- Right 12

---

-- Defined in ‘Control.Exception’
-- catches :: IO a -> [Handler a] -> IO a

catchBoth :: IO Int -> IO Int
catchBoth ioInt = catches ioInt
  [ Handler (\(NotEven _) -> return (-1))
  , Handler (\(NotDivThree _) -> return (-2))
  ]

-- > catchBoth (evenAndThreeDiv 3)
-- -1
-- > catchBoth (evenAndThreeDiv 4)
-- -2
-- > catchBoth (evenAndThreeDiv 6)
-- 6
