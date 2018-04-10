
import Data.Word (Word16)
import Numeric   (showHex)
import Data.List (intercalate, intersperse)

showIPv6 :: [Word16] -> String
showIPv6 = intercalate ":" . map (`showHex` "")

showIPv6' :: [Word16] -> String
showIPv6' = foldr ($) "" . intersperse (":" ++) . map showHex

showIPv6'' :: [Word16] -> String
showIPv6'' = foldr ($) "" . intersperse (':' :) . map showHex

showIPv6''' :: [Word16] -> String
showIPv6''' []     = ""
showIPv6''' (x:xs) = showHex x $ foldr (\a acc -> ':' : showHex a acc) "" xs



-- Some basic perfomance tests
--
-- > length $ showIPv6 (replicate 100000000 65535)
-- (45.59 secs, 143,200,072,464 bytes)
--
-- > length $ showIPv6' (replicate 100000000 65535)
-- (33.20 secs, 119,200,071,048 bytes)
--
-- > length $ showIPv6'' (replicate 100000000 65535)
-- (43.99 secs, 116,000,071,096 bytes)
--
-- > length $ showIPv6''' (replicate 100000000 65535)
-- (56.87 secs, 105,600,071,192 bytes)
