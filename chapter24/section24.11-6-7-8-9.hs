
import Numeric                 (showHex)
import Data.List               (intersperse, intercalate)
import Data.Word               (Word8, Word16, Word32, Word64)
import Data.Bits               (shiftL, shiftR, (.&.), (.|.))
import Control.Applicative     ((<|>), (<*))
import Data.Char               (digitToInt)
import Text.Parser.Char        (char, hexDigit)
import Text.Parser.Token       (natural)
import Text.Parser.Combinators (unexpected, count, try)
import Text.Trifecta           (Parser, Result(..), parseString)
import Test.Hspec

data IPv4 = IPv4 Word32
  deriving (Eq, Ord)

createIPv4 :: [Word8] -> IPv4
createIPv4 = IPv4 . fromIntegral . combineBitBlocks 8 . take 4

instance Show IPv4 where
  show (IPv4 w) = intercalate "." (map show octets)
    where octets = splitBitBlocks 4 8 w

data IPv6 = IPv6 Word64 Word64
  deriving (Eq, Ord)

createIPv6 :: [Word16] -> IPv6
createIPv6 hs = IPv6 block1 block2
  where (h, t) = splitAt 4 (take 8 (hs ++ repeat 0))
        block1 = fromIntegral $ combineBitBlocks 16 h
        block2 = fromIntegral $ combineBitBlocks 16 t

instance Show IPv6 where
  show (IPv6 w1 w2) = (foldr ($) "" . intersperse (":" ++) . map showHex) hs
    where hs = (splitBitBlocks 4 16 w1) ++ (splitBitBlocks 4 16 w2)

ipV6ToInteger :: IPv6 -> Integer
ipV6ToInteger (IPv6 p1 p2) = combineBitBlocks 64 [p1, p2]

combineBitBlocks :: Integral a => Int -> [a] -> Integer
combineBitBlocks b = foldl (\acc d -> fromIntegral d + shiftL acc b) 0

splitBitBlocks :: Integral a => Int -> Int -> a -> [Integer]
splitBitBlocks n b dec = (reverse . map fst . take n . drop 1) bls
  where bls = iterate (\(_, d) -> (d .&. (2 ^ b - 1), shiftR d b)) (0, int)
        int = fromIntegral dec

convertIPv4ToIPv6 :: IPv4 -> IPv6
convertIPv4ToIPv6 (IPv4 w32) = IPv6 0 (mask .|. (fromIntegral w32))
  where mask = shiftL (2 ^ 16 - 1) 32

---

parseOctet :: Parser Word8
parseOctet = do
  oct <- natural
  if oct > 255 then
    unexpected "invalid octect"
  else
    return $ fromIntegral oct

ipV4 :: Parser IPv4
ipV4 = createIPv4 <$> octets
  where octets = (:) <$> parseOctet <*> count 3 (char '.' *> parseOctet)

parseHextet :: Parser Word16
parseHextet = do
  hex <- oneUpTo 4 hexDigit
  let val = hexToInteger hex
  if val > 65535 then
    unexpected "invalid hextet"
  else
    return $ fromIntegral val

-- ipV6Full      0 (:0)+
-- ipV6AbbrStart : (:0)+
-- ipV6AbbrMid   (0:)+ (:0)+
-- ipV6AbbrEnd   (0:)+ :
-- ipV6Zero      ::
ipV6 :: Parser IPv6
ipV6 = try ipV6Full <|>
       try ipV6AbbrStart <|>
       try ipV6AbbrMid <|>
       try ipV6AbbrEnd <|>
       ipV6Zero

ipV6Full :: Parser IPv6
ipV6Full = createIPv6 <$> hextets
  where hextets = (:) <$> parseHextet <*> count 7 (char ':' *> parseHextet)

ipV6AbbrStart :: Parser IPv6
ipV6AbbrStart = do
  char ':'
  t <- oneUpTo 7 (char ':' *> parseHextet)
  let missing = 8 - length t
  return $ createIPv6 ((replicate missing 0) ++ t)

ipV6AbbrEnd :: Parser IPv6
ipV6AbbrEnd = do
  h <- oneUpTo 7 (parseHextet <* char ':')
  char ':'
  let missing = 8 - length h
  return $ createIPv6 (h ++ (replicate missing 0))

ipV6AbbrMid :: Parser IPv6
ipV6AbbrMid = do
  h <- oneUpTo 6 (parseHextet <* char ':')
  let remain = 8 - length h
  t <- oneUpTo (remain - 1) (char ':' *> parseHextet)
  let missing = remain - length t
  return $ createIPv6 (h ++ (replicate missing 0) ++ t)

ipV6Zero :: Parser IPv6
ipV6Zero = char ':' *> char ':' *> return (createIPv6 [])

hexToInteger :: [Char] -> Integer
hexToInteger = foldl (\acc d -> 16 * acc + fromIntegral (digitToInt d)) 0

zeroUpTo :: Int -> Parser a -> Parser [a]
zeroUpTo n p
  | n > 0      = (:) <$> try p <*> zeroUpTo (n - 1) p <|> return []
  | otherwise  = return []

oneUpTo :: Int -> Parser a -> Parser [a]
oneUpTo n p
  | n > 0      = (:) <$> p <*> zeroUpTo (n - 1) p
  | otherwise  = return []

---

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

parseIPv4 :: String -> Maybe IPv4
parseIPv4 = maybeSuccess . parseString ipV4 mempty

parseIPv6 :: String -> Maybe IPv6
parseIPv6 = maybeSuccess . parseString ipV6 mempty

-- IPv6 validator:
-- https://www.helpsystems.com/intermapper/ipv6-test-address-validation

main :: IO ()
main = hspec $ do

  describe "Parse examples" $ do

    it "IPv4" $ do
      let ip1 = "172.16.254.1"
      let ip2 = "204.120.0.15"
      parseIPv4 ip1 `shouldBe` Just (IPv4 2886794753)
      parseIPv4 ip2 `shouldBe` Just (IPv4 3430416399)

    it "IPv6" $ do
      let ip1 = "0:0:0:0:0:ffff:ac10:fe01"
      let ip2 = "0:0:0:0:0:ffff:cc78:f"
      let ip3 = "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
      let ip4 = "2001:DB8:0:0:8:800:200C:417A"
      parseIPv6 ip1 `shouldBe` Just (IPv6 0 281473568538113)
      parseIPv6 ip2 `shouldBe` Just (IPv6 0 281474112159759)
      ipV6ToInteger <$> parseIPv6 ip3 `shouldBe`
        Just 338288524927261089654163772891438416681
      ipV6ToInteger <$> parseIPv6 ip4 `shouldBe`
        Just 42540766411282592856906245548098208122
      let ip5 = "FE80::0202:B3FF:FE1E:8329"
      ipV6ToInteger <$> parseIPv6 ip5 `shouldBe`
        Just 338288524927261089654163772891438416681
      let ip6 = "2001:DB8::8:800:200C:417A"
      ipV6ToInteger <$> parseIPv6 ip6 `shouldBe`
        Just 42540766411282592856906245548098208122

  describe "Parse basic IPs" $ do

    it "IPv4" $ do
      parseIPv4 "0.0.0.0" `shouldBe` Just (IPv4 0)
      parseIPv4 "0.0.0.1" `shouldBe` Just (IPv4 1)
      parseIPv4 "0.0.1.0" `shouldBe` Just (IPv4 256)
      parseIPv4 "0.1.0.0" `shouldBe` Just (IPv4 65536)
      parseIPv4 "1.0.0.0" `shouldBe` Just (IPv4 16777216)
      parseIPv4 "1.1.1.1" `shouldBe` Just (IPv4 16843009)

    it "IPv6" $ do
      parseIPv6 "0:0:0:0:0:0:0:0" `shouldBe` Just (IPv6 0 0)
      parseIPv6 "0:0:0:1:0:0:0:0" `shouldBe` Just (IPv6 1 0)
      parseIPv6 "0:0:0:0:0:0:0:1" `shouldBe` Just (IPv6 0 1)
      parseIPv6 "0:0:0:1:0:0:0:1" `shouldBe` Just (IPv6 1 1)
      parseIPv6 "1:0:0:0:0:0:0:1" `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "0000:0000:0000:1:0000:0000:0000:1" `shouldBe` Just (IPv6 1 1)

    it "IPv6 with missing hextets" $ do
      parseIPv6 "1::0:0:0:0:0:1"  `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1:0::0:0:0:0:1"  `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1:0:0::0:0:0:1"  `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1:0:0:0::0:0:1"  `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1:0:0:0:0::0:1"  `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1:0:0:0:0:0::1"  `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1:0:0:0:0::1"    `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1:0:0:0::1"      `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1:0:0::1"        `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1:0::1"          `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1::1"            `shouldBe` Just (IPv6 281474976710656 1)
      parseIPv6 "1::"             `shouldBe` Just (IPv6 281474976710656 0)
      parseIPv6 "::1"             `shouldBe` Just (IPv6 0 1)
      parseIPv6 "::"              `shouldBe` Just (IPv6 0 0)

  describe "Reject invalid IPv4s" $ do

    it "missing octects" $ do
      parseIPv4 ".16.254.1"   `shouldBe` Nothing
      parseIPv4 "172..254.1"  `shouldBe` Nothing
      parseIPv4 "204.120..15" `shouldBe` Nothing
      parseIPv4 "204.120.0."  `shouldBe` Nothing

    it "invalid octects" $ do
      parseIPv4 "256.16.254.1"  `shouldBe` Nothing
      parseIPv4 "172.a.254.1"   `shouldBe` Nothing
      parseIPv4 "204.120.-1.15" `shouldBe` Nothing
      parseIPv4 "204.120.0.- "  `shouldBe` Nothing

  describe "Reject invalid IPv6s" $ do

    it "missing hextets" $ do
      parseIPv6 "0:0:0:0:0:0:0"  `shouldBe` Nothing
      parseIPv6 "0:0:0:0:0:0:0:" `shouldBe` Nothing
      parseIPv6 ":0:0:0:0:0:0:0" `shouldBe` Nothing
      -- TODO: should these be rejected?
      -- parseIPv6 "0:::0:0:0:0:0"  `shouldBe` Nothing
      -- parseIPv6 ":::0:0:0:0:0"   `shouldBe` Nothing
      -- parseIPv6 "0::::0:0:0:0"   `shouldBe` Nothing
      -- parseIPv6 ":::::::"        `shouldBe` Nothing

    it "invalid hextets" $ do
      parseIPv6 "0:0:0:0:0:0:0:x" `shouldBe` Nothing
      parseIPv6 "0:0:0:0:0:0:x:0"  `shouldBe` Nothing
      parseIPv6 "0:0:0:0:0:-1:0:0" `shouldBe` Nothing
      parseIPv6 "0:0:0:0:+1:0:0:0" `shouldBe` Nothing
      parseIPv6 "0:0:0:*:0:0:0:0"  `shouldBe` Nothing
      parseIPv6 "0:0:g:0:0:0:0:0"  `shouldBe` Nothing
      parseIPv6 "0:G:0:0:0:0:0:0"  `shouldBe` Nothing
      parseIPv6 "/:G:0:0:0:0:0:0"  `shouldBe` Nothing

    it "hextets with more than 4 characters" $ do
      parseIPv6 "00000:0:0:0:0:0:0:0" `shouldBe` Nothing
      parseIPv6 "0:00000:0:0:0:0:0:0" `shouldBe` Nothing
      parseIPv6 "0:0:00000:0:0:0:0:0" `shouldBe` Nothing
      parseIPv6 "0:0:0:00000:0:0:0:0" `shouldBe` Nothing
      parseIPv6 "0:0:0:0:00000:0:0:0" `shouldBe` Nothing
      parseIPv6 "0:0:0:0:0:00000:0:0" `shouldBe` Nothing
      parseIPv6 "0:0:0:0:0:0:00000:0" `shouldBe` Nothing
      -- TODO: should this be rejected?
      -- parseIPv6 "0:0:0:0:0:0:0:00000" `shouldBe` Nothing

  describe "Show and parse complement" $ do

    it "IPv4" $ do
      let Just ip1 = parseIPv4 "0.0.0.0"
      let Just ip2 = parseIPv4 "179.16.0.99"
      parseIPv4 (show ip1) `shouldBe` Just ip1
      parseIPv4 (show ip2) `shouldBe` Just ip2

    it "IPv6" $ do
      let Just ip1 = parseIPv6 "0:0:0:0:0:0:0:0"
      let Just ip2 = parseIPv6 "FE80:0:0:0:0202:B3FF:FE1E:8329"
      let Just ip3 = parseIPv6 "2001:DB8:0:0:8:800:200C:417A"
      parseIPv6 (show ip1) `shouldBe` Just ip1
      parseIPv6 (show ip2) `shouldBe` Just ip2
      parseIPv6 (show ip3) `shouldBe` Just ip3

    describe "Convert IPv4 to IPv6" $ do

      it "basic IPs" $ do
        convertIPv4ToIPv6 <$> (parseIPv4 "0.0.0.0") `shouldBe`
          parseIPv6 "0:0:0:0:0:ffff:0:0"
        convertIPv4ToIPv6 <$> (parseIPv4 "0.1.0.1") `shouldBe`
          parseIPv6 "0:0:0:0:0:ffff:1:1"
        convertIPv4ToIPv6 <$> (parseIPv4 "1.0.1.0") `shouldBe`
          parseIPv6 "0:0:0:0:0:ffff:100:100"
        convertIPv4ToIPv6 <$> (parseIPv4 "255.255.255.255") `shouldBe`
          parseIPv6 "0:0:0:0:0:ffff:ffff:ffff"

      it "basic IPs" $ do
        convertIPv4ToIPv6 <$> (parseIPv4 "37.0.7.189") `shouldBe`
          parseIPv6 "0:0:0:0:0:ffff:2500:07bd"
        convertIPv4ToIPv6 <$> (parseIPv4 "176.169.0.55") `shouldBe`
          parseIPv6 "0:0:0:0:0:ffff:b0a9:0037"
        convertIPv4ToIPv6 <$> (parseIPv4 "249.100.111.88") `shouldBe`
          parseIPv6 "0:0:0:0:0:ffff:f964:6f58"
