
import Control.Applicative     ((<|>), (<*), (*>))
import Text.Parser.Char        (char, digit, oneOf)
import Text.Parser.Token       (decimal)
import Text.Parser.Combinators (some, sepBy1, option, eof, try, notFollowedBy)
import Text.Trifecta           (Parser, Result(..), parseString)
import Test.Hspec

data VerIdentifier = AlphaIdentifier String | NumIdentifier Integer
  deriving (Eq, Show)

instance Ord VerIdentifier where
  compare (NumIdentifier   _) (AlphaIdentifier _) = LT
  compare (AlphaIdentifier _) (NumIdentifier   _) = GT
  compare (NumIdentifier   x) (NumIdentifier   y) = compare x y
  compare (AlphaIdentifier x) (AlphaIdentifier y) = compare x y

type VerLabel = [VerIdentifier]

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = VerLabel
type Metadata = VerLabel

data SemanticVer = SemanticVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemanticVer where
  compare (SemanticVer ma1 mi1 p1 r1 _) (SemanticVer ma2 mi2 p2 r2 _)
    | ma1 /= ma2          = compare ma1 ma2
    | mi1 /= mi2          = compare mi1 mi2
    | p1  /= p2           = compare p1 p2
    | null r1 && null r2  = EQ
    | null r1             = GT
    | null r2             = LT
    | otherwise           = compare r1 r2

---

alphaNumHyphen :: Parser Char
alphaNumHyphen = oneOf ['a'..'z'] <|> oneOf ['A'..'Z'] <|> digit <|> char '-'

-- TODO: reject numeric identifiers with leading zero (except for metadata?)
parseVerIdentifier :: Parser VerIdentifier
parseVerIdentifier =
  (NumIdentifier <$> try (decimal <* notFollowedBy alphaNumHyphen)) <|>
  (AlphaIdentifier <$> some alphaNumHyphen)

parseReleaseLabel :: Parser VerLabel
parseReleaseLabel = char '-' *> sepBy1 parseVerIdentifier (char '.')

parseMetadataLabel :: Parser VerLabel
parseMetadataLabel = char '+' *> sepBy1 parseVerIdentifier (char '.')

parseSemanticVer :: Parser SemanticVer
parseSemanticVer = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  patch <- decimal
  release <- option [] parseReleaseLabel
  metadata <- option [] parseMetadataLabel
  eof
  return (SemanticVer major minor patch release metadata)

---

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

parse :: String -> Maybe SemanticVer
parse = maybeSuccess . parseString parseSemanticVer mempty

main :: IO ()
main = hspec $ do

  describe "Parse semantic version" $ do

    it "without labels" $ do
      parse "1.2.3"  `shouldBe` Just (SemanticVer 1 2 3 [] [])
      parse "0.0.0"  `shouldBe` Just (SemanticVer 0 0 0 [] [])
      parse ".2.3"   `shouldBe` Nothing
      parse "1..3"   `shouldBe` Nothing
      parse "1.2."   `shouldBe` Nothing
      parse "1.."    `shouldBe` Nothing
      parse ".2."    `shouldBe` Nothing
      parse "..3"    `shouldBe` Nothing
      parse ".."     `shouldBe` Nothing
      parse "1.2"    `shouldBe` Nothing
      parse "1."     `shouldBe` Nothing
      parse "1"      `shouldBe` Nothing
      parse "a"      `shouldBe` Nothing
      parse "-"      `shouldBe` Nothing
      parse " "      `shouldBe` Nothing
      parse ""       `shouldBe` Nothing
      parse "a.0.0"  `shouldBe` Nothing
      parse "0.a.0"  `shouldBe` Nothing
      parse "0.0.a"  `shouldBe` Nothing
      parse "-1.1.1" `shouldBe` Nothing
      parse "1.-1.1" `shouldBe` Nothing
      parse "1.1.-1" `shouldBe` Nothing
      -- Not handling these cases at the moment...
      -- parse "01.1.1" `shouldBe` Nothing
      -- parse "1.01.1" `shouldBe` Nothing
      -- parse "1.1.01" `shouldBe` Nothing

    it "with release labels only" $ do
      parse "1.2.3-a"  `shouldBe`
        Just (SemanticVer 1 2 3 [AlphaIdentifier "a"] [])
      parse "1.2.3-1"  `shouldBe`
        Just (SemanticVer 1 2 3 [NumIdentifier 1] [])
      parse "1.2.3-a1"  `shouldBe`
        Just (SemanticVer 1 2 3 [AlphaIdentifier "a1"] [])
      parse "1.2.3-alpha-1.9.-x-y-z.1"  `shouldBe`
        Just (SemanticVer 1 2 3 [AlphaIdentifier "alpha-1",
                                 NumIdentifier 9,
                                 AlphaIdentifier "-x-y-z",
                                 NumIdentifier 1] [])
      parse "1.2.3-a-"  `shouldBe`
        Just (SemanticVer 1 2 3 [AlphaIdentifier "a-"] [])
      parse "1.2.3-"     `shouldBe` Nothing
      parse "1.2.3-."    `shouldBe` Nothing
      parse "1.2.3-a."   `shouldBe` Nothing
      parse "1.2.3-á"    `shouldBe` Nothing
      parse "1.2.3-1."   `shouldBe` Nothing
      parse "1.2.3-1.1." `shouldBe` Nothing
      parse "1.2.3-1:"   `shouldBe` Nothing
      parse "1.2.3-1?"   `shouldBe` Nothing
      parse "1.2.3-1a"   `shouldBe`
        Just (SemanticVer 1 2 3 [AlphaIdentifier "1a"] [])
      -- Not handling these cases at the moment...
      -- parse "1.2.3-0"  `shouldBe` Nothing
      -- parse "1.2.3-01" `shouldBe` Nothing

    it "with metadata labels only" $ do
      parse "1.2.3+a"  `shouldBe`
        Just (SemanticVer 1 2 3 [] [AlphaIdentifier "a"])
      parse "1.2.3+1"  `shouldBe`
        Just (SemanticVer 1 2 3 [] [NumIdentifier 1])
      parse "1.2.3+a1"  `shouldBe`
        Just (SemanticVer 1 2 3 [] [AlphaIdentifier "a1"])
      parse "1.2.3+alpha-1.9.-xyz.1"  `shouldBe`
        Just (SemanticVer 1 2 3 [] [AlphaIdentifier "alpha-1",
                                    NumIdentifier 9,
                                    AlphaIdentifier "-xyz",
                                    NumIdentifier 1])
      parse "1.2.3+"     `shouldBe` Nothing
      parse "1.2.3+."    `shouldBe` Nothing
      parse "1.2.3+a."   `shouldBe` Nothing
      parse "1.2.3+á"    `shouldBe` Nothing
      parse "1.2.3+1."   `shouldBe` Nothing
      parse "1.2.3+1.1." `shouldBe` Nothing
      parse "1.2.3+1:"   `shouldBe` Nothing
      parse "1.2.3+1+"   `shouldBe` Nothing
      parse "1.2.3+1a"   `shouldBe`
        Just (SemanticVer 1 2 3 [] [AlphaIdentifier "1a"])

    it "with release and metadata labels" $ do
      parse "1.2.3-a+b" `shouldBe`
        Just (SemanticVer 1 2 3 [AlphaIdentifier "a"] [AlphaIdentifier "b"])
      parse "1.2.3-1.alpha-2.9+2.beta-1.99" `shouldBe`
        Just (SemanticVer 1 2 3 [NumIdentifier 1,
                                 AlphaIdentifier "alpha-2",
                                 NumIdentifier 9]
                                [NumIdentifier 2,
                                 AlphaIdentifier "beta-1",
                                 NumIdentifier 99])
      parse "1.2.3-a+"  `shouldBe` Nothing
      parse "1.2.3-.+a" `shouldBe` Nothing
      parse "1.2.3-+a"  `shouldBe` Nothing
      parse "1.2.3-+a." `shouldBe` Nothing

  describe "Compare version labels separately" $ do

    it "single indetifiers" $ do
      []                    < [NumIdentifier 0]     `shouldBe` True
      []                    < [AlphaIdentifier "a"] `shouldBe` True
      [NumIdentifier 9]     < [AlphaIdentifier "a"] `shouldBe` True
      [NumIdentifier 0]     < [NumIdentifier 1]     `shouldBe` True
      [AlphaIdentifier "a"] < [AlphaIdentifier "z"] `shouldBe` True
      [AlphaIdentifier "z"] > [NumIdentifier 0]     `shouldBe` True

    it "multiple indetifiers" $ do
      compare [NumIdentifier 0, NumIdentifier 9]
              [NumIdentifier 0, AlphaIdentifier "a"]
              `shouldBe` LT
      compare [NumIdentifier 0, NumIdentifier 9]
              [NumIdentifier 0, NumIdentifier 9, AlphaIdentifier "a"]
              `shouldBe` LT
      compare [NumIdentifier 0, NumIdentifier 9, AlphaIdentifier "a"]
              [NumIdentifier 0, NumIdentifier 9, AlphaIdentifier "a"]
              `shouldBe` EQ
      compare [NumIdentifier 0, NumIdentifier 9, AlphaIdentifier "b"]
              [NumIdentifier 0, NumIdentifier 9, AlphaIdentifier "a"]
              `shouldBe` GT

  describe "Compare parsed semantic versions" $ do

    it "without version labels" $ do
      parse "1.9.0"  `compare` parse "1.10.0" `shouldBe` LT
      parse "1.10.0" `compare` parse "1.11.0" `shouldBe` LT
      parse "1.10.9" `compare` parse "1.11.0" `shouldBe` LT
      parse "1.10.0" `compare` parse "1.10.1" `shouldBe` LT
      parse "1.10.9" `compare` parse "2.0.0"  `shouldBe` LT
      parse "1.2.3"  `compare` parse "1.2.3"  `shouldBe` EQ

    it "with release labels only" $ do
      parse "1.2.3-0"     `compare` parse "1.2.3"       `shouldBe` LT
      parse "1.2.3-a"     `compare` parse "1.2.3"       `shouldBe` LT
      parse "1.2.3-0"     `compare` parse "1.2.3-1"     `shouldBe` LT
      parse "1.2.3-0"     `compare` parse "1.2.3-a"     `shouldBe` LT
      parse "1.2.3-a"     `compare` parse "1.2.3-b"     `shouldBe` LT
      parse "1.2.3-a"     `compare` parse "1.2.3-a0"    `shouldBe` LT
      parse "1.2.3-a.0"   `compare` parse "1.2.3-a.1"   `shouldBe` LT
      parse "1.2.3-a.0"   `compare` parse "1.2.3-a.0.0" `shouldBe` LT
      parse "1.0.0-alpha" `compare` parse "1.0.0"       `shouldBe` LT
      parse "1.2.3-a.0.0" `compare` parse "1.2.3-a.0.0" `shouldBe` EQ

    it "with metadata labels only" $ do
      parse "1.2.3"       `compare` parse "1.2.3+0"     `shouldBe` EQ
      parse "1.2.3"       `compare` parse "1.2.3+a"     `shouldBe` EQ
      parse "1.2.3+0"     `compare` parse "1.2.3+1"     `shouldBe` EQ
      parse "1.2.3+0"     `compare` parse "1.2.3+a"     `shouldBe` EQ
      parse "1.2.3+a"     `compare` parse "1.2.3+b"     `shouldBe` EQ
      parse "1.2.3+a"     `compare` parse "1.2.3+a0"    `shouldBe` EQ
      parse "1.2.3+a.0"   `compare` parse "1.2.3+a.1"   `shouldBe` EQ
      parse "1.2.3+a.0"   `compare` parse "1.2.3+a.0.0" `shouldBe` EQ
      parse "1.2.3+a.0.0" `compare` parse "1.2.3+a.0.0" `shouldBe` EQ

    it "with release and metadata labels" $ do
      parse "1.2.3-0"       `compare` parse "1.2.3-0+0"     `shouldBe` EQ
      parse "1.2.3-0"       `compare` parse "1.2.3-0+a"     `shouldBe` EQ
      parse "1.2.3-0+0"     `compare` parse "1.2.3-0+1"     `shouldBe` EQ
      parse "1.2.3-0+0"     `compare` parse "1.2.3-0+a"     `shouldBe` EQ
      parse "1.2.3-0+a"     `compare` parse "1.2.3-0+b"     `shouldBe` EQ
      parse "1.2.3-0+a"     `compare` parse "1.2.3-0+a0"    `shouldBe` EQ
      parse "1.2.3-0+a.0"   `compare` parse "1.2.3-0+a.1"   `shouldBe` EQ
      parse "1.2.3-0+a.0"   `compare` parse "1.2.3-0+a.0.0" `shouldBe` EQ
      parse "1.2.3-0+a.0.0" `compare` parse "1.2.3-0+a.0.0" `shouldBe` EQ

    it "matches examples in the definition page" $ do
      parse "1.0.0-alpha"      `compare` parse "1.0.0-alpha.1"    `shouldBe` LT
      parse "1.0.0-alpha.1"    `compare` parse "1.0.0-alpha.beta" `shouldBe` LT
      parse "1.0.0-alpha.beta" `compare` parse "1.0.0-beta"       `shouldBe` LT
      parse "1.0.0-beta"       `compare` parse "1.0.0-beta.2"     `shouldBe` LT
      parse "1.0.0-beta.2"     `compare` parse "1.0.0-beta.11"    `shouldBe` LT
      parse "1.0.0-beta.11"    `compare` parse "1.0.0-rc.1"       `shouldBe` LT
      parse "1.0.0-rc.1"       `compare` parse "1.0.0"            `shouldBe` LT
