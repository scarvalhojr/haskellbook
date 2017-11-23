
import Data.Char (isAsciiUpper, toLower, isLetter)
import Data.Ord (comparing)
import Data.List (find, elemIndex, sort, sortBy, group)

type Digit = Char

data KeyPad = KeyPad [(Digit, [Char])] deriving Show

pulsePhone :: KeyPad
pulsePhone = KeyPad [('1', "1"),
                     ('2', "abc2"),
                     ('3', "def3"),
                     ('4', "ghi4"),
                     ('5', "jkl5"),
                     ('6', "mno6"),
                     ('7', "pqrs7"),
                     ('8', "tuv8"),
                     ('9', "wxyz9"),
                     ('0', "+ 0"),
                     ('*', "^*"),
                     ('#', ".,#")]

-- valid presses: 1 and up
type Presses = Int

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have you ever tasted alcohol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty",
         "Lol ya",
         "Just making sure rofl ur turn"]

reverseTaps :: KeyPad -> Char -> [(Digit, Presses)]
reverseTaps (KeyPad keys) c
  | isAsciiUpper c = ('*', 1) : presses (toLower c)
  | otherwise      = presses c
  where presses x = maybe [] (\(d, chars) -> [(d, 1 + index x chars)]) $ findKey x
        findKey x     = find (\(_,o) -> elem x o) keys
        index x chars =
          case elemIndex x chars of Just i -> i

cellPhonesDead :: KeyPad -> String -> [(Digit, Presses)]
cellPhonesDead keypad = foldMap (reverseTaps keypad)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mostFrequent :: Ord a => [a] -> (Int, a)
mostFrequent = maximum . map (\l -> (length l, head l)) . group . sort

mostPopularLetter :: String -> Char
mostPopularLetter = snd . mostFrequent . map toLower . filter isLetter

mostPopularLetterWithCost :: String -> (Char, Int)
mostPopularLetterWithCost msg = (letter, count * presses)
  where (count, letter) = mostFrequent msg
        presses         = fingerTaps (reverseTaps pulsePhone letter)

coolestLetter :: [String] -> Char
coolestLetter = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = snd. mostFrequent . words . concat
