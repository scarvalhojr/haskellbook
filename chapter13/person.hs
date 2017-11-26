
import Text.Read (readMaybe)
import System.Exit (exitSuccess)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise             = Left $ PersonInvalidUnknown $
                              "Name was: " ++ show name ++
                              "Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn $ "Enter the person's name: "
  name <- getLine
  putStrLn $ "Enter the person's age: "
  ageStr <- getLine
  case readMaybe ageStr :: Maybe Integer of
    Nothing  -> do putStrLn $ "Age must be a numeric value."
    Just age -> do case mkPerson name age of
                     Left NameEmpty ->
                       putStrLn $ "ERROR: empty name."
                     Left AgeTooLow ->
                       putStrLn $ "ERROR: age too low."
                     Left (PersonInvalidUnknown err) ->
                       putStrLn $ "ERROR: " ++ err
                     Right person ->
                       putStrLn $ "Person created: " ++ show person
