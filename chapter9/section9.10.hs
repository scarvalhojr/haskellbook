
answer1 = filter ((==0) . (`rem` 3)) [1..30]

answer2 = (length . filter ((==0) . (`rem` 3))) [1..30]

myFilter :: String -> [String]
myFilter =  filter (not . (`elem` ["the", "a", "an"])) . words
