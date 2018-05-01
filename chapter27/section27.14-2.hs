
-- Type code in GHCI instead of loading the file

-- What will :sprint output?

-- 1.

let x1 = 1

-- x1 = _

-- 2.

let x2 = ['1']

-- x2 = "1"

-- 3.

let x3 = [1]

-- x3 = _

-- 4.

let x4 = 1 :: Int

-- x4 = 1

-- 5.

let f = \x -> x
let x5 = f 1

-- x5 = _

-- 6.

let f' :: Int -> Int; f' = \x -> x
let x6 = f' 1

-- x6 = _
