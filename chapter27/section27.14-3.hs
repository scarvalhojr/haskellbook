
-- Type code in GHCI instead of loading the file

-- Will printing these expressions result in bottom?

-- 1.

snd (undefined, 1)

-- No

-- 2.

let x = undefined
let y = x `seq` 1 in snd (x, y)

-- Yes

-- 3.

length $ [1..5] ++ undefined

-- Yes

-- 4.

length $ [1..5] ++ [undefined]

-- No

-- 5.

const 1 undefined

-- No

-- 6.

const 1 (undefined `seq` 1)

-- No

-- 7.

const undefined 1

-- Yes
