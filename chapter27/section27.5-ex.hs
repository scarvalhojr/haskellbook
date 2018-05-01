
-- 1.

ex1 = const 1 undefined

-- ex1 = 1

-- 2.

ex2 = const undefined 1

-- ex2 = bottom

-- 3.

ex3 = flip const undefined 1

-- ex3 = 1

-- 4.

ex4 = flip const 1 undefined

-- ex4 = bottom

-- 5.

ex5 = const undefined undefined

-- ex5 = bottom

-- 6.

ex6 = foldr const 'z' ['a'..'e']

-- ex6 = 'a'

-- 7.

ex7 = foldr (flip const) 'z' ['a'..'e']

-- ex7 = 'z'
