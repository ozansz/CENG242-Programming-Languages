--------- [ Multiple function definitions /w pattern matching ] ---------

-- Basic function definition
-- fact : int -> int
fact 1 = 1
fact x = x * (fact (x - 1))

-- Function definition with pattern matching
-- fact2 : int -> int
fact2 x = case x of
    1 -> 1
    x -> x * (fact2 (x - 1))

-- X : A basic custom data type
data X = A Int | B (Int->Int) | C

ff x = x + 2

-- Function declaration
gg :: X -> Int

-- Function definitions
gg (A x) = (\x -> x+2) x
gg (B f) = (f 0)
gg C = 0 -- With this definition, the function 'gg' is now a total func.

--------- [ Multivariable functions ] ---------

-- Function that accepts 3 parameters
f1 (x, y, z) = x + y + z

-- Currying function - Higher order function - produces a function if not all three parameters are given at the same time
f2 x y z = x + y + z

f3 = f2 1 -- f3 : \y z -> 1 + y + z
f4 = f3 2 -- f4 : \z -> 1 + 2 + z

--------- [ Recursive functions ] ---------

-- fact3 : Tail-recursive factorial function
fact3' 0 n = n
fact3' k n = fact3' (k-1) n*k 
fact3 n = fact3' n 1

--------- [ Examples of pattern matching ] ---------

-- TREE : Basic tree data type
data TREE a = E | N (a, TREE a, TREE a) deriving Show

tv = N (3,
        N (2, E, E),
        N (3,
            N (1, E, E),
            E
        )
    )

sum1 E = 0
sum1 (N (x, y, z)) = x + (sum1 y) + (sum1 z) 

max1 x y = if (x > y)
    then x
    else y

depth1 E = 0
depth1 (N (x, y, z)) = 1 + (max1 (depth1 y) (depth1 z))

-- Pattern matching in array elements
--last1 [] = ??
last1 [x] = x
last1 (x:y) = last1 y

length1 [] = 0
length1 [x] = 1 -- NOTE: Actually, there is no need to this matching. The one below handles this too.
length1 (x:y) = 1 + (length1 y) 

--nth n [] = ??
nth 1 (x:_) = x
nth n (_:y) = nth (n - 1) y

makelist 0 = []
makelist n = n : (makelist (n - 1))

main = do
    print $ fact 1
    print $ fact 5
    print $ fact2 1
    print $ fact2 5
    print $ gg (A 5)

    -- Giving function as a parameter to another function
    print $ gg (B ff)

    print $ f4 4

    print $ fact3 5

    print $ sum1 tv
    print $ depth1 tv

    print $ last1 [1, 2, 3, 4, 5]
    --print $ last1 [] -- BUG: This requires exception handling
    print $ length1 [1, 2, 3, 4]
    print $ nth 3 [1, 3, 5, 7, 9]
    --print $ nth 99 [1, 3, 9] -- BUG: This requires exception handling
    print $ makelist 5
    print $ length1 (makelist 5)
    print $ last1 (makelist 5)
    print $ nth 2 (makelist 5)
    
