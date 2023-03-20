--1st lab, with some basic things about miniHaskell, with its own specific expressions evaluation

--1
let squareSum = \x -> \y -> + (* x x) (* y y) in squareSum 2 3

--2
letrec revRange = \n -> if (isZero n) [0] (:n (revRange (- n 1))) in revRange 4

let range = \n -> letrec revRange = \x -> if (isZero x) [0] (:x (revRange (- x 1))) in reverse (revRange n) in range 4

--3
let justList = \l -> map (fromMaybe []) (filter (\x -> isJust x) l) in justList [Just 4, Nothing, Just 5]

--4
let all = \p -> \xs -> foldr (&&) True (map p xs) in all isZero [0, 0, 1]
