import Control.Monad

--By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

--3
--7 4
--2 4 6
--8 5 9 3

--That is, 3 + 7 + 4 + 9 = 23.

--Find the maximum total from top to bottom of the triangle below:

--75
--95 64
--17 47 82
--18 35 87 10
--20 04 82 47 65
--19 01 23 75 03 34
--88 02 77 73 07 63 67
--99 65 04 28 06 16 70 92
--41 41 26 56 83 40 80 70 33
--41 48 72 33 47 32 37 16 94 29
--53 71 44 65 25 43 91 52 97 51 14
--70 11 33 28 77 73 17 78 39 68 17 57
--91 71 52 38 17 14 91 43 58 50 27 29 48
--63 66 04 68 89 53 67 30 73 16 69 87 40 31
--04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

--NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route.
--However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force,
 --and requires a clever method! ;o)

readInts :: [String] -> [Int]
readInts x = map read x

getLines = (liftM lines . readFile) "euler18triangle"

getWords = do
	lns <- getLines
	return (map  words lns)

getInts = do
	ints <- getWords
	return (map readInts ints)

calcMaxes :: ([[Int]], Int, Int) -> Int
calcMaxes (triangle, x, y)
		| y == 0 = (triangle !! y) !! 0
		| x == 0 = ((triangle !! y) !! x) + calcMaxes(triangle, x, y-1)
		| x == length (triangle !! y) - 1 = ((triangle !! y) !! x) + calcMaxes(triangle, x-1, y-1)
		| otherwise = (maximum [calcMaxes(triangle, x-1, y-1), calcMaxes(triangle, x, y-1)]) + ((triangle !! y) !! x)

answer = do
	tr <- getInts
	let numRows = length tr
	let lengthOfLastRow = length (tr !! (numRows-1))
	let calcMaxesForCol n = calcMaxes (tr, n, numRows-1)
	print (map calcMaxesForCol [0..(lengthOfLastRow-1)])

efficient18 = readFile "euler18triangle" >>= print . solve . parse
parse = map (map read . words) . lines
solve = head . foldr1 step
step [] [z] = [z]
step (x:xs) (y:z:zs) = x + max y z : step xs (z:zs)
