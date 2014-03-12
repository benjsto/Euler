import Control.Monad

--By starting at the top of the triangle below and moving to adjacent numbers on the row below, 
--the maximum total from top to bottom is 23.

--3
--7 4
--2 4 6
--8 5 9 3

--That is, 3 + 7 + 4 + 9 = 23.

--Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'),
--a 15K text file containing a triangle with one-hundred rows.

--NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to
--solve this problem, as there are 299 altogether! If you could check one trillion (1012) routes every
--second it would take over twenty billion years to check them all. There is an efficient algorithm
--to solve it. ;o)

readInts :: [String] -> [Int]
readInts x = map read x

getLines = (liftM lines . readFile) "euler67triangle.txt"

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

efficient67 = readFile "euler67triangle.txt" >>= print . solve . parse
parse = map (map read . words) . lines
solve = head . foldr1 step
step [] [z] = [z]
step (x:xs) (y:z:zs) = x + max y z : step xs (z:zs)

