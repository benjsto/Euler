import Data.List
import Data.Char

problem_1 = sumStep 3 999 + sumStep 5 999 - sumStep 15 999
  where
    sumStep s n = s * sumOnetoN (n `div` s)
    sumOnetoN n = n * (n+1) `div` 2

problem_2 = sum (retainEven (takeWhile ( < 4000000 ) fibs))
	where
		retainEven ns = filter isEven ns
			where
				isEven :: Int -> Bool 
				isEven n = ((mod n 2) == 0)
		fib 0 = 0
		fib 1 = 1
		fib n = fib (n-1) + fib (n-2)
		fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

problem_3 = primeFactors 600851475143
	where
		primeFactors :: Integer -> [Integer]
		primeFactors = factors primes
			where
				factors :: [Integer] -> Integer -> [Integer]
				factors qs@(p:ps) n | n <= 1 = []
									| m == 0 = p : factors qs d
									| otherwise = factors ps n
					where (d,m) = n `divMod` p
				primes :: [Integer]
				primes = primes' (2:[3,5..])
					where 
						primes' (x:xs) = x : primes' (filter (notDivisorOf x) xs)
						notDivisorOf d n = n `mod` d /= 0

problem_4 = maximum (filter isPalindromic (concatMap comb nums))
	where
		isPalindromic s = show s == reverse (show s)
		comb x = map (*x) nums
		nums = [100..999]

problem_5 = foldr1 lcm [1..20]

problem_6 = abs (sum_squares ins - squared_sums ins)
	where
		sum_squares x = sum ( squares (x) )
			where
				squares y = map square y
					where
						square z = z * z
		squared_sums x = sum (x) * sum (x)
		ins = [1..100]

problem_7 = primes !! 10000
	where
		primes :: [Integer]
		primes = 2: 3: sieve (tail primes) [5,7..]
		   	where 
		    	sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
		            where (h,~(_:t)) = span (< p*p) xs

problem_8 = maximum (map product slices)
	where
		slices = map rotatingSlice iters
			where
				iters = [0,1..length(numString) - 5]
				rotatingSlice x = slice x 5 (getListDigits numString)
					where
						slice from n xs = take n (drop from xs)
						getListDigits = map digitToInt
		numString = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

problem_9 s = mult3Tuple $ head $ take 1 [ (a,b,c) | c <- [1..1000], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == s]
	where
		mult3Tuple (a, b, c) = a*b*c

problem_10 = sum $ takeWhile (<2000000) primes
	where
		primes :: [Integer]
		primes = 2: 3: sieve (tail primes) [5,7..]
		   	where 
		    	sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
		            where (h,~(_:t)) = span (< p*p) xs