--If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

--If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


--NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

chars :: Integer -> String
chars 1 = "one"
chars 2 = "two"
chars 3 = "three"
chars 4 = "four"
chars 5 = "five"
chars 6 = "six"
chars 7 = "seven"
chars 8 = "eight"
chars 9 = "nine"
chars 10 = "ten"
chars 11 = "eleven"
chars 12 = "twelve"
chars 13 = "thirteen"
chars 14 = "fourteen"
chars 15 = "fifteen"
chars 16 = "sixteen"
chars 17 = "seventeen"
chars 18 = "eighteen"
chars 19 = "nineteen"
chars 20 = "twenty"
chars 30 = "thirty"
chars 40 = "forty"
chars 50 = "fifty"
chars 60 = "sixty"
chars 70 = "seventy"
chars 80 = "eighty"
chars 90 = "ninety"
chars 100 = "one hundred"
chars 1000 = "one thousand"

chars n
	| n > 100 = (chars (n `quot` 100))++" hundred"++(digits (n `rem` 100))
	| n > 10 = (chars (10*(n `quot` 10)))++" "++(chars (n `rem` 10))
		where
			digits n
				| n == 0 = ""
				| n <= 100 = " and "++chars(n)

stripChars :: String -> String -> String
stripChars = filter . flip notElem

listOfStrings = map chars [1..1000]

main = print $ length (stripChars " " (concat listOfStrings))
