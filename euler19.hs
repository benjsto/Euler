--You are given the following information, but you may prefer to do some research for yourself.

--1 Jan 1900 was a Monday.
--
--Thirty days has September,
--April, June and November.
--All the rest have thirty-one,
--Saving February alone,
--Which has twenty-eight, rain or shine.
--And on leap years, twenty-nine.
--
--A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
--
--How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

incDay :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
incDay (m,d,y,w) = (month (monthDayYear(m,d,y)), day (monthDayYear(m,d,y)), year (monthDayYear(m,d,y)), (w + 1)`mod`7)
	where 
		monthDayYear :: (Int,Int,Int) -> (Int,Int,Int)
		monthDayYear (m,d,y)
			| m `elem` [1,3,5,7,8,10] = if ((d+1)>31) then (m+1,1,y) else (m,d+1,y)
			| m `elem` [4,6,9,11] = if((d+1)>30) then (m+1,1,y) else (m,d+1,y)
			| (m == 2) && (leapYear y) = if((d+1)>29) then (m+1,1,y) else (m,d+1,y)
			| (m == 2) && not(leapYear y) = if((d+1)>28) then (m+1,1,y) else (m,d+1,y)
			| (m == 12) && (d==31) = (1,1,y+1)
			| otherwise = (m,d+1,y)
		month (m,_,_) = m
		day (_,d,_) = d
		year (_,_,y) = y
		leapYear :: Int -> Bool
		leapYear y
			| ((y`mod`4)==0)&&((y`mod`100)/=0) = True
			| ((y`mod`4)==0)&&((y`mod`400)==0) = True
			| otherwise = False

datesInRange :: [(Int,Int,Int,Int)]
datesInRange = filter inRange before2001
	where
		inRange (_,_,y,_) = if (y >= 1901) then True else False
		before2001 = takeWhile (\(_,_,y,_) -> if (y<=2000) then True else False) (iterate incDay (1,1,1900,1))

answer = length (filter sundayOnFirst datesInRange)
	where
		sundayOnFirst (_,d,_,w) = if ((d==1)&&(w==0)) then True else False

