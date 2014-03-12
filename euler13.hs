-- Work out the first ten digits of the sum of the following one-hundred 50-digit numbers...

import Control.Monad

getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile

first12 :: String -> String
first12 s = take 12 s

truncs = map first12 strings

answer = sum (map readInt truncs)
