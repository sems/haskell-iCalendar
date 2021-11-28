module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import Data.Char
import Data.Maybe

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

parseDate :: Parser Char Date
parseDate = Date <$> (Year <$> parse4Digits)
                 <*> (Month <$> parse2Digits)
                 <*> (Day <$> parse2Digits)

parseTime :: Parser Char Time
parseTime = Time <$> (Hour <$> parse2Digits) 
                 <*> (Minute <$> parse2Digits)  
                 <*> (Second <$> parse2Digits) 

-- From p0 - refresh
parse2Digits :: Parser Char Int
parse2Digits = (\x y -> 10 * x + y) <$> newdigit' <*> newdigit'

-- adjusted from p0 --refresh
parse4Digits :: Parser Char Int
parse4Digits = (\w x y z -> 1000 * w + 100 * x + 10 * y + z) <$> newdigit' <*> newdigit' <*> newdigit' <*> newdigit'

-- From the lecture notes
newdigit' :: Parser Char Int 
newdigit' = f <$> satisfy isDigit
    where 
        f c = ord c - ord '0'

-- Exercise 1

input :: String
input = "19970610T172345Z"

parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate 
                         <* symbol 'T'
                         <*> parseTime 
                         <*> (True <$ symbol 'Z' <|> False <$ epsilon) -- Parser Char Bool

-- parse parseDateTime input
-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p input = getOutput (parse p input)
    where 
        getOutput [] = Nothing
        getOutput ((x,[]):_) = Just x
        getOutput (_:ys) = getOutput ys

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime date time bool) = printDate date ++ "T" ++printTime  time ++ printBool bool
    where
 	    printDate (Date (Year y) (Month m) (Day d)) = show' y ++ show' m ++ show' d
	    printTime (Time (Hour h) (Minute m) (Second s)) = show' h++ show' m ++ show' s
	    printBool False = []
	    printBool True = "Z"
        
show' i | i < 10 = '0': show i -- alt version of show that keeps the string length consistant
        | otherwise = show i

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime  s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
