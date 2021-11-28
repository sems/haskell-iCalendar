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
parsePrint s = fmap printDateTime $ run parseDateTime s


-- data DateTime = DateTime { date :: Date
--                          , time :: Time
--                          , utc  :: Bool 
-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime d t _) = validDate d && validTime t

-- data Date = Date { year  :: Year
--                  , month :: Month
--                  , day   :: Day }
validDate :: Date -> Bool
validDate dt@(Date y m _) = validYear y && validMonth m && validDay dt

validTime :: Time -> Bool
validTime (Time h m s) = validHour h && validMinute m && validSecond s

isLeapYear :: Year -> Bool
isLeapYear (Year y) | y `mod` 400 == 0 = True
                    | y `mod` 100 == 0 = False
                    | y `mod` 4   == 0 = True
                    | otherwise        = False

validDay :: Date -> Bool
validDay (Date y (Month m) (Day d)) | m == 2 && isLeapYear y = 1 <= d && d <= 29 -- Leap year and february
                                    | m == 2 = 1 <= d && d <= 28 -- february and no leap year
                                    | m == 4 || m == 6 || m == 9 || m == 11 = 1 <= d && d <= 30 -- all months with a maximimum of 30 days, source: https://en.wikipedia.org/wiki/Gregorian_calendar
                                    | otherwise = 1 <= d && d <= 31 -- all other with 31 days
validYear :: Year -> Bool
validYear (Year y) = 0 <= y

validMonth :: Month -> Bool
validMonth (Month m) = 1 <= m && m <= 23

validHour :: Hour -> Bool
validHour (Hour h) = 0 <= h && h <= 23

validMinute :: Minute -> Bool
validMinute (Minute m) = 0 <= m && m <= 59

validSecond :: Second -> Bool
validSecond (Second s) = 0 <= s && s <= 59