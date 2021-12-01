module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes
import Data.Time as DT

-- Exercise 10
-- prodid :: String,
-- version :: Version,
-- events :: [Event]
countEvents :: Calendar -> Int
countEvents (Calendar _ _ evs) = length evs

findEvents :: DateTime -> Calendar -> [Event]
findEvents d (Calendar _ _ []) = []
findEvents d (Calendar id v (ev:evs)) 
    | d >= startEvent && d < endEvent = ev : findEvents d (Calendar id v evs) -- can compare because it is of Eq instance
    | otherwise = findEvents d (Calendar id v evs)
    where
        startEvent = start ev
        endEvent = end ev
        
checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ _ []) = False
checkOverlapping (Calendar id v events@(ev:evs)) = any p events || checkOverlapping (Calendar id v evs)
    where
        -- if an event's start or end is within the timeframe of a given event.
        p ev' = between (start ev) (start ev') (end ev) || between (start ev) (end ev') (end ev)
        between x y z | x < y = y < z | otherwise = False

timeSpent :: String -> Calendar -> Int
timeSpent sum (Calendar id v events@(ev:evs)) = undefined -- filter (filterEvent sum) events -- list of events with given summary

-- date :: Date
-- , time :: Time
-- , utc  :: Bool

-- data Date = Date { year  :: Year
--                  , month :: Month
--                  , day   :: Day }

-- newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
-- newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
-- newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)
durationOfEvent :: Event -> Integer
durationOfEvent (Event _ _ (DateTime sDate@(Date (Year sYear) (Month sMonth) (Day sDay)) (Time (Hour sHour) (Minute sMinute) (Second sSecond)) _) (DateTime eDate@(Date (Year eYear) (Month eMonth) (Day eDay)) (Time (Hour eHour) (Minute eMinute) (Second eSecond)) _) _ _ _) = differenceInDays
    where
        differenceInDays = abs $ fromInteger $ DT.diffDays startDay endDay
        --
        startDay :: DT.Day
        startDay = DT.fromGregorian (toInteger sYear) sMonth sDay
        endDay :: DT.Day
        endDay = DT.fromGregorian (toInteger eYear) eMonth eDay
        startTimeSecond = (sHour * 3600) + (sMinute * 60) + sSecond
        endTimeSecond = (eHour * 3600) + (eMinute * 60) + eSecond
        ---
        startTime :: DT.UTCTime
        startTime = DT.UTCTime startDay $ DT.secondsToDiffTime startTimeSecond
        endTime :: DT.UTCTime
        endTime = DT.UTCTime endDay $ DT.secondsToDiffTime endTimeSecond
        --
        diffTime = DT.diffUTCTime startTime endTime


filterBySummary :: String -> Event -> Bool
filterBySummary s (Event _ _ _ _ _ sum' _) = case sum' of
    Nothing -> False
    Just x  -> x == s

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

