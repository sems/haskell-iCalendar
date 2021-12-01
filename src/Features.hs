module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes
import Data.Time as DT
import Data.Fixed
import Data.List as DL

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

-- Returns 0 if there are no matches
-- Will round to lower in minutes.
timeSpent :: String -> Calendar -> Int
timeSpent sum (Calendar id v events) = diffInSeconds `div` 60 
    where
        diffInSeconds = DL.sum $ map durationOfEvent filteredEvents  
        filteredEvents = filter (filterBySummary sum) events -- list of events with given summary

durationOfEvent :: Event -> Int
durationOfEvent (Event _ _ (DateTime sDate sTime _) (DateTime eDate eTime _) _ _ _) = floor $ abs diffTime -- abs is needed because it is negative as default and floor to trim trailing zero's
    where
        startTime :: DT.UTCTime
        startTime = DT.UTCTime (toDay sDate) (timeInDiffTime sTime)
        endTime :: DT.UTCTime
        endTime = DT.UTCTime (toDay eDate) (timeInDiffTime eTime) 
        diffTime :: Pico
        diffTime = DT.nominalDiffTimeToSeconds $ DT.diffUTCTime startTime endTime

timeInDiffTime :: Time -> DT.DiffTime
timeInDiffTime (Time (Hour h) (Minute m) (Second s)) = DT.secondsToDiffTime seconds
    where 
        seconds :: Integer -- time in seconds after midnight.
        seconds = toInteger $ (h * 3600) + (m * 60) + s

toDay :: Date -> DT.Day
toDay (Date (Year y) (Month m) (Day d)) = DT.fromGregorian (toInteger y) m d

filterBySummary :: String -> Event -> Bool
filterBySummary s (Event _ _ _ _ _ sum' _) = case sum' of -- needed to use full paternmatch because 'sum' from prelude is also used.
    Nothing -> False
    Just x  -> x == s

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

