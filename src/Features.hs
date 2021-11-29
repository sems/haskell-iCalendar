module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes


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
timeSpent = undefined

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

