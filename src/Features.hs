module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes
import Data.Time as DT
import Data.Fixed
import Data.List as DL
import Data.Maybe
import Data.Char

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

zz = ppMonth (Year 1997) (Month 7) aa'

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m c = render $ makeCalender (height+1) $ getRows $ makeBoxes content height
    where
    size = monthsize y m 
    content = map sort $ getCalenderContent (getAllStartEnd y m c) size
    height = getTopHight content 

                                --  (day,(hour,minuet))
type EventIndexTime  =  (Maybe (Int,(Int,Int)),Maybe (Int,(Int,Int))) -- contains info over the day/time an event starts and ends (if start / end in other month gives nothing)
    
getStartEnd ::Month -> Event -> EventIndexTime -- puts an event is Event Index/Time format
getStartEnd m e = ( getIndexTime $ start e , getIndexTime $ end e )
  where
    getIndexTime dt |month (date dt )== m = Just (iDay dt,hourmin dt)
                    | otherwise = Nothing
    iDay = runDay . day . date
    hourmin dt = (runHour$ hour$ time dt,runMinute $minute$time dt)

getAllStartEnd :: Year -> Month -> Calendar -> [EventIndexTime] -- gets the eventdata of all events that occur in the given month
getAllStartEnd y m c = sort $ foldr getEventInd [] (events c)
  where getEventInd e xs |((month.date.start) e ==m && (year.date.start) e == y) ||((month.date.end) e ==m && (year.date.end) e == y) = getStartEnd m e : xs
                         | otherwise = xs

type OccuringEvent = (Int,EventIndexTime)  --  keeps track of the hight of an currently occurring event

type Content = (Int,String) -- the writen string and it's height position 

getCalenderContent:: [EventIndexTime] -> Int -> [[Content]]
getCalenderContent eventdata size =  reverse $  getDayContent 1 eventdata [] [[]]--getDayContent' 1 (setPos 1 $ filter  ( isNothing . fst ) eventdata) 
    where
        getDayContent :: Int -> [EventIndexTime] -> [OccuringEvent] ->[[Content]] -> [[Content]]
        getDayContent d [] currentEvs (ys:yss) | d > size = yss
                                               | otherwise = getDayContent (d+1) [] (updtCur d currentEvs) ([]:(ys ++ getOcc d currentEvs):yss)
        getDayContent 1 (x@(Nothing,_):xs) currentEvs (ys:yss) = getDayContent 1 xs (sort ((lowestPos 1 currentEvs,x):currentEvs)) (((getContent 1 (lowestPos 1 currentEvs) x) : ys):yss )
        getDayContent d (x@(Just (i,_),_):xs) currentEvs (ys:yss) | d > size = yss
                                                                  | i /= d = getDayContent (d+1) (x:xs) (updtCur d currentEvs) ([]:(ys ++ getOcc d currentEvs):yss)
                                                                  | otherwise = getDayContent d xs (sort ((lowestPos 1 currentEvs,x):currentEvs)) (((getContent d (lowestPos 1 currentEvs) x) : ys):yss)
        lowestPos i [] = i
        lowestPos i ((x,_):xs) | i == x = lowestPos (i+1) xs -- gives the lowest position that is available at a date
                               | otherwise = i
        getContent :: Int -> Int -> EventIndexTime -> Content  --gives the string(+index) of the events that start on the given day             
        getContent d p (Nothing,Just(d2,t)) | d == d2 = (p,  halfLine++ getTime t)
                                            | otherwise =  (p,fulLine )
        getContent d p (Just(d1,t1), Just( d2,t2)) |d1 == d2 = (p, getTime t1 ++ "-"++ getTime t2 )
                                                   | otherwise = (p, getTime t1 ++ halfLine)
        getContent _ p (Just(_,t),Nothing) = (p,getTime t ++halfLine) 
        getTime (h, m) = twodig h ++"." ++ twodig m 
        twodig n | n <10 = '0' : show n  
                 | otherwise = show n
        updtCur d = filter (\x -> isNothing(snd$snd x) || fst (fromJust (snd$snd x)) /= d) -- removes the events that end on the given day
        getOcc :: Int -> [OccuringEvent] ->[Content] -- gives the string(+index) of events that are occuring that started at a earlier date
        getOcc _ [] = []
        getOcc 1 _ = []
        getOcc d ((p,(s,e)):xs) | isJust s && fst (fromJust s) == d = getOcc d xs
                                | isNothing e || (fst (fromJust e) /= d ) = (p,fulLine) :getOcc d xs
                                | otherwise = (p, halfLine ++ getTime ( snd $ fromJust e )) : getOcc d xs

getTopHight :: [[Content]] -> Int 
getTopHight  = maximum . map fst . concat 

--qq = render $ makeBoxes asa (getTopHight asa)

makeBoxes :: [[Content]] -> Int -> [Box]
makeBoxes cont height = getBoxes cont 1
  where  
    getBoxes [] _ = []
    getBoxes (x:xs) d = makeBox d 0 x : getBoxes xs (d+1)
    makeBox :: Int -> Int -> [Content] -> Box
    makeBox d 0 xs| d< 10 = text (show d ++ replicate 10 ' ') // makeBox d 1 xs
                  | otherwise = text (show d ++ replicate 9 ' ')// makeBox d 1 xs
    makeBox d i [] = emptyBox (height - i + 1) 11
    makeBox d i ((p,c):xs) |i == p = text c // makeBox d (i+1) xs
                           | otherwise = emptyLine // makeBox d (i+1) xs
   

getRows :: [a] -> [[a]] -- distibutes the boxes in rows of 7
getRows [] = []
getRows xs = take 7 xs : getRows (drop 7 xs )


makeCalender :: Int -> [[Box]] -> Box
makeCalender  height  =punctuateV top bigLine . makeRow 
   where 
       makeRow [] = []
       makeRow (xs:xss) = punctuateH center1 midsection xs : makeRow xss
       midsection =  vcat top $ replicate height $ text "|"

fulLine = replicate 11 '-'
halfLine = replicate 6 '-'
emptyLine = text $ replicate 11 ' '
bigLine = text $ intercalate "+" $ replicate 7 fulLine 


monthsize :: Year -> Month -> Int
monthsize y m = getsize 28 --gives days in a month 
    where getsize i | validDate (Date y m (Day (i +1))) = getsize (i +1)
                    | otherwise = i
