module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime
import System.IO -- for readCalendar


-- Exercise 6
data Calendar = Calendar {
    prodid :: String,
    version :: Version,
    events :: [Event]
}
    deriving (Eq, Ord, Show)

data Version = V2
    deriving (Eq, Ord, Show)

data Event = Event{
    dtstamp :: DateTime,
    id :: String,
    start :: DateTime,
    end :: DateTime,
    desc :: Maybe String,
    sum :: Maybe String,
    loc :: Maybe String
}
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = do
    file <- openFile fp ReadMode -- readMode is enough, since no manipulation is needed
    hSetNewlineMode file noNewlineTranslation -- advice of the document
    content <- hGetContents file
    return $ recognizeCalendar content

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar p v evs) = 
    "BEGIN:VCALENDAR\r\nVERSION:" ++ v 
    ++ "\r\nPRODID:" ++ p ++ "\r\n" 
    ++ concatMap printEvent evs 
    ++ "END:VCALENDAR\r\n"

-- Used in ex. 9 for printing optional elements.
printMaybeString :: Maybe a -> String-> String
printMaybeString Nothing = ""
printMaybeString (Just x) is = "\r\n" ++ is ++ x

printEvent (Event dtstamp id start end desc sum loc) = "BEGIN:VEVENT" 
    ++ "\r\nUID:" ++ id 
    ++ "\r\nDTSTAMP:" ++ printDateTime dtstamp
    ++ "\r\nDTSTART:" ++ printDateTime start
    ++ "\r\nDTEND:" ++ printDateTime end
    ++ printMaybeString desc "DESCRIPTION:" 
    ++ printMaybeString sum "SUMMARY:"
    ++ printMaybeString loc "LOCATION:"
    ++ "\r\nEND:VEVENT\r\n"