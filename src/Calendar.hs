module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime
import System.IO -- for readCalendar
import Data.Maybe
import Data.List


-- Exercise 6
data Calendar = Calendar {
    version :: Version,
    prodid :: String,
    events :: [Event]
}
    deriving (Eq, Ord, Show)

data Version = V2
    deriving (Eq, Ord)

instance Show Version where
    show V2 = "VERSION:2.0"

data Event = Event{
    dtstamp :: DateTime,
    uid :: String,
    start :: DateTime,
    end :: DateTime,
    desc :: Maybe String,
    sum :: Maybe String,
    loc :: Maybe String
}
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token 
    = BeginT |
    ProdidT String |
    VersionT Version |
    DtstampT DateTime |
    UidT String |
    StartT DateTime |
    DtEndT DateTime |
    DescT String |
    SumT String |
    LocT String |
    EndT |
    Fail
  deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = some (choice [parseBeginT, parseProIdT, parseVersT, parseDtStampT, parseUidT, parseStartT, parseDtEndT, parseDescT, parseSumT, parseLocT, parseEndT])

parseBeginT :: Parser Char Token
parseBeginT = pack (token "BEGIN:") ( BeginT <$ some (satisfy (/= '\r'))) (token "\r\n")
parseProIdT = pack (token "PRODID:") ( ProdidT <$> some (satisfy (/= '\r'))) (token "\r\n")
parseVersT = pack (token "VERSION:") ( VersionT V2 <$ some (satisfy (/= '\r'))) (token "\r\n")
parseDtStampT = pack (token "DTSTAMP:") ( getdt <$> some (satisfy (/= '\r'))) (token "\r\n")
    where getdt s = maybe Fail DtstampT (run parseDateTime s)
parseUidT =  pack (token "UID:") (UidT <$> some (satisfy (/= '\r'))) (token "\r\n")
parseStartT =  pack (token "DTSTART:") (getdt <$> some (satisfy (/= '\r'))) (token "\r\n")
    where getdt s = maybe Fail StartT (run parseDateTime s)
parseDtEndT  =  pack (token "DTEND:") (getdt <$> some (satisfy (/= '\r'))) (token "\r\n")
    where getdt s = maybe Fail DtEndT (run parseDateTime s)
parseDescT=  pack (token "DESCRIPTION:") (DescT <$> some (satisfy (/= '\r'))) (token "\r\n")
parseSumT =  pack (token "SUMMARY:") (SumT <$> some (satisfy (/= '\r'))) (token "\r\n")
parseLocT =  pack (token "LOCATION:") (LocT <$> some (satisfy (/= '\r'))) (token "\r\n")
parseEndT = pack (token "END:") ( EndT <$ some (satisfy (/= '\r'))) (token "\r\n")

aa = recognizeCalendar "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"

parseCalendar :: Parser Token Calendar
parseCalendar = pack (symbol BeginT) (Calendar <$> parseVers <*> parseProId <*> many parseEvent ) (symbol EndT)


parseEvent = pack (symbol BeginT) ( getEvent <$> (sort <$> many (satisfy (/= EndT))) )(symbol EndT)
 

--getEvent = Event <$> parseDtStamp <*> parseUid <*> parseStart <*> parsedtEnd <*> parseDesc <*> parseSum  <*> parseLoc


getEvent (DtstampT a: UidT b: StartT c: DtEndT d: xs) = f $ getDesc xs
            where getDesc [] = [Nothing,Nothing,Nothing]
                  getDesc (DescT y:ys) = getSum [Just y] ys
                  getDesc (_ :ys) = getSum [Nothing] ys
                  getSum x [] = x ++ [Nothing,Nothing]
                  getSum x (SumT y : ys) = getLoc (x ++ [Just y]) ys
                  getSum x (_ :ys) = getLoc (x ++ [Nothing]) ys
                  getLoc x [] = x ++ [Nothing]
                  getLoc x [LocT y] = x ++ [Just y]
                  f [x,y,z]= Event a b c d x y z

parseVers = V2 <$ symbol (VersionT V2)
parseProId = (\(ProdidT x) -> x) <$> anySymbol

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
printCalendar (Calendar v p evs) = 
    "BEGIN:VCALENDAR\r\nVERSION:" ++ show v 
    ++ "\r\nPRODID:" ++ p ++ "\r\n" 
    ++ concatMap printEvent evs 
    ++ "END:VCALENDAR\r\n"

-- Used in ex. 9 for printing optional elements.
printMaybeString :: Maybe String -> String -> String
printMaybeString Nothing _ = ""
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