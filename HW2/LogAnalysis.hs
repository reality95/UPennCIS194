{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char

parseSpaces :: String -> String
parseSpaces [] = []
parseSpaces (c:cs)
    | isSpace(c)    = parseSpaces cs
    | otherwise     = [c] ++ cs


--The first one is the digit and the second one is the rest
parseDigits :: String -> (String, String)
parseDigits [] = ([],[])
parseDigits (c:cs)
    | isDigit(c)    = ([c] ++ (fst p), snd p)
    | otherwise     = ("", [c] ++ cs)
    where p = parseDigits cs

parseDigitsAndSpaces :: String -> (String,String)
parseDigitsAndSpaces = parseDigits . parseSpaces

data StampAndMessage = StampAndMessage Int String
                    | FailToParse
                    deriving (Show,Eq)

parseStampMessage :: String -> StampAndMessage
parseStampMessage s --The number must be separated by white chars from the message, the message must not be empty
    |num == [] || mess == [] || not (isSpace (mess !! 0)) = FailToParse
    |otherwise                                            = StampAndMessage (read num :: Int) (parseSpaces mess)
    where (num, mess) = parseDigitsAndSpaces s

--The type|white chars|Int if Error|white chars if Error|Time Stamp|white chars|Message
parseMessage :: String -> LogMessage
parseMessage ('E':s) 
    |numS1 == [] || s1 == FailToParse = Unknown ("E" ++ s) --cannot parse error type or stamp and message
    |otherwise                        = LogMessage tp num2 mess
    where   (numS1, s0) = parseDigitsAndSpaces s
            s1 = parseStampMessage s0
            StampAndMessage num2 mess = s1
            num1 :: Int
            num1 = read numS1
            tp :: MessageType
            tp = Error num1

parseMessage (c:s)
    |not (c == 'I' || c == 'W') || s0 == FailToParse  = Unknown ([c] ++ s) --cannot parse stamp or message
    |otherwise                                              = LogMessage tp num2 mess
    where   s0 = parseStampMessage s
            StampAndMessage num2 mess = s0
            tp :: MessageType
            tp = if c == 'I' then Info else Warning

parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) tree = tree
insert logM Leaf = Node Leaf logM Leaf
insert logM (Node left rootLogM right) 
        | tRootLogM > tLogM = Node (insert logM left) rootLogM right
        | otherwise         = Node left rootLogM (insert logM right)
        where LogMessage _ tRootLogM _ = rootLogM   --time for root
              LogMessage _ tLogM _     = logM       --time for new log message

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (logM : logsM) = insert logM (build logsM)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left rootLogM right) = (inOrder left) ++ [rootLogM] ++ (inOrder right)

sortLogMessages :: [LogMessage] -> [LogMessage]
sortLogMessages logsM = inOrder $ build logsM

verySevere :: LogMessage -> Bool
verySevere (LogMessage (Error err) _ _) = err > 50
verySevere _ = False

toString :: LogMessage -> String
toString (LogMessage _ _ mess) = mess
toString _ = undefined

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logsM = map toString (filter verySevere logsM)



