{-#OPTIONS_GHC -Wall #-}

module LogAnalitics where

import Log

testData :: Int -> String
testData x  | x == 1 = "E 2 562 help help"
            | x == 2 = "I 29 la la la"
            | x == 3 = "W 32 asd asd asd "
            | otherwise = ""

getSecond :: String -> Int
getSecond = read . head . tail . words

getThird :: String -> Int
getThird =  read . head . tail . tail . words


parseMessage :: String -> LogMessage
parseMessage "" = (Unknown "")
parseMessage str = case (head $ words str) of 
                        "I" -> (LogMessage Info (getSecond str) (unwords $ tail $ tail $ words str)) 
                        "E" -> (LogMessage (Error (getSecond str))  
                                (getThird str) 
                                (unwords $ tail $ tail $ tail $ words str))
                        "W" -> (LogMessage Warning (getSecond str) (unwords $ tail $ tail $ words str))
                        _ -> (Unknown str)

parse :: String -> [LogMessage]
parse str = map parseMessage $ lines str



insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt -- invariant guaranteed by build function starting with Leaf
insert logMes (Leaf) = (Node Leaf logMes Leaf) 
insert  lm@(LogMessage _ ts _) 
           (Node lhs innerlm@(LogMessage _ tstree _) rhs) 
                                                        | ts < tstree = (Node (insert lm lhs) innerlm rhs)
                                                        | ts > tstree = (Node lhs innerlm (insert lm rhs))

build :: [LogMessage] -> MessageTree
build lms = foldl (flip insert) Leaf lms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lhs lm rhs) = [lm] ++ (inOrder lhs) ++ (inOrder rhs)

filterSeverity :: LogMessage -> Bool
filterSeverity (LogMessage (Error severity) _ _) = severity > 50
filterSeverity _ = False

getTextFormLog :: LogMessage -> String
getTextFormLog (LogMessage _ _ text) = text 
getTextFormLog (Unknown text) = text 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map getTextFormLog $ filter filterSeverity lms

testBuild :: [LogMessage]
testBuild = [LogMessage Info 5053 "pci_id: con ing!",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled",LogMessage Info 4076 "verse.'",LogMessage Info 4764 "He trusts to you to set them free,",LogMessage Info 858 "your pocket?' he went on, turning to Alice.",LogMessage Info 898 "would be offended again.",LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)",LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And",LogMessage Info 3899 "hastily."]

-- testWhatWentWrong parse whatWentWrong "error.log"
-- Hacker - The Adriamt