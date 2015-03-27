module HW2.LogAnalysis
( parseMessage
, parse
, insert
, build
, inOrder
, whatWentWrong
) where

import Provided.Log
import Data.List (foldl')

mcode :: String -> Int
mcode msg = read code
  where (code:_) = words msg

msgpart :: String -> String
msgpart msg = unwords rest
  where (_:rest) = words msg

parseMessage :: String -> LogMessage
parseMessage ('E':' ':xs) = LogMessage (Error num) priority msg
  where (num_s:priority_s:msg_ws) = words xs
        num = read num_s
        priority = read priority_s
        msg = unwords msg_ws

parseMessage ('I':' ':xs) = LogMessage Info (mcode xs) (msgpart xs)
parseMessage ('W':' ':xs) = LogMessage Warning (mcode xs) (msgpart xs)
parseMessage _ = Unknown "This is not in the right format"

parse :: String -> [LogMessage]
parse = map parseMessage . lines

timestamp :: LogMessage -> Maybe TimeStamp
timestamp (LogMessage _ stamp _) = Just stamp
timestamp _ = Nothing

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left rootNodeMessage right)
  | msgTime < nodeTime = Node (insert msg left) rootNodeMessage right
  | msgTime > nodeTime = Node left rootNodeMessage (insert msg right)
  | msgTime == nodeTime = Node left msg right
  where msgTime = timestamp msg
        nodeTime = timestamp rootNodeMessage
insert _ _ = Leaf
-- insert msg _ = Node Leaf msg Leaf

-- Node Leaf (LogMessage (Error 9000) 2 "") Leaf

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf
-- build = foldl' (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map msgstring $ filter isError $ inOrder $ build msgs
  where isError (LogMessage (Error severity) _ _)
          | severity > 49 = True
          | otherwise = False
        isError _ = False
        msgstring (LogMessage _ _ str) = str
        msgstring (Unknown str) = str
