{-# LANGUAGE InstanceSigs #-}

module Paxos where

import Utils (splitInts)

data Round = Round {roundNode :: Int, roundNum :: Int}

parseRound :: String -> Round
parseRound s = Round {roundNode = id, roundNum = num}
  where
    id = head $ splitInts ';' s
    num = head . tail $ splitInts ';' s

instance Show Round where
  show :: Round -> String
  show r = show (roundNode r) <> ";" <> show (roundNum r)

data Acceptor = Acceptor {round :: (Int, Int), value :: Maybe Int}

data PaxosLog = PaxosLog {}
