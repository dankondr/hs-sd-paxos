{-# LANGUAGE InstanceSigs #-}

module Paxos where

import Utils (splitInts)

data Round = Round {roundNode :: Int, roundNum :: Int} deriving (Eq, Ord)

parseRound :: String -> Round
parseRound s = Round {roundNode = id, roundNum = num}
  where
    id = head $ splitInts ';' s
    num = head . tail $ splitInts ';' s

instance Show Round where
  show :: Round -> String
  show r = show (roundNode r) <> ";" <> show (roundNum r)

data PaxosAction = Accept Round (Maybe Int) | Promise Round (Maybe Int) | Commit

data Acceptor = Acceptor {highestPromise :: Maybe Round, acceptedValue :: Maybe Int} deriving (Show)

handlePrepare :: Acceptor -> Round -> (Acceptor, Maybe PaxosAction)
handlePrepare state newPromise =
  case highestPromise state of
    Nothing -> (state', Just promise)
    Just promised ->
      if promised < newPromise
        then (state', Just promise)
        else (state, Nothing)
  where
    state' = Acceptor {highestPromise = Just newPromise, acceptedValue = acceptedValue state}
    promise = Promise newPromise Nothing

handleAccept :: Acceptor -> Round -> Int -> (Acceptor, Maybe PaxosAction)
handleAccept state promise value =
  case highestPromise state of
    Nothing -> (state', Just accept)
    Just promised -> case compare promised promise of
      LT -> (state', Just accept)
      EQ -> case acceptedValue state of
        Nothing -> (state', Nothing)
        Just accepted ->
          if accepted == value
            then (state, Nothing)
            else (state', Just accept)
      GT -> (state, Nothing)
  where
    state' = Acceptor {highestPromise = Just promise, acceptedValue = Just value}
    accept = Accept promise (Just value)
