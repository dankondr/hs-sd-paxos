{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import qualified Network.Wreq as Wreq
import System.Console.CmdArgs (cmdArgs)
import System.IO (withFile)
import qualified Web.Scotty as Scotty

data Args = Args {port :: Int, clusterPortsFilePath :: String} deriving (Show, Data, Typeable)

args :: Args
args = Args {port = 3000, clusterPortsFilePath = "paxos_cluster.txt"}

getClusterPorts :: String -> IO [Int]
getClusterPorts path = do
  contents <- readFile path
  return $ map (\x -> read x :: Int) $ lines contents

data PaxosResponse = PaxosResponse
  {logIndex :: Int, yourRound :: Int, currentRound :: Int, currentAccepted :: Maybe Int}
  deriving (Show, Generic, ToJSON, FromJSON)

main :: IO ()
main = do
  args <- cmdArgs args

  clusterPorts <- getClusterPorts $ clusterPortsFilePath args
  putStrLn $ "Cluster ports: " <> show clusterPorts

  let quorumSize = ceiling (toRational (length clusterPorts) / 2)
  putStrLn $ "Quorum size: " <> show quorumSize

  forkIO . forever $ do
    putStr "Input a value to propose: "
    proposeValue <- readLn :: IO Int
    print proposeValue

  Scotty.scotty (port args) $ do
    Scotty.get "/propose" $ do
      logIndex <- Scotty.param "logIndex" :: Scotty.ActionM Int
      round <- Scotty.param "round" :: Scotty.ActionM Int
      Scotty.json $ PaxosResponse {logIndex = logIndex, yourRound = round, currentRound = round, currentAccepted = Nothing}

    Scotty.get "/accept" $ do
      logIndex <- Scotty.param "logIndex" :: Scotty.ActionM Int
      round <- Scotty.param "round" :: Scotty.ActionM Int
      value <- Scotty.param "value" :: Scotty.ActionM Int
      Scotty.json $ PaxosResponse {logIndex = logIndex, yourRound = round, currentRound = round, currentAccepted = Just value}

    Scotty.get "/commit" $ do
      logIndex <- Scotty.param "logIndex" :: Scotty.ActionM Int
      Scotty.text "Ok"
