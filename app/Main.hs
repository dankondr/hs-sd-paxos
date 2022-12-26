{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, forkIO, newMVar, putMVar, takeMVar)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (forever)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Data (Data, Typeable)
import GHC.Exts (fromString)
import GHC.Generics (Generic)
import qualified Network.Wreq as Wreq
import Paxos ( Acceptor(..), PaxosAction(Accept), Round(..) )
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
  {yourRound :: Int, currentRound :: Int, currentAccepted :: Maybe Int}
  deriving (Show, Generic, ToJSON, FromJSON)

proposer :: MVar Round -> [Int] -> Int -> MVar Acceptor -> IO ()
proposer round clusterPorts quorumSize paxos =
  forever $ do
    putStr "Input a value to propose: "
    proposeValue <- readLn :: IO Int

    p <- takeMVar paxos
    r <- takeMVar round

    case acceptedValue p of
      Just accepted -> putStrLn $ "Consensus value is already chosen: " <> show accepted
      Nothing -> do
        result <- propose r proposeValue clusterPorts quorumSize
        case result of
          Nothing -> putStrLn "No result of proposal"
          Just result -> case result of
            Accept acceptedRound acceptedValue -> case acceptedValue of
              Just value -> return ()
              Nothing -> return ()
            _ -> return ()

    putMVar paxos p
    putMVar round r

propose :: Round -> Int -> [Int] -> Int -> IO (Maybe PaxosAction)
propose round value clusterPorts quorumSize = do
  let responses = map propose clusterPorts
  return Nothing
  where
    propose :: Int -> IO (Maybe PaxosAction)
    propose port = do
      let opts = Wreq.defaults & Wreq.param "round" .~ [fromString . show $ round]
      r <- Wreq.asJSON =<< Wreq.getWith opts ("localhost:" <> show port <> "/propose") :: IO (Wreq.Response PaxosResponse)
      print $ r ^. Wreq.responseBody
      print "BODY HERE"
      return Nothing

main :: IO ()
main = do
  args <- cmdArgs args

  let nodeId = port args
  putStrLn $ "Current node id: " <> show nodeId

  clusterPorts <- getClusterPorts $ clusterPortsFilePath args
  putStrLn $ "Cluster ports: " <> show clusterPorts

  let quorumSize = ceiling (toRational (length clusterPorts) / 2)
  putStrLn $ "Quorum size: " <> show quorumSize

  paxos <- newMVar (Acceptor {highestPromise = Nothing, acceptedValue = Nothing})
  currentRound <- newMVar (Round {roundNode = nodeId, roundNum = 0})

  forkIO $ proposer currentRound clusterPorts quorumSize paxos

  Scotty.scotty (port args) $ do
    Scotty.get "/propose" $ do
      round <- Scotty.param "round" :: Scotty.ActionM Int
      liftIO $ do
        p <- takeMVar paxos
        print $ show p
        putMVar paxos p
      Scotty.json $ PaxosResponse {yourRound = round, currentRound = round, currentAccepted = Nothing}

    Scotty.get "/accept" $ do
      round <- Scotty.param "round" :: Scotty.ActionM Int
      value <- Scotty.param "value" :: Scotty.ActionM Int
      Scotty.json $ PaxosResponse {yourRound = round, currentRound = round, currentAccepted = Just value}

    Scotty.get "/commit" $ do
      Scotty.text "Ok"
