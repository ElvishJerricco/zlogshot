{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.Dependent.Sum
import           Data.Maybe                               ( fromJust )
import           Data.Zlogshot
import           Options.Applicative
import           System.IO

data Config = Config
  { srcDataset :: FileSystem
  , destDataset :: FileSystem
  , label :: String
  , coefficient :: Integer
  } deriving Show

getMetadata :: FileSystem -> IO (Backup, Generation)
getMetadata destDataset = do
  b <- fromJust <$> zfsGet destDataset LatestBackup
  g <- fromJust <$> zfsGet destDataset CurrentGeneration
  return (b, g)

main :: IO ()
main = do
  Config { srcDataset, destDataset, label, coefficient } <- execParser opts
  hPutStrLn stderr
    $  "Backing up "
    ++ datasetName srcDataset
    ++ " to "
    ++ datasetName destDataset

  (oldBackup, currentGen) <- getMetadata destDataset

  let newGen = currentGen + 1
  newBackup <- newBackupName label newGen
  let newSnapshot =
        Snapshot {snapFileSystem = srcDataset, snapBackup = newBackup}

  hPutStrLn stderr $ "Creating new generation " ++ show newBackup

  makeExpiringSnapshot newSnapshot coefficient newGen Recursive

  transfer TransferConfig
    { pipeRange = SnapshotRange {source = Just oldBackup, target = newSnapshot}
    , pipeSnapshotDepth     = Recursive
    , pipeGeneration        = newGen
    , pipeDestination       = destDataset
    , pipeReceiveProperties = [ LatestBackup ==> newBackup
                              , CurrentGeneration ==> newGen
                              ]
    }

  -- We don't keep the history on the source dataset. We only keep the
  -- old backup as an incremental source for the send operation.
  zfsDestroy Recursive
    $ Snapshot {snapFileSystem = srcDataset, snapBackup = oldBackup}
 where
  opts = info (helper <*> argParser) $ mconcat
    [fullDesc, progDesc "", header "zlogshot-create - Create a zlogshot backup"]
  argParser = do
    srcDataset  <- FileSystem <$> argument str (metavar "<SOURCE DATASET>")
    destDataset <- FileSystem <$> argument str (metavar "<DETINATION DATASET>")
    label       <- strOption $ mconcat
      [ long "label"
      , short 'l'
      , showDefault
      , value "zlogshot"
      , metavar "LABEL"
      , help
        "Label for the backup destination. Necessary to have multiple destinations. Can be changed at will."
      ]
    coefficient <- option auto $ mconcat
      [ long "coefficient"
      , short 'k'
      , showDefault
      , value 10
      , metavar "COEFFICIENT"
      , help "Tuning coefficient. Higher means backups survive longer."
      ]
    return Config {srcDataset , destDataset , label , coefficient }
  -- argParser = do
  --   decksFile <- argument str (metavar "FILE")
  --   chartFile <- optional $ strOption $ mconcat
  --     [long "chart-output", short 'c', metavar "FILE", help "Output file for charts"]
  --   bins <- optional $ option auto $ mconcat
  --     [short 'b', metavar "DOUBLE", help "Bins in histograms"]
  --   aggregateRank <- option auto $ mconcat
  --     [ long "aggregate-rank"
  --     , short 'k'
  --     , metavar "INT"
  --     , help "Ranking level to use in aggregation."
  --     , showDefault
  --     , value 3
  --     ]
  --   profileMST <- switch
  --     $ mconcat [long "profile-mst", help "Evaluate the MST, then exit, producing no output."]
  --   return Args {}
