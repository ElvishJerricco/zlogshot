{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import           Data.Dependent.Sum
import           Data.ZlogShot
import           Options.Applicative

data Config = Config
  { srcDataset :: FileSystem
  , destDataset :: FileSystem
  , label :: String
  , coefficient :: Integer
  } deriving Show

main :: IO ()
main = do
  Config { srcDataset, destDataset, label, coefficient } <- execParser opts

  let gen = Generation 1
  backup <- newBackupName label gen
  let snapshot = Snapshot {snapFileSystem = srcDataset, snapBackup = backup}

  makeExpiringSnapshot snapshot coefficient gen Recursive
  transfer TransferConfig
    { pipeRange = SnapshotRange {source = Nothing, target = snapshot}
    , pipeSnapshotDepth     = Recursive
    , pipeGeneration        = Generation 1
    , pipeDestination       = destDataset
    , pipeReceiveProperties = [ MountPoint ==> "none"
                              , LatestBackup ==> backup
                              , CurrentGeneration ==> gen
                              ]
    }
 where
  opts = info (helper <*> argParser) $ mconcat
    [fullDesc, progDesc "", header "zlogshot-setup - Setup a zlogshot backup"]
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
        "Label for the backup destination. Necessary to have multiple destinations. Needs to be specified with zlogshot-create too."
      ]
    coefficient <- option auto $ mconcat
      [ long "coefficient"
      , short 'k'
      , showDefault
      , value 10
      , metavar "COEFFICIENT"
      , help "Tuning coefficient. Higher means backups survive longer."
      ]
    return Config {srcDataset , destDataset , label, coefficient }
