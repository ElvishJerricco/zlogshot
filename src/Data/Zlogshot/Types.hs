{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Zlogshot.Types where

import           Data.Some
import           Text.Read                                ( readMaybe )

class Dataset ds where
  datasetName :: ds -> String

-- Technically encompasses zvols as well
newtype FileSystem = FileSystem { unFileSystem :: String }
  deriving Show

instance Dataset FileSystem where
  datasetName = unFileSystem

data Snapshot = Snapshot
  { snapFileSystem :: FileSystem
  , snapBackup :: Backup
  } deriving Show

instance Dataset Snapshot where
  datasetName snap =
    datasetName (snapFileSystem snap) ++ "@" ++ unBackup (snapBackup snap)

newtype Backup = Backup { unBackup :: String }
  deriving Show

newtype Generation = Generation { unGeneration :: Integer }
  deriving (Show, Eq, Ord, Num)

data SnapshotRange = SnapshotRange
  { source :: Maybe Backup
  , target :: Snapshot
  } deriving Show

data Property a where
  -- The generation that was current when this backup was made. Mostly
  -- just record keeping; this information is never actually recalled
  -- by zlogshot.
  BackupGeneration :: Property Generation
  -- When this is the current generation, delete this backup
  BackupExpiration :: Property Generation
  -- The latest backup stored on this dataset
  LatestBackup :: Property Backup
  -- The current generation of the backups on this dataset
  CurrentGeneration :: Property Generation
  -- The mountpoint of a dataset
  MountPoint :: Property String

data SnapshotDepth = Recursive | Shallow
  deriving (Show, Eq)

relativeSnapshotName :: Backup -> String
relativeSnapshotName (Backup b) = "@" ++ b

propertyName :: Property a -> String
propertyName BackupGeneration  = "org.zlogshot:backup_generation"
propertyName BackupExpiration  = "org.zlogshot:backup_expiration"
propertyName LatestBackup      = "org.zlogshot:latest_backup"
propertyName CurrentGeneration = "org.zlogshot:current_generation"
propertyName MountPoint        = "mountpoint"

propertyAssignment :: Property a -> a -> String
propertyAssignment p a = propertyName p ++ "=" ++ case (p, a) of
  (BackupGeneration , Generation g) -> show g
  (BackupExpiration , Generation g) -> show g
  (LatestBackup     , Backup b    ) -> b
  (CurrentGeneration, Generation g) -> show g
  (MountPoint       , m           ) -> m

parsePropertyName :: String -> Maybe (Some Property)
parsePropertyName s =
  let ans = case s of
        "org.zlogshot:backup_generation"  -> Just (This BackupGeneration)
        "org.zlogshot:backup_expiration"  -> Just (This BackupExpiration)
        "org.zlogshot:latest_backup"      -> Just (This LatestBackup)
        "org.zlogshot:current_generation" -> Just (This CurrentGeneration)
        "mountpoint"                      -> Just (This MountPoint)
        _                                 -> Nothing
  in  case ans of -- redundant case for -Wincomplete-patterns reminders
        Nothing                       -> ans
        Just (This BackupGeneration ) -> ans
        Just (This BackupExpiration ) -> ans
        Just (This LatestBackup     ) -> ans
        Just (This CurrentGeneration) -> ans
        Just (This MountPoint       ) -> ans

parsePropertyValue :: Property a -> String -> Maybe a
parsePropertyValue BackupGeneration  s = Generation <$> readMaybe s
parsePropertyValue BackupExpiration  s = Generation <$> readMaybe s
parsePropertyValue LatestBackup      s = Just $ Backup s
parsePropertyValue CurrentGeneration s = Generation <$> readMaybe s
parsePropertyValue MountPoint        s = Just s
