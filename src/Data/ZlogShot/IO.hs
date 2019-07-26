{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.ZlogShot.IO where

import           Control.Concurrent.Async
import           Control.Exception                        ( throwIO
                                                          , Exception
                                                          )
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Data.Dependent.Sum
import           Data.Functor.Identity
import           Data.Time
import           Data.ZlogShot.Types
import           System.Exit                              ( ExitCode(..) )
import           System.IO
import           System.Process

makeSnapshot :: Snapshot -> [DSum Property Identity] -> SnapshotDepth -> IO ()
makeSnapshot snap options sDepth =
  callProcess "zfs"
    $  ["snapshot"]
    ++ (options >>= \(p :=> Identity v) -> ["-o", propertyAssignment p v])
    ++ (guard (sDepth == Recursive) >> ["-r"])
    ++ [datasetName snap]

withCreateSendProcess
  :: SnapshotRange -> SnapshotDepth -> ((Handle, ProcessHandle) -> IO r) -> IO r
withCreateSendProcess r d f = withCreateProcess (createSendProcess r d)
  $ \_ (Just outHandle) _ sendPH -> f (outHandle, sendPH)

createSendProcess :: SnapshotRange -> SnapshotDepth -> CreateProcess
createSendProcess (SnapshotRange { source, target }) sDepth =
  (  proc "zfs"
  $  ["send"]
  ++ (guard (sDepth == Recursive) >> ["-R"])
  ++ maybe [] (\s -> ["-i", relativeSnapshotName s]) source
  ++ [datasetName target]
  ) { std_out = CreatePipe
    , std_in  = NoStream
    }

withCreateReceiveProcess
  :: FileSystem
  -> Handle
  -> [DSum Property Identity]
  -> (ProcessHandle -> IO r)
  -> IO r
withCreateReceiveProcess d h p f =
  withCreateProcess (createReceiveProcess d h p) $ \_ _ _ -> f

createReceiveProcess
  :: FileSystem -> Handle -> [DSum Property Identity] -> CreateProcess
createReceiveProcess dataset handle props =
  (proc
      "zfs"
      (  ["receive"]
      ++ (props >>= \(p :=> Identity v) -> ["-o", propertyAssignment p v])
      ++ [datasetName dataset]
      )
    )
    { std_in = UseHandle handle
    }

data TransferConfig = TransferConfig
  { pipeRange :: SnapshotRange
  , pipeSnapshotDepth :: SnapshotDepth
  , pipeGeneration :: Generation
  , pipeDestination :: FileSystem
  , pipeReceiveProperties :: [DSum Property Identity]
  }

data TransferException = TransferException
  deriving (Show, Eq)
instance Exception TransferException

transfer :: TransferConfig -> IO ()
transfer config = flip runContT return $ do
  (outHandle, sendPH) <- ContT
    $ withCreateSendProcess (pipeRange config) Recursive
  lift $ hSetBinaryMode outHandle True -- probably not necessary
  receivePH <- ContT $ withCreateReceiveProcess (pipeDestination config)
                                                outHandle
                                                (pipeReceiveProperties config)

  lift $ concurrently_ (waitForSuccess sendPH) (waitForSuccess receivePH)
 where
  waitForSuccess ph = waitForProcess ph >>= \case
    ExitFailure _ -> throwIO TransferException
    ExitSuccess   -> return ()

zfsGet :: Dataset dsType => dsType -> Property a -> IO (Maybe a)
zfsGet ds p = parsePropertyValue p . head . lines <$> readProcess
  "zfs"
  ["get", "-H", "-o", "value", propertyName p, datasetName ds]
  ""

zfsSet :: Dataset dsType => dsType -> [DSum Property Identity] -> IO ()
zfsSet ds ps =
  callProcess "zfs"
    $  ["set"]
    ++ [ propertyAssignment p v | p :=> Identity v <- ps ]
    ++ [datasetName ds]

doesDatasetExist :: Dataset dsType => dsType -> IO Bool
doesDatasetExist ds = do
  code <-
    withCreateProcess (proc "zfs" ["list", "-H", datasetName ds])
        { std_in  = NoStream
        , std_out = NoStream
        , std_err = NoStream
        }
      $ \_ _ _ -> waitForProcess
  return $ code == ExitSuccess

-- This function can really return just about anything, as long as
-- each backup has a unique name.
newBackupName :: String -> Generation -> IO Backup
newBackupName label (Generation gen) = do
  t <- formatTime defaultTimeLocale "%FT%T" <$> getCurrentTime
  return $ Backup $ label ++ "-" ++ t ++ "-" ++ show gen

zfsDestroy :: Dataset dsType => SnapshotDepth -> dsType -> IO ()
zfsDestroy sDepth ds =
  callProcess "zfs"
    $  ["destroy"]
    ++ (guard (sDepth == Recursive) >> ["-R"])
    ++ [datasetName ds]
