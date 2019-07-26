module Main where

import           Control.Monad                            ( when )
import           Data.Foldable                            ( for_ )
import           Data.ZlogShot
import           Options.Applicative

main :: IO ()
main = do
  destDataset         <- execParser opts
  snapShotExpirations <- zfsGetFromAllSnapshots destDataset BackupExpiration
  Just currentGen     <- zfsGet destDataset CurrentGeneration
  for_ snapShotExpirations $ \(snap, expirationGenM) ->
    for_ expirationGenM $ \expirationGen ->
      when (expirationGen <= currentGen) $ zfsDestroy Recursive snap
 where
  opts = info (helper <*> argParser) $ mconcat
    [fullDesc, progDesc "", header "zlogshot-prune - Prune a zlogshot backup"]
  argParser = FileSystem <$> argument str (metavar "<DETINATION DATASET>")
