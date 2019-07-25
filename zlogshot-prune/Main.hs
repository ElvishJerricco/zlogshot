module Main where

import           Data.ZlogShot
import           Options.Applicative

main :: IO ()
main = do
  destDataset <- execParser opts
  return ()
 where
  opts = info (helper <*> argParser) $ mconcat
    [fullDesc, progDesc "", header "zlogshot-prune - Prune a zlogshot backup"]
  argParser = Dataset <$> argument str (metavar "<DETINATION DATASET>")
