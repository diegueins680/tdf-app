module Main where
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hFlush)
import Test.Hspec (hspec)
import qualified TDF.AtomicPublicationSpec as AtomicPublication
import TDF.Storage.AtomicPublication (publishCompleteFile)
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--crash-writer", directory] -> void $ publishCompleteFile (directory </> "rider") $ \handle -> do
      BS.hPut handle (BS.replicate 4096 65)
      hFlush handle
      writeFile (directory </> "ready") "partial staging written"
      forever (threadDelay 1000000)
    _ -> hspec AtomicPublication.spec
