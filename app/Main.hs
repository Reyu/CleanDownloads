{-# LANGUAGE MultiWayIf #-}

module Main
  (main)
  where

import Control.Monad    (when, unless)
import Data.Time.Clock  (getCurrentTime, diffUTCTime)
import System.Directory
import System.FilePath  ((</>))

main :: IO ()
main = do
  getHomeDirectory >>= setCurrentDirectory . flip (</>) "Downloads"
  createDirectoryIfMissing True "Old"
  getDirectoryContents "." >>= clean' False ["Old", "Keep"]
  setCurrentDirectory "Old"
  getDirectoryContents "." >>= clean' True []
  where
    clean' remove extra = mapM_ (clean remove) . filter (noDotsPlus extra)
    noDotsPlus extra    = not . flip elem ([".", ".."] ++ extra)

clean :: Bool -> FilePath -> IO ()
clean remove path = do
  cTime  <- getCurrentTime
  aTime  <- getAccessTime path
  isFile <- doesFileExist path -- True: File | False: Directory
  when (diffUTCTime cTime aTime > oneWeek) $
    do if |  isFile && remove         -> removeFile path
          |  isFile && not remove     -> renameFile path newPath
          |  not isFile && remove     -> removeDirectoryRecursive path
          |  not isFile && not remove -> renameDirectory path newPath
       unless remove $ setAccessTime newPath cTime
  where
      newPath = "Old" </> path
      oneWeek = 60 * 60 * 24 * 7
