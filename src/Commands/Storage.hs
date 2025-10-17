module Commands.Storage
  ( cmdStorageList
  , cmdStorageAdd
  ) where

import DB (withConn)
import Domain
import Commands.Common

cmdStorageList :: IO ()
cmdStorageList = withConn $ \conn -> do
  xs <- findAll conn :: IO [StorageLocation]
  mapM_ (\s -> putStrLn (showMid (stId s) ++ " | " ++ stLoc s
                         ++ maybe "" (\n -> " | notes=" ++ n) (stNotes s))) xs

cmdStorageAdd :: String -> Maybe String -> IO ()
cmdStorageAdd location mNotes = withConn $ \conn -> do
  let s = StorageLocation { stId=Nothing, stLoc=location, stNotes=mNotes }
  newId <- create conn s
  putStrLn ("Created storage id=" ++ show newId)
