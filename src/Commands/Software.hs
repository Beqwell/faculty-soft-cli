module Commands.Software
  ( cmdSoftList
  , cmdSoftAdd
  , cmdSoftUpdate
  , cmdSoftUpdateMeta
  , cmdSoftDelete
  , cmdSoftLinkAuthor
  , cmdSoftUnlinkAuthor
  , cmdSoftAuthors
  ) where

import DB (withConn)
import Domain
import Commands.Common

cmdSoftList :: IO ()
cmdSoftList = withConn $ \conn -> do
  xs <- findAll conn :: IO [Software]
  mapM_ (\s -> putStrLn (showMid (swId s) ++ " | " ++ swTitle s ++ " | v" ++ swVersion s)) xs

cmdSoftAdd :: String -> String -> IO ()
cmdSoftAdd title version = withConn $ \conn -> do
  let s = Software { swId=Nothing, swTitle=title, swAnnotation=Nothing
                   , swKindId=Nothing, swVersion=version
                   , swLicenseId=Nothing, swStorageId=Nothing }
  newId <- create conn s
  putStrLn ("Created software id=" ++ show newId)

cmdSoftUpdate :: String -> String -> String -> IO ()
cmdSoftUpdate sidS title version = withConn $ \conn -> do
  let sid = readInt sidS
  let s = Software { swId=Just sid, swTitle=title, swAnnotation=Nothing
                   , swKindId=Nothing, swVersion=version
                   , swLicenseId=Nothing, swStorageId=Nothing }
  update conn s
  putStrLn "Updated."

cmdSoftUpdateMeta :: String -> String -> String -> String -> String -> IO ()
cmdSoftUpdateMeta sidS annS kidS lidS stidS = withConn $ \conn -> do
  let sid  = readInt sidS
      mAnn  = dashToMaybe annS
      mKid  = maybeReadInt kidS
      mLid  = maybeReadInt lidS
      mStid = maybeReadInt stidS
  updateSoftwareMeta conn sid mAnn mKid mLid mStid
  putStrLn "Meta updated."

cmdSoftDelete :: String -> IO ()
cmdSoftDelete sidS = withConn $ \conn -> do
  deleteSoftware conn (readInt sidS)
  putStrLn "Deleted."

cmdSoftLinkAuthor :: String -> String -> IO ()
cmdSoftLinkAuthor sidS aidS = withConn $ \conn -> do
  linkAuthorSoftware conn (readInt sidS) (readInt aidS)
  putStrLn "Linked author to software."

cmdSoftUnlinkAuthor :: String -> String -> IO ()
cmdSoftUnlinkAuthor sidS aidS = withConn $ \conn -> do
  unlinkAuthorSoftware conn (readInt sidS) (readInt aidS)
  putStrLn "Unlinked author from software."

cmdSoftAuthors :: String -> IO ()
cmdSoftAuthors sidS = withConn $ \conn -> do
  let sid = readInt sidS
  as <- authorsOfSoftware conn sid
  if null as
     then putStrLn "No authors linked."
     else mapM_ (\a -> putStrLn (showMid (auId a) ++ " | " ++ auFullName a)) as
