module Commands.Author
  ( cmdAuthorList
  , cmdAuthorAdd
  , cmdAuthorDelete
  ) where

import DB (withConn)
import Domain
import Commands.Common

cmdAuthorList :: IO ()
cmdAuthorList = withConn $ \conn -> do
  xs <- findAll conn :: IO [Author]
  mapM_ (\a -> putStrLn (showMid (auId a) ++ " | " ++ auFullName a)) xs

cmdAuthorAdd :: String -> Maybe String -> IO ()
cmdAuthorAdd fullname mEmail = withConn $ \conn -> do
  let a = Author { auId=Nothing, auFullName=fullname, auEmail=mEmail }
  newId <- create conn a
  putStrLn ("Created author id=" ++ show newId)

cmdAuthorDelete :: String -> IO ()
cmdAuthorDelete aidS = withConn $ \conn -> do
  deleteAuthor conn (readInt aidS)
  putStrLn "Author deleted."
