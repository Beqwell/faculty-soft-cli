module Commands.User
  ( cmdUserList
  , cmdUserAdd
  , cmdUserDelete
  ) where

import DB (withConn)
import Domain
import Commands.Common

cmdUserList :: IO ()
cmdUserList = withConn $ \conn -> do
  xs <- findAll conn :: IO [UserU]
  mapM_ (\u -> putStrLn (showMid (usId u) ++ " | " ++ usUsername u ++ " | " ++ usFullName u)) xs

cmdUserAdd :: String -> String -> Maybe String -> IO ()
cmdUserAdd username fullname mDept = withConn $ \conn -> do
  let u = UserU { usId=Nothing, usUsername=username, usFullName=fullname, usDept=mDept }
  newId <- create conn u
  putStrLn ("Created user id=" ++ show newId)

cmdUserDelete :: String -> IO ()
cmdUserDelete uidS = withConn $ \conn -> do
  deleteUser conn (readInt uidS)
  putStrLn "User deleted."
