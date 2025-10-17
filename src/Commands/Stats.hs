module Commands.Stats
  ( cmdTopRuns
  , cmdStatsByUser
  , cmdSoftwareDetail
  ) where

import DB (withConn)
import Domain
import Commands.Common

cmdTopRuns :: IO ()
cmdTopRuns = withConn $ \conn -> do
  xs <- topRuns conn
  mapM_ (\(sid, ttl, cnt) -> putStrLn (show sid ++ " | " ++ ttl ++ " | runs=" ++ show cnt)) xs

cmdStatsByUser :: String -> IO ()
cmdStatsByUser uidS = withConn $ \conn -> do
  let uid = readInt uidS
  xs <- statsByUser conn uid
  if null xs then putStrLn "No data."
  else mapM_ (\(sid, ttl, runs, ins, uns) ->
                putStrLn (show sid ++ " | " ++ ttl ++
                          " | runs=" ++ show runs ++
                          " | installs=" ++ show ins ++
                          " | uninstalls=" ++ show uns)) xs

cmdSoftwareDetail :: String -> IO ()
cmdSoftwareDetail sidS = withConn $ \conn -> do
  let sid = readInt sidS
  (runs, ins, uns, mLast) <- softwareDetail conn sid
  putStrLn ("runs=" ++ show runs ++ ", installs=" ++ show ins ++
            ", uninstalls=" ++ show uns ++
            maybe "" (\t -> ", last=" ++ t) mLast)
