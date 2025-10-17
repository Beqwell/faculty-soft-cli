module Commands.Kind
  ( cmdKindList
  , cmdKindAdd
  ) where

import DB (withConn)
import Domain
import Commands.Common

cmdKindList :: IO ()
cmdKindList = withConn $ \conn -> do
  xs <- findAll conn :: IO [SoftwareKind]
  mapM_ (\k -> putStrLn (showMid (skId k) ++ " | " ++ skKindName k)) xs

cmdKindAdd :: String -> IO ()
cmdKindAdd kindName = withConn $ \conn -> do
  let k = SoftwareKind { skId=Nothing, skKindName=kindName }
  newId <- create conn k
  putStrLn ("Created kind id=" ++ show newId)
