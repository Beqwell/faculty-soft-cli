module Commands.Usage
  ( cmdUse
  ) where

import DB (withConn)
import Domain
import Commands.Common

cmdUse :: String -> String -> Maybe String -> IO ()
cmdUse kind sidS mUidS = withConn $ \conn -> do
  let sid = readInt sidS
      mUid :: Maybe Integer
      mUid = case mUidS of
               Nothing -> Nothing
               Just t  -> Just (readInt t)
      kindC = case kind of
                "install"   -> Install
                "run"       -> Run
                "uninstall" -> Uninstall
                _           -> error "bad usage kind"
  _ <- createUsage conn UsageEvent { ueId=Nothing, ueSoftware=sid, ueUser=mUid, ueKind=kindC }
  putStrLn "Usage recorded."
