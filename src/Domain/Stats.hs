module Domain.Stats where

import Database.HDBC
import Domain.Types
import Domain.Mapping

linkAuthorSoftware, unlinkAuthorSoftware :: IConnection conn => conn -> Integer -> Integer -> IO ()
linkAuthorSoftware conn sid aid = do
  _ <- run conn "INSERT IGNORE INTO software_authors (software_id, author_id) VALUES (?,?)"
       [toSql sid, toSql aid]
  commit conn

unlinkAuthorSoftware conn sid aid = do
  _ <- run conn "DELETE FROM software_authors WHERE software_id=? AND author_id=?"
       [toSql sid, toSql aid]
  commit conn

authorsOfSoftware :: IConnection conn => conn -> Integer -> IO [Author]
authorsOfSoftware conn sid = do
  rows <- quickQuery' conn
    "SELECT a.id, a.full_name, a.email FROM software_authors sa JOIN authors a ON a.id = sa.author_id WHERE sa.software_id = ? ORDER BY a.id DESC"
    [toSql sid]
  pure $ map rowToAuthor rows

topRuns :: IConnection conn => conn -> IO [(Integer, String, Integer)]
topRuns conn = do
  rows <- quickQuery' conn
    "SELECT s.id, s.title, COUNT(*) AS runs FROM usage_events ue JOIN softwares s ON s.id = ue.software_id WHERE ue.event_type='run' GROUP BY s.id, s.title ORDER BY runs DESC, s.id DESC LIMIT 10"
    []
  pure $ map (\[sid, ttl, cnt] -> (fromSql sid, fromSql ttl, fromSql cnt)) rows

updateSoftwareMeta :: IConnection conn => conn -> Integer -> Maybe String -> Maybe Integer -> Maybe Integer -> Maybe Integer -> IO ()
updateSoftwareMeta conn sid ann kid lid stid = do
  _ <- run conn
    "UPDATE softwares SET annotation = COALESCE(?, annotation), kind_id = COALESCE(?, kind_id), license_id = COALESCE(?, license_id), storage_id = COALESCE(?, storage_id) WHERE id=?"
    [toSql ann, toSql kid, toSql lid, toSql stid, toSql sid]
  commit conn

createUsage :: IConnection conn => conn -> UsageEvent -> IO Integer
createUsage conn ev = do
  let kindTxt = case ueKind ev of
        Install   -> "install"
        Run       -> "run"
        Uninstall -> "uninstall"
  _ <- run conn
        "INSERT INTO usage_events (software_id, user_id, event_type) VALUES (?,?,?)"
        [ toSql (ueSoftware ev), toSql (ueUser ev), toSql (kindTxt :: String) ]
  commit conn
  [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
  pure $ fromSql uid

findUserIdByUsername :: IConnection conn => conn -> String -> IO (Maybe Integer)
findUserIdByUsername conn uname = do
  rows <- quickQuery' conn "SELECT id FROM users WHERE username=? LIMIT 1" [toSql uname]
  case rows of
    [[i]] -> pure $ Just $ fromSql i
    _     -> pure Nothing

statsByUser :: IConnection conn => conn -> Integer -> IO [(Integer, String, Integer, Integer, Integer)]
statsByUser conn uid = do
  rows <- quickQuery' conn
    "SELECT s.id, s.title, SUM(ue.event_type='run'), SUM(ue.event_type='install'), SUM(ue.event_type='uninstall') FROM usage_events ue JOIN softwares s ON s.id = ue.software_id WHERE ue.user_id = ? GROUP BY s.id, s.title ORDER BY runs DESC, s.id DESC"
    [toSql uid]
  pure $ map (\[sid, ttl, r, i, u] -> (fromSql sid, fromSql ttl, fromSql r, fromSql i, fromSql u)) rows

softwareDetail :: IConnection conn => conn -> Integer -> IO (Integer, Integer, Integer, Maybe String)
softwareDetail conn sid = do
  rows <- quickQuery' conn
    "SELECT SUM(event_type='run'), SUM(event_type='install'), SUM(event_type='uninstall'), MAX(event_time) FROM usage_events WHERE software_id=?"
    [toSql sid]
  case rows of
    [[r,i,u,lt]] -> pure (fromSql r, fromSql i, fromSql u, fromSql lt)
    _ -> pure (0,0,0,Nothing)
