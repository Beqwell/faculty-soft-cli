-- src/Domain.hs
-- Domain types + Typeclass for CRUD-like operations (под схему из db/schema.sql)
module Domain
  ( Software(..)
  , Author(..)
  , UserU(..)
  , License(..)
  , SoftwareKind(..)
  , StorageLocation(..)
  , UsageEvent(..), UsageKind(..)
  , Repo(..)
  , createUsage
  , rowToSoftware
  , rowToAuthor
  , rowToUser
  , rowToLicense
  , rowToKind
  , rowToStorage
  , deleteSoftware
  , deleteAuthor
  , deleteUser
  , linkAuthorSoftware
  , unlinkAuthorSoftware
  , authorsOfSoftware
  , topRuns
  , updateSoftwareMeta
  , findUserIdByUsername
  , statsByUser
  , softwareDetail
  ) where

import Database.HDBC

-- ========= Core entities =========

data Software = Software
  { swId          :: Maybe Integer
  , swTitle       :: String
  , swAnnotation  :: Maybe String
  , swKindId      :: Maybe Integer
  , swVersion     :: String
  , swLicenseId   :: Maybe Integer
  , swStorageId   :: Maybe Integer
  } deriving (Show, Eq)

data Author = Author
  { auId       :: Maybe Integer
  , auFullName :: String
  , auEmail    :: Maybe String
  } deriving (Show, Eq)

data UserU = UserU
  { usId        :: Maybe Integer
  , usUsername  :: String
  , usFullName  :: String
  , usDept      :: Maybe String
  } deriving (Show, Eq)

-- License: name, terms_text, valid_until
data License = License
  { liId         :: Maybe Integer
  , liName       :: String
  , liTermsText  :: Maybe String
  , liValidUntil :: Maybe String  -- YYYY-MM-DD 
  } deriving (Show, Eq)

-- kind_name
data SoftwareKind = SoftwareKind
  { skId       :: Maybe Integer
  , skKindName :: String
  } deriving (Show, Eq)

-- location, notes
data StorageLocation = StorageLocation
  { stId    :: Maybe Integer
  , stLoc   :: String
  , stNotes :: Maybe String
  } deriving (Show, Eq)

-- Popularity / usage stats
data UsageKind = Install | Run | Uninstall deriving (Show, Eq)

data UsageEvent = UsageEvent
  { ueId        :: Maybe Integer
  , ueSoftware  :: Integer
  , ueUser      :: Maybe Integer
  , ueKind      :: UsageKind
  } deriving (Show, Eq)

-- ========= Typeclass =========

class Repo a where
  create  :: IConnection conn => conn -> a -> IO Integer
  findAll :: IConnection conn => conn -> IO [a]
  update  :: IConnection conn => conn -> a -> IO ()

-- ========= Software =========

instance Repo Software where
  create conn s = do
    _ <- run conn
          "INSERT INTO softwares (title, annotation, kind_id, version_label, license_id, storage_id)\
          \ VALUES (?,?,?,?,?,?)"
          [ toSql (swTitle s)
          , toSql (swAnnotation s)
          , toSql (swKindId s)
          , toSql (swVersion s)
          , toSql (swLicenseId s)
          , toSql (swStorageId s)
          ]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure (fromSql uid)

  findAll conn = do
    rows <- quickQuery' conn
              "SELECT id, title, annotation, kind_id, version_label, license_id, storage_id\
              \ FROM softwares ORDER BY id DESC LIMIT 100" []
    pure (map rowToSoftware rows)

  update conn s = case swId s of
    Nothing   -> error "Software.update: missing id"
    Just sid  -> do
      _ <- run conn
            "UPDATE softwares SET title=?, annotation=?, kind_id=?, version_label=?, license_id=?, storage_id=?\
            \ WHERE id=?"
            [ toSql (swTitle s)
            , toSql (swAnnotation s)
            , toSql (swKindId s)
            , toSql (swVersion s)
            , toSql (swLicenseId s)
            , toSql (swStorageId s)
            , toSql sid
            ]
      commit conn

rowToSoftware :: [SqlValue] -> Software
rowToSoftware row = case row of
  [sid, ttl, ann, kid, ver, lid, stid] ->
    Software
      { swId        = Just (fromSql sid)
      , swTitle     = fromSql ttl
      , swAnnotation= fromSql ann
      , swKindId    = fromSql kid
      , swVersion   = fromSql ver
      , swLicenseId = fromSql lid
      , swStorageId = fromSql stid
      }
  _ -> error "bad row shape for Software"

-- ========= Author =========

instance Repo Author where
  create conn a = do
    _ <- run conn
          "INSERT INTO authors (full_name, email) VALUES (?,?)"
          [ toSql (auFullName a)
          , toSql (auEmail a)
          ]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure (fromSql uid)

  findAll conn = do
    rows <- quickQuery' conn
              "SELECT id, full_name, email FROM authors ORDER BY id DESC LIMIT 100" []
    pure (map rowToAuthor rows)

  update conn a = case auId a of
    Nothing  -> error "Author.update: missing id"
    Just aid -> do
      _ <- run conn "UPDATE authors SET full_name=?, email=? WHERE id=?"
            [ toSql (auFullName a), toSql (auEmail a), toSql aid ]
      commit conn

rowToAuthor :: [SqlValue] -> Author
rowToAuthor row = case row of
  [i,n,e] -> Author (Just (fromSql i)) (fromSql n) (fromSql e)
  _       -> error "bad row shape for Author"

-- ========= User =========

instance Repo UserU where
  create conn u = do
    _ <- run conn
          "INSERT INTO users (username, full_name, department) VALUES (?,?,?)"
          [ toSql (usUsername u), toSql (usFullName u), toSql (usDept u) ]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure (fromSql uid)

  findAll conn = do
    rows <- quickQuery' conn
              "SELECT id, username, full_name, department FROM users ORDER BY id DESC LIMIT 100" []
    pure (map rowToUser rows)

  update conn u = case usId u of
    Nothing  -> error "User.update: missing id"
    Just uid -> do
      _ <- run conn
            "UPDATE users SET username=?, full_name=?, department=? WHERE id=?"
            [ toSql (usUsername u), toSql (usFullName u), toSql (usDept u), toSql uid ]
      commit conn

rowToUser :: [SqlValue] -> UserU
rowToUser row = case row of
  [i,un,fn,d] -> UserU (Just (fromSql i)) (fromSql un) (fromSql fn) (fromSql d)
  _           -> error "bad row shape for User"

-- ========= License (name, terms_text, valid_until) =========

instance Repo License where
  create conn l = do
    _ <- run conn
          "INSERT INTO licenses (name, terms_text, valid_until) VALUES (?,?,?)"
          [ toSql (liName l), toSql (liTermsText l), toSql (liValidUntil l) ]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure (fromSql uid)

  findAll conn = do
    rows <- quickQuery' conn
              "SELECT id, name, terms_text, DATE_FORMAT(valid_until, '%Y-%m-%d') \
              \FROM licenses ORDER BY id DESC LIMIT 100" []
    pure (map rowToLicense rows)

  update conn l = case liId l of
    Nothing -> error "License.update: missing id"
    Just i  -> do
      _ <- run conn
            "UPDATE licenses SET name=?, terms_text=?, valid_until=? WHERE id=?"
            [ toSql (liName l), toSql (liTermsText l), toSql (liValidUntil l), toSql i ]
      commit conn

rowToLicense :: [SqlValue] -> License
rowToLicense row = case row of
  [i,n,t,u] -> License (Just (fromSql i)) (fromSql n) (fromSql t) (fromSql u)
  _         -> error "bad row shape for License"

-- ========= SoftwareKind (kind_name) =========

instance Repo SoftwareKind where
  create conn k = do
    _ <- run conn "INSERT IGNORE INTO software_kinds (kind_name) VALUES (?)" [toSql (skKindName k)]
    commit conn
    rows <- quickQuery' conn
              "SELECT id FROM software_kinds WHERE kind_name=? LIMIT 1"
              [toSql (skKindName k)]
    case rows of
      [[uid]] -> pure (fromSql uid)
      _       -> error "Kind.create: cannot resolve id by kind_name"
  findAll conn = do
    rows <- quickQuery' conn "SELECT id, kind_name FROM software_kinds ORDER BY id DESC LIMIT 100" []
    pure (map rowToKind rows)
  update conn k = case skId k of
    Nothing -> error "Kind.update: missing id"
    Just i  -> do
      _ <- run conn "UPDATE software_kinds SET kind_name=? WHERE id=?" [toSql (skKindName k), toSql i]
      commit conn

rowToKind :: [SqlValue] -> SoftwareKind
rowToKind row = case row of
  [i,n] -> SoftwareKind (Just (fromSql i)) (fromSql n)
  _     -> error "bad row shape for Kind"

-- ========= StorageLocation (location, notes) =========

instance Repo StorageLocation where
  create conn s = do
    _ <- run conn "INSERT INTO storage_locations (location, notes) VALUES (?,?)"
            [toSql (stLoc s), toSql (stNotes s)]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure (fromSql uid)

  findAll conn = do
    rows <- quickQuery' conn "SELECT id, location, notes FROM storage_locations ORDER BY id DESC LIMIT 100" []
    pure (map rowToStorage rows)

  update conn s = case stId s of
    Nothing -> error "Storage.update: missing id"
    Just i  -> do
      _ <- run conn "UPDATE storage_locations SET location=?, notes=? WHERE id=?"
            [toSql (stLoc s), toSql (stNotes s), toSql i]
      commit conn

rowToStorage :: [SqlValue] -> StorageLocation
rowToStorage row = case row of
  [i,loc,n] -> StorageLocation (Just (fromSql i)) (fromSql loc) (fromSql n)
  _         -> error "bad row shape for Storage"

-- ========= Deletes =========

deleteSoftware :: IConnection conn => conn -> Integer -> IO ()
deleteSoftware conn sid = do
  _ <- run conn "DELETE FROM softwares WHERE id=?" [toSql sid]
  commit conn

deleteAuthor :: IConnection conn => conn -> Integer -> IO ()
deleteAuthor conn aid = do
  _ <- run conn "DELETE FROM authors WHERE id=?" [toSql aid]
  commit conn

deleteUser :: IConnection conn => conn -> Integer -> IO ()
deleteUser conn uid = do
  _ <- run conn "DELETE FROM users WHERE id=?" [toSql uid]
  commit conn

-- ========= Usage helpers =========

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
  pure (fromSql uid)

linkAuthorSoftware :: IConnection conn => conn -> Integer -> Integer -> IO ()
linkAuthorSoftware conn sid aid = do
  _ <- run conn
        "INSERT IGNORE INTO software_authors (software_id, author_id) VALUES (?,?)"
        [toSql sid, toSql aid]
  commit conn

unlinkAuthorSoftware :: IConnection conn => conn -> Integer -> Integer -> IO ()
unlinkAuthorSoftware conn sid aid = do
  _ <- run conn
        "DELETE FROM software_authors WHERE software_id=? AND author_id=?"
        [toSql sid, toSql aid]
  commit conn

authorsOfSoftware :: IConnection conn => conn -> Integer -> IO [Author]
authorsOfSoftware conn sid = do
  rows <- quickQuery' conn
            "SELECT a.id, a.full_name, a.email \
            \FROM software_authors sa \
            \JOIN authors a ON a.id = sa.author_id \
            \WHERE sa.software_id = ? \
            \ORDER BY a.id DESC"
            [toSql sid]
  pure (map rowToAuthor rows)

topRuns :: IConnection conn => conn -> IO [(Integer, String, Integer)]
topRuns conn = do
  rows <- quickQuery' conn
            "SELECT s.id, s.title, COUNT(*) AS runs \
            \FROM usage_events ue \
            \JOIN softwares s ON s.id = ue.software_id \
            \WHERE ue.event_type='run' \
            \GROUP BY s.id, s.title \
            \ORDER BY runs DESC, s.id DESC \
            \LIMIT 10" []
  pure $ map (\[sid, ttl, cnt] ->
                ( fromSql sid :: Integer
                , fromSql ttl :: String
                , fromSql cnt :: Integer)) rows

-- ========= Meta update for Software =========
updateSoftwareMeta
  :: IConnection conn
  => conn
  -> Integer               -- software id
  -> Maybe String          -- annotation
  -> Maybe Integer         -- kind_id
  -> Maybe Integer         -- license_id
  -> Maybe Integer         -- storage_id
  -> IO ()
updateSoftwareMeta conn sid ann kid lid stid = do
  _ <- run conn
        "UPDATE softwares SET \
        \ annotation = COALESCE(?, annotation), \
        \ kind_id    = COALESCE(?, kind_id), \
        \ license_id = COALESCE(?, license_id), \
        \ storage_id = COALESCE(?, storage_id) \
        \ WHERE id=?"
        [ toSql ann, toSql kid, toSql lid, toSql stid, toSql sid ]
  commit conn

-- ========= Lookups / Stats =========

findUserIdByUsername :: IConnection conn => conn -> String -> IO (Maybe Integer)
findUserIdByUsername conn uname = do
  rows <- quickQuery' conn "SELECT id FROM users WHERE username=? LIMIT 1" [toSql uname]
  case rows of
    [[i]] -> pure (Just (fromSql i))
    _     -> pure Nothing

-- Per-user stats: (software_id, title, runs, installs, uninstalls)
statsByUser :: IConnection conn => conn -> Integer -> IO [(Integer, String, Integer, Integer, Integer)]
statsByUser conn uid = do
  rows <- quickQuery' conn
            "SELECT s.id, s.title, \
            \ SUM(ue.event_type='run')        AS runs, \
            \ SUM(ue.event_type='install')    AS installs, \
            \ SUM(ue.event_type='uninstall')  AS uninstalls \
            \FROM usage_events ue \
            \JOIN softwares s ON s.id = ue.software_id \
            \WHERE ue.user_id = ? \
            \GROUP BY s.id, s.title \
            \ORDER BY runs DESC, s.id DESC"
            [toSql uid]
  pure $ map (\[sid, ttl, r, i, u] ->
              ( fromSql sid :: Integer
              , fromSql ttl :: String
              , fromSql r   :: Integer
              , fromSql i   :: Integer
              , fromSql u   :: Integer)) rows

-- Software detail: counts + last usage time (as String)
softwareDetail :: IConnection conn => conn -> Integer -> IO (Integer, Integer, Integer, Maybe String)
softwareDetail conn sid = do
  rows <- quickQuery' conn
            "SELECT \
            \ SUM(event_type='run'), \
            \ SUM(event_type='install'), \
            \ SUM(event_type='uninstall'), \
            \ MAX(event_time) \
            \FROM usage_events WHERE software_id=?"
            [toSql sid]
  case rows of
    [[r,i,u,lt]] ->
      pure ( fromSql r :: Integer
           , fromSql i :: Integer
           , fromSql u :: Integer
           , let s :: Maybe String; s = fromSql lt in s)
    _ -> pure (0,0,0,Nothing)
