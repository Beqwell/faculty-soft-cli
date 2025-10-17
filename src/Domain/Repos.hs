module Domain.Repos where

import Database.HDBC
import Domain.Types
import Domain.Mapping

class Repo a where
  create  :: IConnection conn => conn -> a -> IO Integer
  findAll :: IConnection conn => conn -> IO [a]
  update  :: IConnection conn => conn -> a -> IO ()

-- ========= Software =========
instance Repo Software where
  create conn s = do
    _ <- run conn
          "INSERT INTO softwares (title, annotation, kind_id, version_label, license_id, storage_id) VALUES (?,?,?,?,?,?)"
          [ toSql (swTitle s), toSql (swAnnotation s), toSql (swKindId s)
          , toSql (swVersion s), toSql (swLicenseId s), toSql (swStorageId s) ]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure $ fromSql uid

  findAll conn = do
    rows <- quickQuery' conn
      "SELECT id, title, annotation, kind_id, version_label, license_id, storage_id FROM softwares ORDER BY id DESC LIMIT 100" []
    pure $ map rowToSoftware rows

  update conn s = case swId s of
    Nothing -> error "Software.update: missing id"
    Just sid -> do
      _ <- run conn
        "UPDATE softwares SET title=?, annotation=?, kind_id=?, version_label=?, license_id=?, storage_id=? WHERE id=?"
        [ toSql (swTitle s), toSql (swAnnotation s), toSql (swKindId s)
        , toSql (swVersion s), toSql (swLicenseId s), toSql (swStorageId s)
        , toSql sid ]
      commit conn

-- ========= Author =========
instance Repo Author where
  create conn a = do
    _ <- run conn "INSERT INTO authors (full_name, email) VALUES (?,?)"
         [toSql (auFullName a), toSql (auEmail a)]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure $ fromSql uid

  findAll conn = do
    rows <- quickQuery' conn "SELECT id, full_name, email FROM authors ORDER BY id DESC LIMIT 100" []
    pure $ map rowToAuthor rows

  update conn a = case auId a of
    Nothing -> error "Author.update: missing id"
    Just aid -> do
      _ <- run conn "UPDATE authors SET full_name=?, email=? WHERE id=?"
        [ toSql (auFullName a), toSql (auEmail a), toSql aid ]
      commit conn

-- ========= User =========
instance Repo UserU where
  create conn u = do
    _ <- run conn "INSERT INTO users (username, full_name, department) VALUES (?,?,?)"
         [toSql (usUsername u), toSql (usFullName u), toSql (usDept u)]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure $ fromSql uid

  findAll conn = do
    rows <- quickQuery' conn
      "SELECT id, username, full_name, department FROM users ORDER BY id DESC LIMIT 100" []
    pure $ map rowToUser rows

  update conn u = case usId u of
    Nothing -> error "User.update: missing id"
    Just uid -> do
      _ <- run conn
        "UPDATE users SET username=?, full_name=?, department=? WHERE id=?"
        [ toSql (usUsername u), toSql (usFullName u), toSql (usDept u), toSql uid ]
      commit conn

-- ========= License =========
instance Repo License where
  create conn l = do
    _ <- run conn "INSERT INTO licenses (name, terms_text, valid_until) VALUES (?,?,?)"
         [toSql (liName l), toSql (liTermsText l), toSql (liValidUntil l)]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure $ fromSql uid

  findAll conn = do
    rows <- quickQuery' conn
      "SELECT id, name, terms_text, DATE_FORMAT(valid_until, '%Y-%m-%d') FROM licenses ORDER BY id DESC LIMIT 100" []
    pure $ map rowToLicense rows

  update conn l = case liId l of
    Nothing -> error "License.update: missing id"
    Just i -> do
      _ <- run conn "UPDATE licenses SET name=?, terms_text=?, valid_until=? WHERE id=?"
        [ toSql (liName l), toSql (liTermsText l), toSql (liValidUntil l), toSql i ]
      commit conn

-- ========= Kind =========
instance Repo SoftwareKind where
  create conn k = do
    _ <- run conn "INSERT IGNORE INTO software_kinds (kind_name) VALUES (?)" [toSql (skKindName k)]
    commit conn
    rows <- quickQuery' conn "SELECT id FROM software_kinds WHERE kind_name=? LIMIT 1" [toSql (skKindName k)]
    case rows of
      [[uid]] -> pure $ fromSql uid
      _ -> error "Kind.create: cannot resolve id by kind_name"

  findAll conn = do
    rows <- quickQuery' conn "SELECT id, kind_name FROM software_kinds ORDER BY id DESC LIMIT 100" []
    pure $ map rowToKind rows

  update conn k = case skId k of
    Nothing -> error "Kind.update: missing id"
    Just i -> do
      _ <- run conn "UPDATE software_kinds SET kind_name=? WHERE id=?" [toSql (skKindName k), toSql i]
      commit conn

-- ========= Storage =========
instance Repo StorageLocation where
  create conn s = do
    _ <- run conn "INSERT INTO storage_locations (location, notes) VALUES (?,?)"
         [toSql (stLoc s), toSql (stNotes s)]
    commit conn
    [[uid]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    pure $ fromSql uid

  findAll conn = do
    rows <- quickQuery' conn "SELECT id, location, notes FROM storage_locations ORDER BY id DESC LIMIT 100" []
    pure $ map rowToStorage rows

  update conn s = case stId s of
    Nothing -> error "Storage.update: missing id"
    Just i -> do
      _ <- run conn "UPDATE storage_locations SET location=?, notes=? WHERE id=?"
        [toSql (stLoc s), toSql (stNotes s), toSql i]
      commit conn

-- ========= Deletes =========
deleteSoftware, deleteAuthor, deleteUser :: IConnection conn => conn -> Integer -> IO ()
deleteSoftware conn sid = run conn "DELETE FROM softwares WHERE id=?" [toSql sid] >> commit conn
deleteAuthor conn aid = run conn "DELETE FROM authors WHERE id=?" [toSql aid] >> commit conn
deleteUser conn uid = run conn "DELETE FROM users WHERE id=?" [toSql uid] >> commit conn
