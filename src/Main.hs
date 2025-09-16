-- src/Main.hs
-- CLI: software, authors, users, licenses, kinds, storage, usage, links, stats
module Main (main) where

import System.Environment (getArgs)
import DB (withConn)
import Domain

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- software
    ("soft:list":_) -> cmdSoftList
    ("soft:add":title:version:_) -> cmdSoftAdd title version
    ("soft:update":sid:title:version:_) -> cmdSoftUpdate sid title version
    ("soft:update-meta":sid:ann:kid:lid:stid:_) -> cmdSoftUpdateMeta sid ann kid lid stid
    ("soft:delete":sid:_) -> cmdSoftDelete sid
    ("soft:link-author":sid:aid:_)   -> cmdSoftLinkAuthor sid aid
    ("soft:unlink-author":sid:aid:_) -> cmdSoftUnlinkAuthor sid aid
    ("soft:authors":sid:_) -> cmdSoftAuthors sid

    -- authors
    ("author:list":_) -> cmdAuthorList
    ("author:add":fullname:emailOrDash:_) -> cmdAuthorAdd fullname (dashToMaybe emailOrDash)
    ("author:delete":aid:_) -> cmdAuthorDelete aid

    -- users
    ("user:list":_) -> cmdUserList
    ("user:add":username:fullname:deptOrDash:_) -> cmdUserAdd username fullname (dashToMaybe deptOrDash)
    ("user:delete":uid:_) -> cmdUserDelete uid

    -- licenses  (name, terms_text|-, valid_until|-)  YYYY-MM-DD для даты
    ("license:list":_) -> cmdLicenseList
    ("license:add":name:termsOrDash:validOrDash:_) -> cmdLicenseAdd name (dashToMaybe termsOrDash) (dashToMaybe validOrDash)

    -- kinds   (kind_name)
    ("kind:list":_) -> cmdKindList
    ("kind:add":kindName:_) -> cmdKindAdd kindName

    -- storage  (location, notes)
    ("storage:list":_) -> cmdStorageList
    ("storage:add":location:notesOrDash:_) -> cmdStorageAdd location (dashToMaybe notesOrDash)

    -- usage events
    ("use:run":sid:uidOrDash:_)        -> cmdUse "run" sid (dashToMaybe uidOrDash)
    ("use:install":sid:uidOrDash:_)    -> cmdUse "install" sid (dashToMaybe uidOrDash)
    ("use:uninstall":sid:uidOrDash:_)  -> cmdUse "uninstall" sid (dashToMaybe uidOrDash)

    -- stats
    ("stats:top-runs":_) -> cmdTopRuns
    ("stats:by-user":uid:_) -> cmdStatsByUser uid
    ("stats:software-detail":sid:_) -> cmdSoftwareDetail sid

    _ -> putStrLn $ unlines
          [ "Usage:"
          , "  stack run -- soft:list"
          , "  stack run -- soft:add <title> <version>"
          , "  stack run -- soft:update <id> <title> <version>"
          , "  stack run -- soft:update-meta <id> <annotation|-> <kind_id|-> <license_id|-> <storage_id|->"
          , "  stack run -- soft:delete <id>"
          , "  stack run -- soft:link-author <software_id> <author_id>"
          , "  stack run -- soft:unlink-author <software_id> <author_id>"
          , "  stack run -- soft:authors <software_id>"
          , "  stack run -- author:list"
          , "  stack run -- author:add <full_name> <email|->"
          , "  stack run -- author:delete <id>"
          , "  stack run -- user:list"
          , "  stack run -- user:add <username> <full_name> <department|->"
          , "  stack run -- user:delete <id>"
          , "  stack run -- license:list"
          , "  stack run -- license:add <name> <terms_text|-> <valid_until(YYYY-MM-DD)|->"
          , "  stack run -- kind:list"
          , "  stack run -- kind:add <kind_name>"
          , "  stack run -- storage:list"
          , "  stack run -- storage:add <location> <notes|->"
          , "  stack run -- use:run <software_id> <user_id|->"
          , "  stack run -- use:install <software_id> <user_id|->"
          , "  stack run -- use:uninstall <software_id> <user_id|->"
          , "  stack run -- stats:top-runs"
          , "  stack run -- stats:by-user <user_id>"
          , "  stack run -- stats:software-detail <software_id>"
          ]

-- ===== helpers =====

readInt :: String -> Integer
readInt s = case reads s of
  [(n,"")] -> n
  _        -> error "bad integer"

dashToMaybe :: String -> Maybe String
dashToMaybe s
  | s == "-"    = Nothing
  | s == "NULL" = Nothing
  | s == "null" = Nothing
  | otherwise   = Just s


maybeReadInt :: String -> Maybe Integer
maybeReadInt s
  | s == "-"    = Nothing
  | s == "NULL" = Nothing
  | s == "null" = Nothing
  | otherwise   = case reads s of
      [(n,"")] -> Just n
      _        -> error "bad integer in optional field"


-- pretty print Maybe Integer (id)
showMid :: Maybe Integer -> String
showMid = maybe "-" show

-- ===== software =====

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

-- ===== authors =====

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

-- ===== users =====

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

-- ===== licenses (name, terms_text, valid_until) =====

cmdLicenseList :: IO ()
cmdLicenseList = withConn $ \conn -> do
  xs <- findAll conn :: IO [License]
  mapM_ (\l -> putStrLn (showMid (liId l) ++ " | " ++ liName l
                         ++ maybe "" (\t -> " | terms=" ++ t) (liTermsText l)
                         ++ maybe "" (\d -> " | valid_until=" ++ d) (liValidUntil l)
                         )) xs

cmdLicenseAdd :: String -> Maybe String -> Maybe String -> IO ()
cmdLicenseAdd name mTerms mValid = withConn $ \conn -> do
  let l = License { liId=Nothing, liName=name, liTermsText=mTerms, liValidUntil=mValid }
  newId <- create conn l
  putStrLn ("Created license id=" ++ show newId)

-- ===== kinds (kind_name) =====

cmdKindList :: IO ()
cmdKindList = withConn $ \conn -> do
  xs <- findAll conn :: IO [SoftwareKind]
  mapM_ (\k -> putStrLn (showMid (skId k) ++ " | " ++ skKindName k)) xs

cmdKindAdd :: String -> IO ()
cmdKindAdd kindName = withConn $ \conn -> do
  let k = SoftwareKind { skId=Nothing, skKindName=kindName }
  newId <- create conn k
  putStrLn ("Created kind id=" ++ show newId)

-- ===== storage (location, notes) =====

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

-- ===== usage =====

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

-- ===== stats =====

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
