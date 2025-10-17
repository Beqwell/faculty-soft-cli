module App (runApp) where

import Commands.Common as C
import qualified Commands.Software as Software
import qualified Commands.Author   as Author
import qualified Commands.User     as User
import qualified Commands.License  as License
import qualified Commands.Kind     as Kind
import qualified Commands.Storage  as Storage
import qualified Commands.Usage    as Usage
import qualified Commands.Stats    as Stats

runApp :: [String] -> IO ()
runApp args = case args of
  -- software
  ("soft:list":_) -> Software.cmdSoftList
  ("soft:add":title:version:_) -> Software.cmdSoftAdd title version
  ("soft:update":sid:title:version:_) -> Software.cmdSoftUpdate sid title version
  ("soft:update-meta":sid:ann:kid:lid:stid:_) -> Software.cmdSoftUpdateMeta sid ann kid lid stid
  ("soft:delete":sid:_) -> Software.cmdSoftDelete sid
  ("soft:link-author":sid:aid:_)   -> Software.cmdSoftLinkAuthor sid aid
  ("soft:unlink-author":sid:aid:_) -> Software.cmdSoftUnlinkAuthor sid aid
  ("soft:authors":sid:_) -> Software.cmdSoftAuthors sid

  -- authors
  ("author:list":_) -> Author.cmdAuthorList
  ("author:add":fullname:emailOrDash:_) -> Author.cmdAuthorAdd fullname (C.dashToMaybe emailOrDash)
  ("author:delete":aid:_) -> Author.cmdAuthorDelete aid

  -- users
  ("user:list":_) -> User.cmdUserList
  ("user:add":username:fullname:deptOrDash:_) -> User.cmdUserAdd username fullname (C.dashToMaybe deptOrDash)
  ("user:delete":uid:_) -> User.cmdUserDelete uid

  -- licenses
  ("license:list":_) -> License.cmdLicenseList
  ("license:add":name:termsOrDash:validOrDash:_) -> License.cmdLicenseAdd name (C.dashToMaybe termsOrDash) (C.dashToMaybe validOrDash)

  -- kinds
  ("kind:list":_) -> Kind.cmdKindList
  ("kind:add":kindName:_) -> Kind.cmdKindAdd kindName

  -- storage
  ("storage:list":_) -> Storage.cmdStorageList
  ("storage:add":location:notesOrDash:_) -> Storage.cmdStorageAdd location (C.dashToMaybe notesOrDash)

  -- usage
  ("use:run":sid:uidOrDash:_)        -> Usage.cmdUse "run" sid (C.dashToMaybe uidOrDash)
  ("use:install":sid:uidOrDash:_)    -> Usage.cmdUse "install" sid (C.dashToMaybe uidOrDash)
  ("use:uninstall":sid:uidOrDash:_)  -> Usage.cmdUse "uninstall" sid (C.dashToMaybe uidOrDash)

  -- stats
  ("stats:top-runs":_) -> Stats.cmdTopRuns
  ("stats:by-user":uid:_) -> Stats.cmdStatsByUser uid
  ("stats:software-detail":sid:_) -> Stats.cmdSoftwareDetail sid

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
