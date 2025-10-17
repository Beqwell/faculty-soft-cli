module Domain.Mapping where

import Database.HDBC
import Domain.Types

rowToSoftware :: [SqlValue] -> Software
rowToSoftware [sid, ttl, ann, kid, ver, lid, stid] =
  Software (Just $ fromSql sid) (fromSql ttl) (fromSql ann)
           (fromSql kid) (fromSql ver) (fromSql lid) (fromSql stid)
rowToSoftware _ = error "bad row shape for Software"

rowToAuthor :: [SqlValue] -> Author
rowToAuthor [i,n,e] = Author (Just $ fromSql i) (fromSql n) (fromSql e)
rowToAuthor _ = error "bad row shape for Author"

rowToUser :: [SqlValue] -> UserU
rowToUser [i,un,fn,d] = UserU (Just $ fromSql i) (fromSql un) (fromSql fn) (fromSql d)
rowToUser _ = error "bad row shape for User"

rowToLicense :: [SqlValue] -> License
rowToLicense [i,n,t,u] = License (Just $ fromSql i) (fromSql n) (fromSql t) (fromSql u)
rowToLicense _ = error "bad row shape for License"

rowToKind :: [SqlValue] -> SoftwareKind
rowToKind [i,n] = SoftwareKind (Just $ fromSql i) (fromSql n)
rowToKind _ = error "bad row shape for Kind"

rowToStorage :: [SqlValue] -> StorageLocation
rowToStorage [i,loc,n] = StorageLocation (Just $ fromSql i) (fromSql loc) (fromSql n)
rowToStorage _ = error "bad row shape for Storage"
