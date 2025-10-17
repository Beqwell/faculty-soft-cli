module Domain.Types where

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

data License = License
  { liId         :: Maybe Integer
  , liName       :: String
  , liTermsText  :: Maybe String
  , liValidUntil :: Maybe String
  } deriving (Show, Eq)

data SoftwareKind = SoftwareKind
  { skId       :: Maybe Integer
  , skKindName :: String
  } deriving (Show, Eq)

data StorageLocation = StorageLocation
  { stId    :: Maybe Integer
  , stLoc   :: String
  , stNotes :: Maybe String
  } deriving (Show, Eq)

data UsageKind = Install | Run | Uninstall deriving (Show, Eq)

data UsageEvent = UsageEvent
  { ueId        :: Maybe Integer
  , ueSoftware  :: Integer
  , ueUser      :: Maybe Integer
  , ueKind      :: UsageKind
  } deriving (Show, Eq)
