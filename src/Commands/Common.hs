module Commands.Common
  ( readInt
  , dashToMaybe
  , maybeReadInt
  , showMid
  ) where

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

showMid :: Maybe Integer -> String
showMid = maybe "-" show
