module Commands.License
  ( cmdLicenseList
  , cmdLicenseAdd
  ) where

import DB (withConn)
import Domain
import Commands.Common

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
