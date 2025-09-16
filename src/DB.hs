-- src/DB.hs
-- Simple DB helpers (ODBC connection)
module DB (withConn) where

import Control.Exception      (bracket)
import Database.HDBC          (disconnect)
import Database.HDBC.ODBC     (Connection, connectODBC)

connStr :: String
connStr =
  "Driver={MySQL ODBC 9.4 Unicode Driver};" ++
  "Server=127.0.0.1;Port=3307;" ++
  "Database=testdb;User=root;Password=pass123;OPTION=3;"


withConn :: (Connection -> IO a) -> IO a
withConn = bracket (connectODBC connStr) disconnect
