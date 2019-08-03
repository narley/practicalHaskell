import Data.ByteString.Char8 (pack)
import Database (migrateDB, localConnString)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  databaseString <- lookupEnv "DATABASE_URL"
  let conn = case databaseString of
        Nothing -> localConnString
        Just c -> pack c
  migrateDB conn
