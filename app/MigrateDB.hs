import Database (migrateDB, localConnString)

main :: IO ()
main = migrateDB localConnString
