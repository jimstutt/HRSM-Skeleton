module DB
  ( withConnPool
  , getAllUsers
  ) where

import Common.Types (User(..))
import Database.MySQL.Simple
import Data.Pool (Pool, createPool, withResource)
import Control.Exception (bracket)

-- | Database connection settings
data DBSettings = DBSettings
  { dbHost     :: Host
  , dbPort     :: Port
  , dbUser     :: User
  , dbPassword :: Password
  , dbName     :: Query
  }

-- | Placeholder settings for a local MariaDB instance
defaultSettings :: DBSettings
defaultSettings = DBSettings "127.0.0.1" 3306 "root" "" "ngologistics_db"

-- | Initializes a MariaDB connection and creates a connection pool.
createMariaDBPool :: DBSettings -> IO (Pool Connection)
createMariaDBPool s = createPool
  (connect defaultConnectInfo { connectHost = dbHost s, connectPort = dbPort s, connectUser = dbUser s, connectPassword = dbPassword s, connectDatabase = dbName s })
  close
  2 -- Number of subpools to keep alive
  60 -- Seconds to keep an idle resource
  10 -- Max number of concurrent connections

-- | Runs an IO action with a database connection pool.
withConnPool :: (Pool Connection -> IO a) -> IO a
withConnPool action = bracket (createMariaDBPool defaultSettings) destroyAllResources action

-- | Example: Fetch all users from the database.
getAllUsers :: Pool Connection -> IO [User]
getAllUsers pool = withResource pool $ \conn -> do
  -- NOTE: This assumes a 'users' table exists with matching columns.
  query_ conn "SELECT user_id, user_name, user_email FROM users"
  -- Example: [(1, "Alice", "alice@example.com"), (2, "Bob", "bob@example.com")]
  -- The query result must be converted to [User]
  
