{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module AppDesign where



data DbConfig =
    DbConfig {
        dbConn :: DbConnection
    ,   schema :: Schema
    }

data NetworkConfig =
    NetConfig {
        port :: Port
    ,   ssl  :: Ssl
    }

data AppConfig =
    AppConfig {
        appDbConfig :: DbConfig
    ,   appNetConfig :: NetworkConfig
    }


data DbError =
    QueryError Text
  | InvalidConnection

data NetworkError =
    Timeout Int
  | ServerOnFire

data AppError =
    AppDbError DbError
  | AppNetError NetworkError



newtype App a =
    App
    {
        unApp :: ReaderT AppConfig (ExceptT AppError IO) a
    } deriving (
        Functor,
        Applicative,
        Monad,
        MonadReader AppConfig,
        MonadError AppError,
        MonadIO
    )


class Monad m => MonadReader r m | m -> r where

    -- Retrieves the monad environment.
    ask :: mr
    ask = reader id

    -- Retrieves a function of the current environment.
    reader :: (r -> a) -> m a
    readef f = do
        r <- ask
        return (f r)


getPort :: MonadReader NetworkConfig m => m Port
getPort = reader port

getPort :: MonadReader NetConfig m => m Port
getPort = do
    cfg <- ask
    return (port cfg)


-- MonadIO
class (Monad m) => MonadIO m where
    -- Lift a computation from the 'IO' monad.
    liftIO :: IO a -> m a


printM :: MonadIO m => String -> m ()
printM s = liftIO (putStrLn s)



class (Monad m) => MonadError e m | m -> e where
    -- Is used within a monadic computation to
    -- begin exception processing.
    throwError :: e -> m a

    -- A handler function to handle previous errors
    -- and return to normal execution
    catchError :: m a -> (e -> m a) -> m a


mightFail :: MonadError Err m => m Int
couldFail :: MonadError Err m => m String

maybeFail :: MonadError Err m => m (Maybe (Int, String))
maybeFail =
    (
        do a <- mightFail
           b <- couldFail
           pure (Just (a, b))
    ) 'catchError' (\err -> pure Nothing)


-- instance MonadReader AppConfig App
ask :: App AppConfig

-- instance MonadError AppError App
throwError :: AppError -> App a
catchError :: App a -> (AppError -> App a) -> App a

-- instance MonadIO App
liftIO :: IO a -> App a








-- loadFromDb :: App MyData
-- This is better
loadFromDb :: (MonadReader DbConfig m,
               MonadError  DbError  m,
               MonadIO m
    ) => m MyData


-- sendOverNet :: MyData -> App ()
-- This is better
sendOverNet :: (MonadReader NetworkConfig m,
                MonadError  NetworkError  m,
                MonadIO m
    ) => m MyData -> m ()


loadAndSend :: App ()
loadAndSend = loadFromDb >>= sendOverNet


main :: IO ()
main = undefined




