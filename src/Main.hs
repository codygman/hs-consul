{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Network.URI
import Network.Wreq
import Control.Lens
import Data.Text
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.Monoid((<>))

data ConsulInfo = ConsulInfo { consulHost :: String
                             , consulPort :: Int
                             } deriving Show

data AppError = ServerDown | KeyParseError String | UnknownError

data AppEnv = AppEnv { appInfo :: ConsulInfo }

newtype App a = App { unApp :: ExceptT AppError (ReaderT AppEnv IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader AppEnv
           , MonadError AppError
           , MonadIO)

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp ae app = (runReaderT (runExceptT (unApp app))) ae

defaultConsul = ConsulInfo { consulHost = "127.0.0.1"
                           , consulPort = 8500
                           }

runAppEx = runApp (AppEnv defaultConsul) consulUrl

-- TODO return a more limited KeyError type here
consulUrl :: App URI
consulUrl = do
  host <- asks (consulHost . appInfo)
  port <- asks (consulPort . appInfo)
  let uriString =  ("http://" ++ host ++ ":" ++ show port)
  case parseURI uriString of
    Just u -> do
      -- liftIO $ print u
      return (u :: URI)
    -- Just u -> return ((Right (u :: URI)) :: Either AppError URI)
    Nothing -> throwError (KeyParseError $ "error decoding consul url '" ++ uriString ++ "'")

-- catching an error made by throwError
-- throwErrorExample = runExceptT . catchError (throwError "Bzzt") $ \e -> do
--   liftIO (putStrLn $ "ERROR: " <> e)
--   pure "All ok now!"

-- keyEndpoint ci = do
--   let url = consulUrl ci
--       keyEndpoint = show url ++ "/v1/kv/"
--   case parseURI keyEndpoint of
--     Just u -> keyEndpoint
--     Nothing -> error $ "'" ++ keyEndpoint ++ "' is not a valid URL"


-- getKey ci key= do
--   let x = parseURI (keyEndpoint ci ++ key)
--   case x of
--     Just keyUrl -> do
--       get (show keyUrl)
--     Nothing -> error $ "'" ++ show x ++ "'  was not a valid key url"

-- setKey ci key contents = do
--   let x = parseURI (keyEndpoint ci ++ key)
--   case x of
--     Just keyUrl -> do
--       put (show keyUrl) contents
--     Nothing -> error $ "'" ++ show x ++ "'  was not a valid key url"


-- safeLoadFile :: FilePath -> IO (Either IOException String)
-- safeLoadFile f = (Right <$> readFile f) `catch` (\e -> pure (Left e))

-- fileChars :: FilePath -> IO (Either IOException Int)
-- fileChars = fmap (fmap Prelude.length) . safeLoadFile

-- safePrintFile :: FilePath -> IO (Either IOException ())
-- safePrintFile f = safeLoadFile f >>= traverse putStrLn
