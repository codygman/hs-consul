{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Consul where

import Network.URI
import Network.Wreq
import Control.Lens
import Data.Text
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.Monoid((<>))
import qualified Data.ByteString.Lazy as LBS



data ConsulInfo = ConsulInfo { consulHost :: String
                             , consulPort :: Int
                             } deriving Show

defaultConsul = ConsulInfo { consulHost = "127.0.0.1"
                           , consulPort = 8500
                           }

data AppError = ServerDown | Http404 | Http500 | KeyParseError String | UnknownError deriving Show

data AppEnv = AppEnv { appInfo :: ConsulInfo }

newtype App a = App { unApp :: ExceptT AppError (ReaderT AppEnv IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader AppEnv
           , MonadError AppError
           , MonadIO)

-- safe versions of wreq's get/put/delete that don't throw exceptions
-- TODO update these, set checkResponse Nothing didn't do the trick
-- safeGet url = getWith (set checkStatus (Just $ \_ _ _ -> Nothing) defaults) url
-- safePut url = putWith (set checkStatus (Just $ \_ _ _ -> Nothing) defaults) url
-- safeDelete url = deleteWith (set checkStatus (Just $ \_ _ _ -> Nothing) defaults) url

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp ae app = (runReaderT (runExceptT (unApp app))) ae

-- TODO Maybe return a more limited KeyError type here?
consulUrl :: App URI
consulUrl = do
  host <- asks (consulHost . appInfo)
  port <- asks (consulPort . appInfo)
  let uriString =  ("http://" ++ host ++ ":" ++ show port)
  case parseURI uriString of
    Just u -> return u
    Nothing -> throwError (KeyParseError $ "error decoding consul url '" ++ uriString ++ "'")

keyEndpoint :: App URI
keyEndpoint = do
  baseUrl <- consulUrl
  let keyEndpoint = show baseUrl ++ "/v1/kv/"
  case parseURI keyEndpoint of
    Just u -> return u
    Nothing -> throwError (KeyParseError $ "error building keyEndpoint url")

getKey :: String -> App (Response LBS.ByteString)
getKey key = do
  endpoint <- keyEndpoint
  let x = parseURI (show endpoint ++ key)
  case x of
    Just keyUrl -> do
      r <- liftIO $ get (show keyUrl)
      let responseCode = r ^. responseStatus . statusCode
      case responseCode of
        200 -> pure r
        404 -> throwError Http404
        500 -> throwError Http500
        _ -> throwError UnknownError
    Nothing -> throwError (KeyParseError $ "error decoding consul url '")

setKey key contents = do
  endpoint <- keyEndpoint
  let x = parseURI (show endpoint ++ key)
  case x of
    Just keyUrl -> liftIO $ put (show keyUrl) contents
    Nothing -> throwError (KeyParseError $ "error appending: '" ++ show endpoint ++ "' and '" ++ key ++"'")

-- TODO handle recursively deleting
-- TODO PLEASE handle building urls better :S It caused an error
deleteKey key = do
  endpoint <- keyEndpoint
  let x = parseURI (show endpoint ++ key)
  case x of
    Just keyUrl -> liftIO $ delete (show keyUrl)
    Nothing -> throwError (KeyParseError $ "error appending: '" ++ show endpoint ++ "' and '" ++ key ++"'")

runAppEx = runApp (AppEnv defaultConsul) $ do
  let key = "test"

  -- TODO figure out how to catch this error and print "expected error" when it shows up otherwise it short-circuits
  -- liftIO $ putStrLn "Before inserting:"
  -- getKey key >>= liftIO . print
  -- liftIO $ putStrLn ""
  -- notes I left myself on catching errors from throwError:
  --- catching an error made by throwError
  --- throwErrorExample = runExceptT . catchError (throwError "Bzzt") $ \e -> do
  ---   liftIO (putStrLn $ "ERROR: " <> e)
  ---   pure "All ok now!"

  setKey key ("stuff" :: LBS.ByteString)

  liftIO $ putStrLn "after setting key"
  getKey key >>= liftIO . print
  liftIO $ putStrLn ""

  deleteKey key

  liftIO $ putStrLn "after deleting key"
  getKey key >>= liftIO . print
