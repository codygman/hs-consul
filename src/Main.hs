{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.URI
import Network.Wreq
import Control.Lens
import Data.Text

data ConsulInfo = ConsulInfo { consulHost :: String
                             , consulPort :: Int
                             } deriving Show

defaultConsul = ConsulInfo { consulHost = "127.0.0.1"
                           , consulPort = 8500
                           }

consulUrl ci = do
  let uriString =  ("http://" ++ consulHost ci ++ ":" ++ show (consulPort ci))
  case parseURI uriString of
    Just u -> u
    Nothing -> error $ "error decoding consul url '" ++ uriString ++ "'"

keyEndpoint ci = do
  let url = consulUrl ci
      keyEndpoint = show url ++ "/v1/kv/"
  case parseURI keyEndpoint of
    Just u -> keyEndpoint
    Nothing -> error $ "'" ++ keyEndpoint ++ "' is not a valid URL"


getKey ci key= do
  let x = parseURI (keyEndpoint ci ++ key)
  case x of
    Just keyUrl -> do
      get (show keyUrl)
    Nothing -> error $ "'" ++ show x ++ "'  was not a valid key url"

setKey ci key contents = do
  let x = parseURI (keyEndpoint ci ++ key)
  case x of
    Just keyUrl -> do
      put (show keyUrl) contents
    Nothing -> error $ "'" ++ show x ++ "'  was not a valid key url"

