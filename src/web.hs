{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString,unpack)
import Data.ByteString.UTF8 (toString)
import Data.List
import Data.Maybe
import Data.Either
import Data.String.Conversions
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified Data.Aeson.Parser
import Network.Wai.Logger (withStdoutLogger)
import Network.HTTP.Req as R
import Data.Yaml

import Chebyshev

type API = "isalive" :> Get '[PlainText,JSON] [Char]
    :<|> "factorial" :> Capture "N" Integer :> Get '[JSON] (Maybe Integer)
    :<|> "fibonacci" :> Capture "N" Integer :> Get '[JSON] (Maybe Integer)
    :<|> "chebyshev1st" :> Capture "x" Double :> Capture "N" Integer :> Get '[JSON] (Maybe Double)
    :<|> "chebyshev2nd" :> Capture "x" Double :> Capture "N" Integer :> Get '[JSON] (Maybe Double)
    :<|> "laguerre" :> Capture "x" Double :> Capture "N" Integer :> Get '[JSON] (Maybe Double)
    :<|> "portnum" :> Get '[JSON] Int
    :<|> "dl" :> Get '[JSON] [Char]

server1 :: Int -> Server API
server1 nConfig = isaliveHandler 
    :<|> factorialHandler 
    :<|> fibonacciHandler 
    :<|> chebyshev1stHandler
    :<|> chebyshev2ndHandler
    :<|> laguerreHandler
    :<|> portnumHandler
    :<|> dlHandler
  where 
    isaliveHandler :: Handler [Char]
    isaliveHandler = do
        -- do expects lines to be statments of the Handler monad. liftIO just converts an IO monad to a Handler one
        -- That's only okay b/c Handler has IO inside it or is compatible or something like that.
        liftIO $ putStrLn "Running /isalive"
        return "OK"

    factorialHandler :: Integer -> Handler (Maybe Integer)
    factorialHandler n = return $ factorial n

    fibonacciHandler :: Integer -> Handler (Maybe Integer)
    fibonacciHandler n = return $ fibonacci n

    chebyshev1stHandler :: Double -> Integer -> Handler (Maybe Double)
    chebyshev1stHandler x n = return $ chebyshev1st x n

    chebyshev2ndHandler :: Double -> Integer -> Handler (Maybe Double)
    chebyshev2ndHandler x n = return $ chebyshev2nd x n

    laguerreHandler :: Double -> Integer -> Handler (Maybe Double)
    laguerreHandler x n = return $ laguerre x n

    portnumHandler :: Handler Int
    portnumHandler = do
        liftIO $ putStrLn "Running /portnum"
        return nConfig

    dlHandler :: Handler [Char]
    dlHandler = do
        liftIO $ putStrLn "Running /dl"
        -- converts getWebPage IO monad value into required Handler monad
        -- return type of function
        liftIO $ getWebPage

getWebPage :: IO [Char]
getWebPage = R.runReq R.defaultHttpConfig $ do
    liftIO $ putStrLn "Running getWebPage"
    resp <- R.req
            R.GET
            (R.http "httpbin.org" /: "html")
            R.NoReqBody
            R.bsResponse
            mempty
    let result = toString $ responseBody resp
    liftIO $ putStrLn result
    return result


apiProxy :: Proxy API
apiProxy = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Int -> Application
app1 portNum = serve apiProxy $ server1 portNum

-- Configuration with Yaml
data ServerConfig = ServerConfig {
    port :: Int
} deriving (Show, Eq, Ord, Generic)

instance FromJSON ServerConfig

logConfigLoading :: IO (Either ParseException ServerConfig) -> String -> IO ()
logConfigLoading x fn = do
    -- The isSuccess is wrapped in an implicit IO monad in this statement.
    -- This is helpful for unpacking the boolean from the IO monad it picks up with <$>
    -- I couldn't figure out how to do it in loadConfig, because I couldn't unwrap it
    -- Here it is easy!
    isSuccess <- isRight <$> x
    xShow <- either show show <$> x
    when isSuccess $ putStrLn $ "Successfully loaded config from " ++ fn
    unless isSuccess $ putStrLn $ "Failed to load config " ++ fn ++ " due to '" ++ xShow  ++ "', using defaults"

loadConfig :: IO ServerConfig
loadConfig = do
    let fn = "config-web.yaml"
    let defaults = ServerConfig 8081
    let rawDecoded = decodeFileEither fn
    liftIO $ logConfigLoading rawDecoded fn
    fromRight defaults <$> rawDecoded
    
------------------------------------------------------------------

main :: IO ()
main = do
    config <- loadConfig
    putStrLn $ "Server configuration: " ++ (show config)
    putStrLn $ "Server running at http://localhost:" ++ ( show $ Main.port config)
    withStdoutLogger $ \logger ->
        runSettings (setPort (Main.port config) $ setLogger logger $ defaultSettings) $ app1 $ Main.port config
