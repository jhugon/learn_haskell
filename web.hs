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
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified Data.Aeson.Parser

import Chebyshev

type API = "isalive" :> Get '[PlainText,JSON] [Char]
        :<|> "factorial" :> Capture "N" Integer :> Get '[JSON] Integer
        :<|> "fibonacci" :> Capture "N" Integer :> Get '[JSON] Integer
        :<|> "chebyshev1st" :> Capture "x" Double :> Capture "N" Integer :> Get '[JSON] Double
        :<|> "chebyshev2nd" :> Capture "x" Double :> Capture "N" Integer :> Get '[JSON] Double
        :<|> "laguerre" :> Capture "x" Double :> Capture "N" Integer :> Get '[JSON] Double
        :<|> "portnum" :> Get '[JSON] Int

server1 :: Int -> Server API
server1 nConfig = isaliveHandler 
    :<|> factorialHandler 
    :<|> fibonacciHandler 
    :<|> chebyshev1stHandler
    :<|> chebyshev2ndHandler
    :<|> laguerreHandler
    :<|> portnum
         where isaliveHandler :: Handler [Char]
               isaliveHandler = do
                    -- do expects lines to be statments of the Handler monad. liftIO just converts an IO monad to a Handler one
                    liftIO $ putStrLn "Running /isalive"
                    return "OK"

               factorialHandler :: Integer -> Handler Integer
               factorialHandler n = return $ factorial n

               fibonacciHandler :: Integer -> Handler Integer
               fibonacciHandler n = return $ fibonacci n

               chebyshev1stHandler :: Double -> Integer -> Handler Double
               chebyshev1stHandler x n = return $ chebyshev1st x n

               chebyshev2ndHandler :: Double -> Integer -> Handler Double
               chebyshev2ndHandler x n = return $ chebyshev2nd x n

               laguerreHandler :: Double -> Integer -> Handler Double
               laguerreHandler x n = return $ laguerre x n

               portnum :: Handler Int
               portnum = do
                    liftIO $ putStrLn "Running /portnum"
                    return nConfig


apiProxy :: Proxy API
apiProxy = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Int -> Application
app1 portNum = serve apiProxy $ server1 portNum

main :: IO ()
main = do
    let portNum = 8081
    putStrLn $ "Server running at http://localhost:" ++ show portNum
    run portNum $ app1 portNum
