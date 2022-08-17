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
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified Data.Aeson.Parser

import Chebyshev

type API = "anint" :> Get '[JSON] Int
        :<|> "factorial" :> Capture "N" Integer :> Get '[JSON] Integer

server1 :: Server API
server1 = return 36  :<|> factorialHandler
    where factorialHandler :: Integer -> Handler Integer
          factorialHandler n = return $ factorial n

apiProxy :: Proxy API
apiProxy = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve apiProxy server1

main :: IO ()
main = run 8081 app1


