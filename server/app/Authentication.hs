{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}

module Authentication where

import User(User(..))
import Data.Aeson
import GHC.Generics
import Data.Proxy
import System.IO
import Network.Wai.Handler.Warp
import Servant as S
import Control.Monad.IO.Class (liftIO)
import Servant.Auth as SA
import Servant.Auth.Server as SAS

