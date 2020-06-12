{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Networking.Rpc.Schema where

import Data.Text as T
import GHC.Generics
import RIO.FilePath ((<.>), (</>))

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Mu.Quasi.GRpc
import Mu.Schema

grpc "NetSchema" (++ "Servicer") $ "protos" </> "roguelike" <.> "proto"

newtype SessionId =
  SessionId
    { id :: Int32
    }
  deriving ( Generic
           , ToSchema NetSchema "SessionId"
           , FromSchema NetSchema "SessionId"
           )

data SessionInfo =
  SessionInfo
    { name :: T.Text
    , id :: Maybe SessionId
    }
  deriving ( Generic
           , ToSchema NetSchema "SessionInfo"
           , FromSchema NetSchema "SessionInfo"
           )

data State
  = NOT_RUNNING
  | RUNNING
  deriving (Generic, ToSchema NetSchema "State", FromSchema NetSchema "State")

data SessionState =
  SessionState
    { state :: Maybe State
    , content :: ByteString
    }
  deriving ( Generic
           , ToSchema NetSchema "SessionState"
           , FromSchema NetSchema "SessionState"
           )

data Empty =
  Empty
  deriving (Generic, ToSchema NetSchema "Empty", FromSchema NetSchema "Empty")

newtype SessionsList =
  SessionsList
    { sessions :: [SessionInfo]
    }
  deriving ( Generic
           , ToSchema NetSchema "SessionsList"
           , FromSchema NetSchema "SessionsList"
           )
