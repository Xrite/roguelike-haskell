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

data Empty =
  Empty
  deriving (Generic, ToSchema NetSchema "Empty", FromSchema NetSchema "Empty")

newtype SessionId =
  SessionId
    { idx :: Int32
    }
  deriving (Generic, ToSchema NetSchema "SessionId", FromSchema NetSchema "SessionId")

newtype PlayerId =
  PlayerId
    { idx :: Int32
    }
  deriving (Generic, ToSchema NetSchema "PlayerId", FromSchema NetSchema "PlayerId")

data SessionInfo =
  SessionInfo
    { name :: T.Text
    , idx :: Maybe SessionId
    }
  deriving (Generic, ToSchema NetSchema "SessionInfo", FromSchema NetSchema "SessionInfo")

newtype SessionsList =
  SessionsList
    { sessions :: [SessionInfo]
    }
  deriving (Generic, ToSchema NetSchema "SessionsList", FromSchema NetSchema "SessionsList")

data State
  = NOT_RUNNING
  | RUNNING
  deriving (Generic, ToSchema NetSchema "State", FromSchema NetSchema "State")

data ActionRequest =
  ActionRequest
    { sessionId :: Maybe SessionId
    , playerId :: Maybe PlayerId
    , moveX :: Int32
    , moveY :: Int32
    }
  deriving (Generic, ToSchema NetSchema "ActionRequest", FromSchema NetSchema "ActionRequest")

data PlayerClickSlotRequest =
  PlayerClickSlotRequest
    { sessionId :: Maybe SessionId
    , playerId :: Maybe PlayerId
    , slotIdx :: Int32
    }
   deriving (Generic, ToSchema NetSchema "PlayerClickSlotRequest", FromSchema NetSchema "PlayerClickSlotRequest")

data PlayerClickItemRequest =
  PlayerClickItemRequest
    { sessionId :: Maybe SessionId
    , playerId :: Maybe PlayerId
    , itemIdx :: Int32
    }
   deriving (Generic, ToSchema NetSchema "PlayerClickItemRequest", FromSchema NetSchema "PlayerClickItemRequest")

data RemovePlayerRequest =
  RemovePlayerRequest
    { sessionId :: Maybe SessionId
    , playerId :: Maybe PlayerId
    }
   deriving (Generic, ToSchema NetSchema "RemovePlayerRequest", FromSchema NetSchema "RemovePlayerRequest")

data SessionState =
  SessionState
    { state :: Maybe State
    , content :: ByteString
    }
  deriving (Generic, ToSchema NetSchema "SessionState", FromSchema NetSchema "SessionState")
