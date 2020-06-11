{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# language OverloadedStrings     #-}

module Networking.Rpc.Schema where

import RIO.FilePath ((</>), (<.>))
import Data.Text as T
import GHC.Generics

import Mu.Quasi.GRpc
import Mu.Schema
import Data.Int (Int32)
import Data.ByteString (ByteString)

grpc "NetSchema" (++ "Servicer") $ "protos" </> "roguelike" <.> "proto"

newtype SessionId
  = SessionId { id :: Int32 }
  deriving ( Generic
           , ToSchema NetSchema "SessionId"
           , FromSchema NetSchema "SessionId"
           )

data SessionInfo
  = SessionInfo
    { name :: T.Text
    , id :: Maybe SessionId
    } deriving ( Generic
               , ToSchema NetSchema "SessionInfo"
               , FromSchema NetSchema "SessionInfo"
               )

data State
  = NOT_RUNNING
  | RUNNING
  deriving ( Generic
           , ToSchema NetSchema "State"
           , FromSchema NetSchema "State"
           )

data SessionState
  = SessionState
    { state :: Maybe State
    , content :: ByteString
    } deriving ( Generic
               , ToSchema NetSchema "SessionState"
               , FromSchema NetSchema "SessionState"
               )