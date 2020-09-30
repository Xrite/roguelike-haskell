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


module Networking.Test where

import RIO.FilePath ((</>), (<.>))
import Data.Text as T
import GHC.Generics

import Mu.Quasi.GRpc
import Mu.Schema

grpc "TheSchema" id $ "protos" </> "test" <.> "proto"

newtype HelloRequest
  = HelloRequest { name :: T.Text }
  deriving ( Generic
           , ToSchema TheSchema "HelloRequest"
           , FromSchema TheSchema "HelloRequest"
           )

newtype HelloReply
  = HelloReply { message :: T.Text }
  deriving ( Generic
           , ToSchema TheSchema "HelloReply"
           , FromSchema TheSchema "HelloReply"
           )
