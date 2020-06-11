{-# LANGUAGE TypeApplications #-}
{-# language FlexibleContexts      #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# language DataKinds             #-}

module Networking.Testmain where

import Mu.GRpc.Server
import Mu.Server

import Networking.Test

main :: IO ()
main = runGRpcApp msgProtoBuf 8080 server


sayHello :: (MonadServer m) => HelloRequest -> m HelloReply
sayHello (HelloRequest nm) = return . HelloReply $ "HI " <> nm <> "!!!"

server :: MonadServer m => SingleServerT Greeter m _
server = singleService (method @"SayHello" sayHello)

