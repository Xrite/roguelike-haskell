module AppOptions (AppOptions (..), parseArguments) where

import Options.Applicative
import Data.Semigroup ((<>))

data AppOptions
  = ServerOptions Int Int
  | LocalOptions

argParser :: Parser AppOptions
argParser
  = ServerOptions
    <$> option auto
        ( long "serverport"
       <> short 's'
       <> metavar "PORT"
       <> help "Start server on this port" )
    <*> option auto
        ( long "seed"
       <> short 'r'
       <> value 0
       <> help "Seed to use for server. Use random if set to zero." )
  <|>
    LocalOptions
      <$ switch
            ( long "client"
           <> short 'c'
           <> help "start local client" )

parseArguments :: IO AppOptions
parseArguments = execParser inf
  where
    inf = info (argParser <**> helper)
      ( fullDesc
     <> progDesc "Opens the game or launches game server" )