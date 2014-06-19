{-# LANGUAGE RecordWildCards #-}
module Fynder.Messaging
    ( RabbitMQConfig(..)
    , connect
    , rabbitOptparse
    ) where

import Control.Applicative ((<*>), (<$>))
import Data.Monoid (mconcat)
import Data.Text (Text)

import qualified Data.Text as Text
import qualified Network.AMQP as AMQP
import qualified Options.Applicative as Optparse

--------------------------------------------------------------------------------
data RabbitMQConfig = RabbitMQConfig
  { rabbitHost :: String
  , rabbitVHost :: Text
  , rabbitUser :: Text
  , rabbitPassword :: Text
  }
  deriving (Eq, Read, Show)


--------------------------------------------------------------------------------
connect :: RabbitMQConfig -> IO AMQP.Connection
connect RabbitMQConfig {..} =
  AMQP.openConnection rabbitHost rabbitVHost rabbitUser rabbitPassword


--------------------------------------------------------------------------------
rabbitOptparse :: Optparse.Parser RabbitMQConfig
rabbitOptparse =
  RabbitMQConfig
    <$> Optparse.strOption (mconcat [ Optparse.long "rabbit-host"
                                    , Optparse.value "127.0.0.1"
                                    , Optparse.help "RabbitMQ host"
                                    ])
    <*> textOption (mconcat [ Optparse.long "rabbit-vhost"
                            , Optparse.value "/email"
                            , Optparse.help "RabbitMQ virtual host for email"
                            ])
    <*> textOption (mconcat [ Optparse.long "rabbit-user"
                            , Optparse.value "guest"
                            , Optparse.help "RabbitMQ username"
                            ])
    <*> textOption (mconcat [ Optparse.long "rabbit-password"
                            , Optparse.value "guest"
                            , Optparse.help "RabbitMQ username"
                            ])

--------------------------------------------------------------------------------
textOption :: Optparse.Mod Optparse.OptionFields String -> Optparse.Parser Text
textOption = fmap Text.pack . Optparse.strOption
