{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Fynder.Email
    ( Email(..)
    , Template(..)

      -- * Messaging Setup
    , establishRabbitMqConfiguration

      -- * Exchanges
    , outboxExchange
    , failureExchange

      -- * Routing keys
    , invalidKey
    , unroutableKey

      -- * Queues
    , outboxQueue
    , invalidQueue
    , unroutableQueue

      -- * Queueing emails
    , enqueueEmail
    ) where

import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail


--------------------------------------------------------------------------------
deriving instance Eq Mail.Address
deriving instance Generic Mail.Address
deriving instance Show Mail.Address

data Email = Email
    { emailTemplate :: Template
    , emailTo :: Mail.Address
    , emailFrom :: Mail.Address
    }
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Mail.Address
instance Aeson.FromJSON Email

instance Aeson.ToJSON Mail.Address
instance Aeson.ToJSON Email


--------------------------------------------------------------------------------
data Template = PasswordReset { passwordResetEditor :: Text.Text }
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Template

instance Aeson.ToJSON Template

--------------------------------------------------------------------------------
outboxExchange, failureExchange :: Text
invalidKey, unroutableKey :: Text
outboxQueue, unroutableQueue, invalidQueue :: Text

outboxExchange = "outbox"
failureExchange = "failure"

invalidKey = "invalid"
unroutableKey = "unroutable"

outboxQueue = "outbox"
invalidQueue = "outbox.invalid"
unroutableQueue = "outbox.unroutable"

--------------------------------------------------------------------------------
establishRabbitMqConfiguration :: AMQP.Channel -> IO ()
establishRabbitMqConfiguration rabbitMq = do
  AMQP.declareExchange rabbitMq
    AMQP.newExchange { AMQP.exchangeName = outboxExchange
                     , AMQP.exchangeType = "fanout"
                     }

  AMQP.declareExchange rabbitMq
    AMQP.newExchange { AMQP.exchangeName = failureExchange
                     , AMQP.exchangeType = "direct"
                     }


  AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = outboxQueue }

  AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = invalidQueue }

  AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = unroutableQueue }


  AMQP.bindQueue rabbitMq outboxQueue outboxExchange ""
  AMQP.bindQueue rabbitMq invalidQueue failureExchange invalidKey
  AMQP.bindQueue rabbitMq unroutableQueue failureExchange unroutableKey


--------------------------------------------------------------------------------
enqueueEmail :: AMQP.Channel -> Email -> IO ()
enqueueEmail rabbitMq email =
  AMQP.publishMsg rabbitMq outboxExchange ""
    AMQP.newMsg
      { AMQP.msgBody = Aeson.encode email
      , AMQP.msgDeliveryMode = Just AMQP.Persistent
      }
