{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Applicative ((<$>), (<*>), (<**>))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Monoid (mconcat, mempty)
import System.IO.Error (catchIOError)

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Pg
import qualified Fynder.Email as Email
import qualified Fynder.Messaging as Messaging
import qualified Mailer
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail
import qualified Network.Metric as Metrics
import qualified Network.Socket as Socket
import qualified Options.Applicative as Optparse
import qualified RateLimit
--import qualified System.Posix.User as POSIX

--------------------------------------------------------------------------------
data StatsdConfiguration = Statsd
  { statsdHost :: String
  , statsdPort :: Socket.PortNumber
  }


--------------------------------------------------------------------------------
data Options = Options Messaging.RabbitMQConfig StatsdConfiguration String


--------------------------------------------------------------------------------
run :: Options -> IO ()
run (Options rabbitMqConf Statsd{..} connString) = do
  rabbitMqConn <- Messaging.connect rabbitMqConf
  rabbitMq <- AMQP.openChannel rabbitMqConn

  Email.establishRabbitMqConfiguration rabbitMq

  statsd <- Metrics.open Metrics.Statsd "fynder" statsdHost statsdPort
  sendMail <-
    RateLimit.rateLimit 1 $ \mail -> do
      Mail.renderSendMail mail
      Metrics.push statsd (Metrics.Counter "email" "sent" 1)
        `catchIOError` (const $ putStrLn "Couldn't write to statsd")

  dbPool <- Pool.createPool (Pg.connectPostgreSQL (Char8.pack connString))
                            Pg.close
                            10 0.5 20

  heist <- Mailer.loadTemplates
  Mailer.consumeOutbox rabbitMqConn heist dbPool sendMail

  forever (threadDelay maxBound)


--------------------------------------------------------------------------------
main :: IO ()
main = Optparse.execParser parser >>= run

 where

  parser =
    Optparse.info
      (Options <$> Messaging.rabbitOptparse
               <*> statsdParser
               <*> Optparse.strOption (mconcat [ Optparse.long "db"
                                               , Optparse.help "PostgreSQL connection string"])
               <**> Optparse.helper)
          mempty

  statsdParser =
    Statsd
      <$> Optparse.strOption (mconcat [ Optparse.long "statsd-host"
                                      , Optparse.help "Statsd host"
                                      , Optparse.value "localhost"
                                      ])
      <*> fmap fromInteger
            (Optparse.option (mconcat [ Optparse.long "statsd-port"
                                      , Optparse.value 8125
                                      , Optparse.help "Statsd port"
                                      ]))
