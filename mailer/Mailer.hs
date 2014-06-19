{-# LANGUAGE OverloadedStrings #-}
module Mailer where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Error.Util ((!?))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Aeson ((.=))
import Data.Monoid (mempty)
import Heist ((##))

import qualified Blaze.ByteString.Builder as Builder
import qualified Control.Error as Error
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as Pg
import qualified Fynder.Db.Read as FQ
import qualified Fynder.Email as Email
import qualified Fynder.Types as Fynder
import qualified Fynder.Types.Model.Class as Class
import qualified Fynder.Types.Model.ClassTemplate as ClassTemplate
import qualified Fynder.Types.Model.ClassType as ClassType
import qualified Fynder.Types.Model.CustomerProfile as CustomerProfile
import qualified Fynder.Types.Model.Studio as Studio
import qualified Fynder.Types.Model.User as User
import qualified Heist as Heist
import qualified Heist.Interpreted as Heist
import qualified Network.AMQP as AMQP
import qualified Network.HTTP as HTTP
import qualified Network.Mail.Mime as Mail
import qualified System.Locale as Locale
import qualified Text.XmlHtml as XmlHtml

import qualified Paths_fynder_email as Email


--------------------------------------------------------------------------------
emailToMail
  :: Email.Email
  -> Heist.HeistState Fynder.DbRO
  -> Fynder.DbRO (Maybe Mail.Mail)
emailToMail email heist = runMaybeT $ do
  splices <- templateSplices
  MaybeT $ Heist.evalHeistT
    mailBuilder
    (XmlHtml.TextNode "")
    (Heist.bindSplice "urlEncode" urlEncode $
     Heist.bindSplices splices heist)

 where

  ------------------------------------------------------------------------------
  urlEncode =
    map (XmlHtml.TextNode . Text.pack . HTTP.urlEncode . Text.unpack . XmlHtml.nodeText)
      <$> Heist.runChildren

  mailBuilder = fmap (makeMail . runTemplate) <$> Heist.evalTemplate templatePath

  runTemplate = Builder.toLazyByteString . XmlHtml.renderHtmlFragment XmlHtml.UTF8

  template = Email.emailTemplate email

  makeMail messageBody = Mail.Mail
    { Mail.mailFrom = Email.emailFrom email
    , Mail.mailTo = [ Email.emailTo email ]
    , Mail.mailCc = []
    , Mail.mailBcc = []
    , Mail.mailHeaders = [("Subject", emailSubject)]
    , Mail.mailParts = [ [ Mail.Part { Mail.partType = "text/html; charset=UTF-8"
                                     , Mail.partEncoding = Mail.None
                                     , Mail.partFilename = Nothing
                                     , Mail.partHeaders = []
                                     , Mail.partContent = messageBody
                                     } ] ]
    }

  ------------------------------------------------------------------------------
  templatePath = case template of
    (Email.ClassSpaceAvailable{}) -> "space-available"

  templateSplices = case template of
    (Email.ClassSpaceAvailable customerProfileId classId) -> do
      class_ <- MaybeT $ FQ.getClassById classId
      classTemplate <- MaybeT $ FQ.getClassTemplateById (class_ ^. Class.classTemplateId)
      classType <- MaybeT $ FQ.getClassTypeById (classTemplate ^. ClassTemplate.classTypeId)
      studio <- MaybeT $ FQ.getStudioById (classTemplate ^. ClassTemplate.studioId)
      customerProfile <- MaybeT $ FQ.getCustomerProfileById customerProfileId
      user <- MaybeT $ FQ.getUserById (customerProfile ^. CustomerProfile.userId)

      return $ do
        "fynder-class-type" ##
          textySplice (classType ^. ClassType.title)

        "fynder-studio" ##
          textySplice (studio ^. Studio.name)

        "fynder-class-date" ##
          Heist.textSplice $ Text.pack $ Time.formatTime Locale.defaultTimeLocale "%B %-e at %R" (class_ ^. Class.startTs)

        "customer" ## textySplice (user ^. User.name)

  emailSubject = case template of
    (Email.ClassSpaceAvailable{}) -> "A Space is Available!"


  ------------------------------------------------------------------------------
  textySplice = Heist.textSplice . Lens.review Fynder.texty

--------------------------------------------------------------------------------
-- | Takes a 'AMQP.Connection' and returns a callback that can be used on the
-- outbox queue. To form the callback, IO is performed to open a 'AMQP.Channel'
-- for the callback, so that it can publish failures.
consumeOutbox :: AMQP.Connection
              -> Heist.HeistState Fynder.DbRO
              -> Pool.Pool Pg.Connection
              -> (Mail.Mail -> IO ())
              -> IO ()
consumeOutbox rabbitMqConn heist connectionPool sendMail = do
  rabbitMq <- AMQP.openChannel rabbitMqConn

  void $ AMQP.consumeMsgs rabbitMq Email.outboxQueue AMQP.Ack $
    \(msg, env) -> do
      maybe
        (publishFailure rabbitMq Email.invalidKey (unableToDecode msg))
        (Error.eitherT (publishFailure rabbitMq Email.unroutableKey) return .
           trySendEmail)
        (Aeson.decode $ AMQP.msgBody msg)

      AMQP.ackEnv env

 where

  unableToDecode msg =
    let errorMessage = case Aeson.decode (AMQP.msgBody msg) :: Maybe Aeson.Value of
          Just _  -> "Unknown JSON schema"
          Nothing -> "Malformed JSON syntax"

    in AMQP.newMsg { AMQP.msgBody = Aeson.encode $ Aeson.object
                      [ "error" .= (errorMessage :: String)
                      , "json" .= Text.decodeUtf8 (LBS.toStrict (AMQP.msgBody msg))
                      ]
                   }

  trySendEmail email = tryFormEmail >>= trySend

   where

    failureMessage e =
      AMQP.newMsg { AMQP.msgBody = Aeson.encode $ Aeson.object
                      [ "email" .= email
                      , "error" .= (e :: Text.Text)
                      ]
                  }

    tryFormEmail =
      let mkEmail = Pool.withResource connectionPool $
                      Fynder.runDbR (emailToMail email heist)
      in mkEmail !? (failureMessage "Couldn't render template")

    trySend mail =
      let exceptionMessage e =
            failureMessage (Text.pack (show (e :: SomeException)))
      in Error.bimapEitherT exceptionMessage id $
           Error.EitherT $ try (sendMail mail)

  publishFailure rabbitMq = AMQP.publishMsg rabbitMq Email.failureExchange


--------------------------------------------------------------------------------
loadTemplates :: Monad m => IO (Heist.HeistState m)
loadTemplates = fmap (either (error . show) id) $ do
  templatesDir <- Email.getDataFileName "templates"
  Error.runEitherT $ do
    Heist.initHeist mempty
      { Heist.hcTemplateLocations =
          [ Heist.loadTemplates templatesDir ]

      , Heist.hcLoadTimeSplices = Heist.defaultLoadTimeSplices
      }
