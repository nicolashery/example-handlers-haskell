{-# LANGUAGE QuasiQuotes #-}

module App.Cart
  ( CartException (CartException),
    CartId (CartId, unCartId),
    BookingId (BookingId, unBookingId),
    PaymentId (PaymentId, unPaymentId),
    HasCartConfig (getBookingUrl, getPaymentUrl),
    CartStatus (CartStatusOpen, CartStatusLocked, CartStatusPurchased),
    getCartStatus,
    processBooking,
    processPayment,
  )
where

import App.Db (HasDbPool, withConn)
import App.Req (isStatusCodeException')
import Blammo.Logging (Message ((:#)), MonadLogger, logInfo, logWarn, (.=))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Maybe (fromJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only (Only), ResultError (ConversionFailed, UnexpectedNull), query)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( HttpException,
    JsonResponse,
    MonadHttp,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    jsonResponse,
    req,
    responseBody,
    responseStatusCode,
    useHttpURI,
  )
import Text.URI (mkURI)
import UnliftIO (Exception, MonadUnliftIO, Typeable)
import UnliftIO.Exception (catch, throwIO)

newtype CartException = CartException Text
  deriving (Show, Typeable)

instance Exception CartException

newtype CartId = CartId {unCartId :: Text}
  deriving (Eq, Read, Show, Generic, ToJSON, ToField)

class HasCartConfig env where
  getBookingUrl :: env -> Text
  getPaymentUrl :: env -> Text

newtype BookingId = BookingId {unBookingId :: Text}
  deriving (FromJSON, ToJSON)

newtype PaymentId = PaymentId {unPaymentId :: Text}
  deriving (FromJSON, ToJSON)

data BookingRequest = BookingRequest
  { bookingRequestVenue :: Text,
    bookingRequestSeats :: [Text]
  }
  deriving (Generic)

instance ToJSON BookingRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 14}

data BookingResponse = BookingResponse
  { bookingResponseBookingId :: BookingId,
    bookingResponseSeatsReserved :: Int
  }
  deriving (Generic)

instance FromJSON BookingResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 15}

data PaymentRequest = PaymentRequest
  { paymentRequestCardholderName :: Text,
    paymentRequestCardNumber :: Text
  }
  deriving (Generic)

instance ToJSON PaymentRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 14}

data PaymentResponse = PaymentResponse
  { paymentResponsePaymentId :: PaymentId,
    paymentResponseAmountCharged :: Int
  }
  deriving (Generic)

instance FromJSON PaymentResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 15}

data CartStatus
  = CartStatusOpen
  | CartStatusLocked
  | CartStatusPurchased
  deriving (Eq)

instance FromField CartStatus where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) =
    case bs of
      "open" -> pure CartStatusOpen
      "locked" -> pure CartStatusLocked
      "purchased" -> pure CartStatusPurchased
      _ -> returnError ConversionFailed f ""

getCartStatus :: (MonadReader env m, HasDbPool env, MonadIO m) => CartId -> m (Maybe CartStatus)
getCartStatus cartId = do
  result <- withConn $ \conn -> query conn qry args
  case result of
    [Only cartStatus] -> pure $ Just cartStatus
    _ -> pure Nothing
  where
    qry = [sql|select status from carts where id = ? limit 1|]
    args = Only cartId

processBooking ::
  forall env m.
  (MonadReader env m, HasCartConfig env, MonadLogger m, MonadHttp m, MonadUnliftIO m) =>
  CartId ->
  m BookingId
processBooking cartId = do
  bookingUrl <- asks getBookingUrl
  logInfo $ "Booking starting" :# ["cart_id" .= cartId, "booking_url" .= bookingUrl]
  liftIO $ threadDelay (2 * 1000 * 1000)
  uri <- liftIO $ mkURI bookingUrl
  let (url, options) = fromJust (useHttpURI uri)
      venue = if cartId == CartId "ghi789" then "TDE8751" else "HRT3974"
      bookingRequest =
        BookingRequest
          { bookingRequestVenue = venue,
            bookingRequestSeats = ["D31", "D32", "D33"]
          }
      handleFailure :: HttpException -> m (JsonResponse BookingResponse)
      handleFailure e = do
        case isStatusCodeException' e of
          Just (r, b) -> do
            let statusCode = responseStatusCode r
                msg = cs b
            logWarn $
              "Booking failed"
                :# [ "cart_id" .= cartId,
                     "status_code" .= statusCode,
                     "response" .= msg
                   ]
            throwIO $ CartException ("Booking failed: " <> msg)
          Nothing -> throwIO e
  response <-
    req POST url (ReqBodyJson bookingRequest) jsonResponse options
      `catch` handleFailure
  let bookingResponse :: BookingResponse
      bookingResponse = responseBody response
      bookingId = bookingResponseBookingId bookingResponse
  logInfo $ "Booking successful" :# ["cart_id" .= cartId, "booking_id" .= bookingId]
  pure bookingId

processPayment ::
  forall env m.
  (MonadReader env m, HasCartConfig env, MonadLogger m, MonadHttp m, MonadUnliftIO m) =>
  CartId ->
  m PaymentId
processPayment cartId = do
  paymentUrl <- asks getPaymentUrl
  logInfo $ "Payment starting" :# ["cart_id" .= cartId, "payment_url" .= paymentUrl]
  liftIO $ threadDelay (1 * 1000 * 1000)
  uri <- liftIO $ mkURI paymentUrl
  let (url, options) = fromJust (useHttpURI uri)
      cardNumber = if cartId == CartId "ghi789" then "a192901463306478" else "5192901463306478"
      paymentRequest =
        PaymentRequest
          { paymentRequestCardholderName = "John Doe",
            paymentRequestCardNumber = cardNumber
          }
      handleFailure :: HttpException -> m (JsonResponse PaymentResponse)
      handleFailure e = do
        case isStatusCodeException' e of
          Just (r, b) -> do
            let statusCode = responseStatusCode r
                msg = cs b
            logWarn $
              "Payment failed"
                :# [ "cart_id" .= cartId,
                     "status_code" .= statusCode,
                     "response" .= msg
                   ]
            throwIO $ CartException ("Payment failed: " <> msg)
          Nothing -> throwIO e
  response <-
    req POST url (ReqBodyJson paymentRequest) jsonResponse options
      `catch` handleFailure
  let paymentResponse :: PaymentResponse
      paymentResponse = responseBody response
      paymentId = paymentResponsePaymentId paymentResponse
  logInfo $ "Payment successful" :# ["cart_id" .= cartId, "payment_id" .= paymentId]
  pure paymentId
