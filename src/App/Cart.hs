module App.Cart
  ( CartException (CartException),
    CartId (CartId, unCartId),
    BookingId (BookingId, unBookingId),
    PaymentId (PaymentId, unPaymentId),
    HasCartConfig (getBookingUrl, getPaymentUrl),
    processBooking,
    processPayment,
  )
where

import Blammo.Logging (Message ((:#)), MonadLogger, logInfo, logWarn, (.=))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
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
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( MonadHttp,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    jsonResponse,
    req,
    responseBody,
    useHttpURI,
  )
import Text.URI (mkURI)
import UnliftIO (Exception, Typeable, throwIO)

newtype CartException = CartException Text
  deriving (Show, Typeable)

instance Exception CartException

newtype CartId = CartId {unCartId :: Text}
  deriving (Eq, Read, Show, ToJSON)

class HasCartConfig env where
  getBookingUrl :: env -> Text
  getPaymentUrl :: env -> Text

newtype BookingId = BookingId {unBookingId :: Text}
  deriving (FromJSON, ToJSON)

newtype PaymentId = PaymentId {unPaymentId :: Text}
  deriving (FromJSON, ToJSON)

data BookingRequest = BookingRequest
  { bookingRequestShow :: Text,
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

processBooking ::
  (MonadReader env m, HasCartConfig env, MonadLogger m, MonadHttp m) =>
  CartId ->
  m BookingId
processBooking cartId = do
  bookingUrl <- asks getBookingUrl
  logInfo $ "Booking starting" :# ["cart_id" .= cartId, "booking_url" .= bookingUrl]
  liftIO $ threadDelay (2 * 1000 * 1000)
  when (cartId == CartId "ghi789") $ do
    logWarn $ "Booking failed" :# ["cart_id" .= cartId]
    throwIO $ CartException "Booking failed"
  uri <- liftIO $ mkURI bookingUrl
  let (url, options) = fromJust (useHttpURI uri)
      bookingRequest =
        BookingRequest
          { bookingRequestShow = "HRT3974",
            bookingRequestSeats = ["D31", "D32", "D33"]
          }
  response <- req POST url (ReqBodyJson bookingRequest) jsonResponse options
  let bookingResponse :: BookingResponse
      bookingResponse = responseBody response
      bookingId = bookingResponseBookingId bookingResponse
  logInfo $ "Booking successful" :# ["cart_id" .= cartId, "booking_id" .= bookingId]
  pure bookingId

processPayment ::
  (MonadReader env m, HasCartConfig env, MonadLogger m, MonadHttp m) =>
  CartId ->
  m PaymentId
processPayment cartId = do
  paymentUrl <- asks getPaymentUrl
  logInfo $ "Payment starting" :# ["cart_id" .= cartId, "payment_url" .= paymentUrl]
  liftIO $ threadDelay (1 * 1000 * 1000)
  when (cartId == CartId "ghi789") $ do
    logWarn $ "Payment failed" :# ["cart_id" .= cartId]
    throwIO $ CartException "Booking failed"
  uri <- liftIO $ mkURI paymentUrl
  let (url, options) = fromJust (useHttpURI uri)
      paymentRequest =
        PaymentRequest
          { paymentRequestCardholderName = "John Doe",
            paymentRequestCardNumber = "5192901463306478"
          }
  response <- req POST url (ReqBodyJson paymentRequest) jsonResponse options
  let paymentResponse :: PaymentResponse
      paymentResponse = responseBody response
      paymentId = paymentResponsePaymentId paymentResponse
  logInfo $ "Payment successful" :# ["cart_id" .= cartId, "payment_id" .= paymentId]
  pure paymentId
