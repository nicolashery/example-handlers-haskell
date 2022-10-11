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
import Data.Aeson (ToJSON)
import Data.Text (Text)
import UnliftIO (Exception, Typeable, throwIO)

newtype CartException = CartException Text
  deriving (Show, Typeable)

instance Exception CartException

newtype CartId = CartId {unCartId :: Text}
  deriving (Eq, Read, Show, ToJSON)

newtype BookingId = BookingId {unBookingId :: Text}
  deriving (ToJSON)

class HasCartConfig env where
  getBookingUrl :: env -> Text
  getPaymentUrl :: env -> Text

processBooking ::
  (MonadReader env m, HasCartConfig env, MonadIO m, MonadLogger m) =>
  CartId ->
  m BookingId
processBooking cartId = do
  bookingUrl <- asks getBookingUrl
  logInfo $ "Booking starting" :# ["cart_id" .= cartId, "booking_url" .= bookingUrl]
  liftIO $ threadDelay (2 * 1000 * 1000)
  when (cartId == CartId "ghi789") $ do
    logWarn $ "Booking failed" :# ["cart_id" .= cartId]
    throwIO $ CartException "Booking failed"
  let bookingId = BookingId "TKCY693D5ACB"
  logInfo $ "Booking successful" :# ["cart_id" .= cartId, "booking_id" .= bookingId]
  pure bookingId

newtype PaymentId = PaymentId {unPaymentId :: Text}
  deriving (ToJSON)

processPayment ::
  (MonadReader env m, HasCartConfig env, MonadIO m, MonadLogger m) =>
  CartId ->
  m PaymentId
processPayment cartId = do
  paymentUrl <- asks getBookingUrl
  logInfo $ "Payment starting" :# ["cart_id" .= cartId, "payment_url" .= paymentUrl]
  liftIO $ threadDelay (1 * 1000 * 1000)
  when (cartId == CartId "ghi789") $ do
    logWarn $ "Payment failed" :# ["cart_id" .= cartId]
    throwIO $ CartException "Booking failed"
  let paymentId = PaymentId "zTNBbSdy3vdOSnRT3xzFHviB"
  logInfo $ "Payment successful" :# ["cart_id" .= cartId, "payment_id" .= paymentId]
  pure paymentId
