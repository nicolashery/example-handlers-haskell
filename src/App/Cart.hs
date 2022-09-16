module App.Cart
  ( CartId (CartId, unCartId),
    BookingId (BookingId, unBookingId),
    processBooking,
    PaymentId (PaymentId, unPaymentId),
    processPayment,
  )
where

import Blammo.Logging (Message ((:#)), MonadLogger, logInfo, logWarn, (.=))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON)
import Data.Text (Text)
import UnliftIO (throwString)

newtype CartId = CartId {unCartId :: Text}
  deriving (Eq, Read, Show, ToJSON)

newtype BookingId = BookingId {unBookingId :: Text}
  deriving (ToJSON)

processBooking :: (MonadIO m, MonadLogger m) => CartId -> m BookingId
processBooking cartId = do
  logInfo $ "Booking starting" :# ["cart_id" .= cartId]
  liftIO $ threadDelay (2 * 1000 * 1000)
  when (cartId == CartId "ghi789") $ do
    logWarn $ "Booking failed" :# ["cart_id" .= cartId]
    throwString "Booking failed"
  let bookingId = BookingId "TKCY693D5ACB"
  logInfo $ "Booking successful" :# ["cart_id" .= cartId, "booking_id" .= bookingId]
  pure bookingId

newtype PaymentId = PaymentId {unPaymentId :: Text}
  deriving (ToJSON)

processPayment :: (MonadIO m, MonadLogger m) => CartId -> m PaymentId
processPayment cartId = do
  logInfo $ "Payment starting" :# ["cart_id" .= cartId]
  liftIO $ threadDelay (1 * 1000 * 1000)
  when (cartId == CartId "ghi789") $ do
    logWarn $ "Booking failed" :# ["cart_id" .= cartId]
    throwString "Booking failed"
  let paymentId = PaymentId "zTNBbSdy3vdOSnRT3xzFHviB"
  logInfo $ "Payment successful" :# ["cart_id" .= cartId, "payment_id" .= paymentId]
  pure paymentId
