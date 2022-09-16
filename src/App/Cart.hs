module App.Cart
  ( CartId (CartId, unCartId),
    BookingId (BookingId, unBookingId),
    processBooking,
    PaymentId (PaymentId, unPaymentId),
    processPayment,
  )
where

import Blammo.Logging (Message ((:#)), MonadLogger, logInfo, (.=))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON)
import Data.Text (Text)

newtype CartId = CartId {unCartId :: Text}
  deriving (Eq, Read, Show, ToJSON)

newtype BookingId = BookingId {unBookingId :: Text}

processBooking :: (MonadIO m, MonadLogger m) => CartId -> m BookingId
processBooking _ = do
  logInfo "Booking starting"
  liftIO $ threadDelay (2 * 1000 * 1000)
  let bookingId = BookingId "TKCY693D5ACB"
  logInfo $ "Booking successful" :# ["booking_id" .= unBookingId bookingId]
  pure bookingId

newtype PaymentId = PaymentId {unPaymentId :: Text}

processPayment :: (MonadIO m, MonadLogger m) => CartId -> m PaymentId
processPayment _ = do
  logInfo "Payment starting"
  liftIO $ threadDelay (1 * 1000 * 1000)
  let paymentId = PaymentId "zTNBbSdy3vdOSnRT3xzFHviB"
  logInfo $ "Payment successful" :# ["payment_id" .= unPaymentId paymentId]
  pure paymentId
