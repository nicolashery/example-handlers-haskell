module App.Cart
  ( BookingId (BookingId, unBookingId),
    BookingPayload,
    newBookingPayload,
    processBooking,
    PaymentId (PaymentId, unPaymentId),
    PaymentPayload,
    newPaymentPayload,
    processPayment,
  )
where

import Blammo.Logging (Message ((:#)), MonadLogger, logInfo, (.=))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)

newtype BookingId = BookingId {unBookingId :: Text}

data BookingPayload = BookingPayload

newBookingPayload :: BookingPayload
newBookingPayload = BookingPayload

processBooking :: (MonadIO m, MonadLogger m) => BookingPayload -> m BookingId
processBooking _ = do
  logInfo "Booking starting"
  liftIO $ threadDelay (2 * 1000 * 1000)
  let bookingId = BookingId "TKCY693D5ACB"
  logInfo $ "Booking successful" :# ["booking_id" .= unBookingId bookingId]
  pure bookingId

newtype PaymentId = PaymentId {unPaymentId :: Text}

data PaymentPayload = PaymentPayload

newPaymentPayload :: PaymentPayload
newPaymentPayload = PaymentPayload

processPayment :: (MonadIO m, MonadLogger m) => PaymentPayload -> m PaymentId
processPayment _ = do
  logInfo "Payment starting"
  liftIO $ threadDelay (1 * 1000 * 1000)
  let paymentId = PaymentId "zTNBbSdy3vdOSnRT3xzFHviB"
  logInfo $ "Payment successful" :# ["payment_id" .= unPaymentId paymentId]
  pure paymentId
