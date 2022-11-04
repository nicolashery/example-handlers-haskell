module App.External
  ( main,
    -- Suppress 'defined but not used warnings'
    BookingId (unBookingId),
    PaymentId (unPaymentId),
  )
where

import App.Json (defaultParseJSON, defaultToJSON)
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Handler,
    JSON,
    Post,
    Proxy (Proxy),
    ReqBody,
    Server,
    ServerError (errBody),
    err400,
    serve,
    (:>),
    type (:<|>) ((:<|>)),
  )

newtype BookingId = BookingId {unBookingId :: Text}
  deriving (ToJSON)

newtype PaymentId = PaymentId {unPaymentId :: Text}
  deriving (ToJSON)

data BookingRequest = BookingRequest
  { bookingRequestVenue :: Text,
    bookingRequestSeats :: [Text]
  }
  deriving (Generic)

instance FromJSON BookingRequest where
  parseJSON = defaultParseJSON "bookingRequest"

data BookingResponse = BookingResponse
  { bookingResponseBookingId :: BookingId,
    bookingResponseSeatsReserved :: Int
  }
  deriving (Generic)

instance ToJSON BookingResponse where
  toJSON = defaultToJSON "bookingResponse"

data PaymentRequest = PaymentRequest
  { paymentRequestCardholderName :: Text,
    paymentRequestCardNumber :: Text
  }
  deriving (Generic)

instance FromJSON PaymentRequest where
  parseJSON = defaultParseJSON "paymentRequest"

data PaymentResponse = PaymentResponse
  { paymentResponsePaymentId :: PaymentId,
    paymentResponseAmountCharged :: Int
  }
  deriving (Generic)

instance ToJSON PaymentResponse where
  toJSON = defaultToJSON "paymentResponse"

type Api =
  "booking" :> ReqBody '[JSON] BookingRequest :> Post '[JSON] BookingResponse
    :<|> "payment" :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] PaymentResponse

postBookingHandler :: BookingRequest -> Handler BookingResponse
postBookingHandler BookingRequest {bookingRequestVenue = venue} = do
  when (venue == "TDE8751") $ do
    throwError $ err400 {errBody = "Invalid venue"}
  pure $
    BookingResponse
      { bookingResponseBookingId = BookingId "TKCY693D5ACB",
        bookingResponseSeatsReserved = 3
      }

postPaymentHandler :: PaymentRequest -> Handler PaymentResponse
postPaymentHandler PaymentRequest {paymentRequestCardNumber = cardNumber} = do
  when (cardNumber == "a192901463306478") $ do
    throwError $ err400 {errBody = "Invalid card number"}
  pure $
    PaymentResponse
      { paymentResponsePaymentId = PaymentId "zTNBbSdy3vdOSnRT3xzFHviB",
        paymentResponseAmountCharged = 5200
      }

server :: Server Api
server =
  postBookingHandler
    :<|> postPaymentHandler

api :: Proxy Api
api = Proxy

main :: IO ()
main = do
  let port = 3001
      waiApp = serve api server
  run port waiApp
