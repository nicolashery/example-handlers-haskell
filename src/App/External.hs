module App.External
  ( main,
    -- Suppress 'defined but not used warnings'
    PaymentId (unPaymentId),
    BookingId (unBookingId),
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
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
    serve,
    (:>),
    type (:<|>) ((:<|>)),
  )

newtype PaymentId = PaymentId {unPaymentId :: Text}
  deriving (ToJSON)

data PaymentRequest = PaymentRequest
  { paymentRequestCardholderName :: Text,
    paymentRequestCardNumber :: Text
  }
  deriving (Generic)

instance FromJSON PaymentRequest where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 14}

data PaymentResponse = PaymentResponse
  { paymentResponsePaymentId :: PaymentId,
    paymentResponseAmountCharged :: Int
  }
  deriving (Generic)

instance ToJSON PaymentResponse where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 15}

newtype BookingId = BookingId {unBookingId :: Text}
  deriving (ToJSON)

data BookingRequest = BookingRequest
  { bookingRequestShow :: Text,
    bookingRequestSeats :: [Text]
  }
  deriving (Generic)

instance FromJSON BookingRequest where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 14}

data BookingResponse = BookingResponse
  { bookingResponseBookingId :: BookingId,
    bookingResponseSeatsReserved :: Int
  }
  deriving (Generic)

instance ToJSON BookingResponse where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 15}

type Api =
  "payment" :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] PaymentResponse
    :<|> "booking" :> ReqBody '[JSON] BookingRequest :> Post '[JSON] BookingResponse

postPaymentHandler :: PaymentRequest -> Handler PaymentResponse
postPaymentHandler _ =
  pure $
    PaymentResponse
      { paymentResponsePaymentId = PaymentId "zTNBbSdy3vdOSnRT3xzFHviB",
        paymentResponseAmountCharged = 5200
      }

postBookingHandler :: BookingRequest -> Handler BookingResponse
postBookingHandler _ =
  pure $
    BookingResponse
      { bookingResponseBookingId = BookingId "TKCY693D5ACB",
        bookingResponseSeatsReserved = 3
      }

server :: Server Api
server =
  postPaymentHandler
    :<|> postBookingHandler

api :: Proxy Api
api = Proxy

main :: IO ()
main = do
  let port = 3001
      waiApp = serve api server
  run port waiApp
