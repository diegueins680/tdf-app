{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Services.EventLogisticsRoutes (
    RouteEstimateInput (..),
    RouteEstimateResult (..),
    computeGoogleRoute,
    parseGoogleRouteResponse,
    parseGoogleDurationSeconds,
) where

import Control.Exception (try)
import Data.Aeson (Value, eitherDecode, encode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Network.HTTP.Client
    ( HttpException
    , Request (..)
    , RequestBody (RequestBodyLBS)
    , Response
    , httpLbs
    , parseRequest
    , responseBody
    , responseStatus
    )
import Network.HTTP.Types.Status (statusCode)
import Text.Read (readMaybe)

import TDF.DB (sharedTlsManager)

data RouteEstimateInput = RouteEstimateInput
    { reiOriginLatitude :: Double
    , reiOriginLongitude :: Double
    , reiDestinationLatitude :: Double
    , reiDestinationLongitude :: Double
    , reiTravelMode :: Text
    , reiDepartureTime :: UTCTime
    }
    deriving (Show, Eq)

data RouteEstimateResult = RouteEstimateResult
    { rerDurationSeconds :: Int
    , rerStaticDurationSeconds :: Maybe Int
    , rerDistanceMeters :: Int
    , rerEncodedPolyline :: Maybe Text
    }
    deriving (Show, Eq)

computeGoogleRoute :: Text -> Text -> Text -> RouteEstimateInput -> IO (Either Text RouteEstimateResult)
computeGoogleRoute apiKey apiBase locale input = do
    requestResult <- try (parseRequest endpoint) :: IO (Either HttpException Request)
    case requestResult of
        Left _ -> pure (Left "No se pudo preparar la solicitud de Google Routes.")
        Right baseRequest -> do
            let request =
                    baseRequest
                        { method = "POST"
                        , requestHeaders =
                            [ ("Content-Type", "application/json")
                            , ("X-Goog-Api-Key", TE.encodeUtf8 apiKey)
                            , ( "X-Goog-FieldMask"
                              , "routes.duration,routes.staticDuration,routes.distanceMeters,routes.polyline.encodedPolyline"
                              )
                            ]
                        , requestBody = RequestBodyLBS (encode (requestPayload locale input))
                        }
            responseResult <- try (httpLbs request sharedTlsManager) :: IO (Either HttpException (Response BL.ByteString))
            pure $ case responseResult of
                Left _ -> Left "No se pudo contactar Google Routes. Intenta verificar nuevamente."
                Right response
                    | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 ->
                        Left ("Google Routes rechazó la solicitud (HTTP " <> T.pack (show (statusCode (responseStatus response))) <> ").")
                    | otherwise -> parseGoogleRouteResponse (responseBody response)
  where
    endpoint = T.unpack (T.dropWhileEnd (== '/') (T.strip apiBase) <> "/directions/v2:computeRoutes")

requestPayload :: Text -> RouteEstimateInput -> Value
requestPayload locale RouteEstimateInput{..} =
    object
        ( [ "origin" .= waypoint reiOriginLatitude reiOriginLongitude
          , "destination" .= waypoint reiDestinationLatitude reiDestinationLongitude
          , "travelMode" .= googleTravelMode reiTravelMode
          , "departureTime" .= reiDepartureTime
          , "computeAlternativeRoutes" .= False
          , "languageCode" .= locale
          , "units" .= ("METRIC" :: Text)
          ]
            <> ["routingPreference" .= ("TRAFFIC_AWARE" :: Text) | T.toCaseFold reiTravelMode == "drive"]
        )

waypoint :: Double -> Double -> Value
waypoint latitude longitude =
    object
        [ "location"
            .= object
                [ "latLng"
                    .= object
                        [ "latitude" .= latitude
                        , "longitude" .= longitude
                        ]
                ]
        ]

googleTravelMode :: Text -> Text
googleTravelMode rawMode =
    case T.toCaseFold (T.strip rawMode) of
        "walk" -> "WALK"
        "bicycle" -> "BICYCLE"
        "two_wheeler" -> "TWO_WHEELER"
        "transit" -> "TRANSIT"
        _ -> "DRIVE"

parseGoogleRouteResponse :: BL.ByteString -> Either Text RouteEstimateResult
parseGoogleRouteResponse body = do
    value <- either (Left . const "Google Routes devolvió una respuesta inválida.") Right (eitherDecode body :: Either String Value)
    either (Left . T.pack) Right (parseEither routeParser value)
  where
    routeParser = withObject "GoogleRoutesResponse" $ \root -> do
        routes <- root .: "routes"
        route <- maybe (fail "Google Routes no encontró una ruta entre los lugares.") pure (listToMaybe routes)
        withObject "GoogleRoute" parseRoute route
    parseRoute route = do
        durationText <- route .: "duration"
        duration <- maybe (fail "Google Routes devolvió una duración inválida.") pure (parseGoogleDurationSeconds durationText)
        staticDurationText <- route .:? "staticDuration"
        distance <- route .: "distanceMeters"
        polyline <- route .:? "polyline"
        encodedPolyline <- traverse (withObject "GooglePolyline" (.: "encodedPolyline")) polyline
        pure
            RouteEstimateResult
                { rerDurationSeconds = duration
                , rerStaticDurationSeconds = staticDurationText >>= parseGoogleDurationSeconds
                , rerDistanceMeters = distance
                , rerEncodedPolyline = encodedPolyline
                }

parseGoogleDurationSeconds :: Text -> Maybe Int
parseGoogleDurationSeconds raw = do
    seconds <- T.stripSuffix "s" (T.strip raw) >>= readMaybe . T.unpack
    if (seconds :: Double) < 0 then Nothing else Just (ceiling seconds)
