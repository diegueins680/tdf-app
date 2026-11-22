{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.Venues.SearchSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (isJust)
import Data.Pool (destroyAllResources)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.Persist (Entity (..), PersistValue (..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql
  ( ConnectionPool, Single (..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool
  , toSqlKey, transactionUndo )
import Servant (ServerError, (:<|>) (..))
import Servant.Server.Internal.Handler (runHandler)
import System.Environment (lookupEnv)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck

import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.Config (loadConfig)
import TDF.DB (Env (..))
import TDF.DTO.SocialEventsDTO (VenueDTO (venueId, venueName))
import TDF.Models (RoleEnum (Fan))
import qualified TDF.Models.SocialEventsModels as SM
import TDF.Server.SocialEventsHandlers (socialEventsServer)
import TDF.Venues.Search
import TDF.Venues.SearchFold

-- EVT-VENUE-SEARCH-001. 'venueSearchSpec' is the specification. The pure part
-- checks it, the bounding box and the generated fold; the PostgreSQL part runs
-- the statement 'loadVenueSearch' really sends and compares it with the
-- specification on generated venue tables.
spec :: Spec
spec = do
  describe "venue-search-specification" $ do
    prop "returns rows of the input, in order, that all satisfy the filters" $
      forAll genCase $ \(rows, query) ->
        let result = venueSearchSpec storedFields query rows
            kept = venueSearchSpec storedFields query{vsqLimit = length rows, vsqOffset = 0} rows
         in conjoin
              [ counterexample "page"
                  (map storedId result === map storedId (take (vsqLimit query) (drop (vsqOffset query) kept)))
              , counterexample "order"
                  (map storedId kept === filter (`elem` map storedId kept) (map storedId rows))
              , counterexample "city" (property (all (inCity query . storedFields) kept))
              ]
    prop "without filters is plain paging" $
      forAll genCase $ \(rows, query) ->
        let unfiltered = query{vsqCity = Nothing, vsqNeedle = Nothing, vsqNear = Nothing}
         in map storedId (venueSearchSpec storedFields unfiltered rows)
              === map storedId (take (vsqLimit query) (drop (vsqOffset query) rows))
    it "folds case on both sides of the text match" $ do
      let venue = VenueSearchFields "Stra\223e 12" (Just "QUITO") Nothing Nothing Nothing
      matchesVenueText (T.toCaseFold "STRASSE") venue `shouldBe` True
      matchesVenueText (T.toCaseFold "quito") venue `shouldBe` True
      matchesVenueText "QUITO" venue `shouldBe` False
      matchesVenueText "x" venue{vsfCity = Nothing} `shouldBe` False
    it "never matches a venue without both coordinates" $ do
      let venue lat lng = VenueSearchFields "v" Nothing Nothing lat lng
      matchesVenueNear (0, 0, 1000) (venue (Just 0) Nothing) `shouldBe` False
      matchesVenueNear (0, 0, 1000) (venue Nothing (Just 0)) `shouldBe` False
      matchesVenueNear (0, 0, 1000) (venue (Just 0) (Just 0)) `shouldBe` True
    it "a row cap loses matches that the uncapped specification keeps" $ do
      let rows = [Stored n (VenueSearchFields "sala" Nothing Nothing Nothing Nothing) | n <- [1 .. 2001]]
          query = VenueSearchQuery Nothing (Just "sala") Nothing 500 1999
      map storedId (venueSearchSpec storedFields query rows) `shouldBe` [2000, 2001]
      map storedId (venueSearchSpec storedFields query (take 2000 rows)) `shouldBe` [2000]

  describe "venue-search-bounding-box" $ do
    modifyMaxSuccess (const 20000) $
      prop "contains every in-range venue within the radius plus the tolerance" $
        forAll genNear $ \near@(lat, lng, radiusKm) ->
          forAll (genInRangeCoordinates near) $ \(venueLat, venueLng) ->
            let distance = haversineDistanceKm lat lng venueLat venueLng
                inside = inVenueBoundingBox (venueBoundingBox lat lng radiusKm) venueLat venueLng
             in classify (distance <= radiusKm) "within the radius" $
                  counterexample (show distance) (not (distance <= radiusKm + venueSearchToleranceKm) || inside)
    it "keeps venues across the 180 degree meridian" $ do
      boxKeeps (0, 179.95, 25) (0, -179.95)
      boxKeeps (0, -179.95, 25) (0, 179.95)
      boxKeeps (60, 180, 1000) (60, -170)
      boxKeeps (60, -180, 1000) (60, 170)
    it "keeps venues across and at the poles" $ do
      boxKeeps (89.5, 0, 100) (89.9, 180)
      boxKeeps (-89.5, 30, 100) (-89.9, -150)
      boxKeeps (90, 0, 1) (89.995, 137)
      boxKeeps (-90, 0, 1) (-89.995, -42)
      boxKeeps (85, 10, 1000) (90, -95)
    it "keeps all longitudes once the circle is near or over a pole" $ do
      let whole box = (bbLngMin box, bbLngMax box) == (-180, 180)
      venueBoundingBox 81.1 10 1000 `shouldSatisfy` whole
      venueBoundingBox (-90) 10 0.001 `shouldSatisfy` whole
      venueBoundingBox 80 10 1000 `shouldSatisfy` (not . whole)
    it "bounds the longitude tightly away from the poles" $ do
      let box = venueBoundingBox 0 0 111.19492664455873
      bbLngMax box `shouldSatisfy` (\x -> x > 1 && x < 1.0001)
      bbLatMax box `shouldSatisfy` (\x -> x > 1 && x < 1.0001)
    it "is total for a radius larger than the planet" $
      venueBoundingBox 10 20 50000 `shouldSatisfy` (\box -> bbLngMin box == -180 && bbLatMin box < -90)

  describe "venue-search-statement" $ do
    prop "sends as many parameters as the statement has placeholders" $
      forAll genCase $ \(_, query) ->
        let (sql, params) = venueSearchStatement query
         in T.count "?" (T.replace "??" "" sql) === length params
    it "escapes the LIKE wildcards and the escape character" $
      escapeLikeNeedle "50%_off!" `shouldBe` "50!%!_off!!"
    it "reads the flag environment as the other runtime flags do" $ do
      venueSqlSearchEnvironment (Just " Production ") `shouldBe` "production"
      venueSqlSearchEnvironment (Just "prod") `shouldBe` "production"
      venueSqlSearchEnvironment (Just "staging") `shouldBe` "sandbox"
      venueSqlSearchEnvironment Nothing `shouldBe` "sandbox"

  describe "venue-search-fold" $ do
    it "lists only characters whose folded form folding leaves alone" $ do
      length venueSearchFoldEntries `shouldSatisfy` (> 1000)
      [c | (c, folded) <- venueSearchFoldEntries, T.toCaseFold folded /= folded] `shouldBe` []
      let sources = Set.fromList (map fst venueSearchFoldEntries)
      [c | (c, folded) <- venueSearchFoldEntries, T.any (`Set.member` sources) folded] `shouldBe` []
    it "is the function the migration installs" $ do
      migration <- TIO.readFile "sql/2026-11-21_venue_search_sql.sql"
      T.isInfixOf venueSearchFoldFunctionSql migration `shouldBe` True
      T.all (< '\x80') migration `shouldBe` True

  describe "venue-search-postgresql" $ do
    configured <- runIO (lookupEnv "TDF_VENUE_SEARCH_DATABASE_URL")
    routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
    case configured of
      Nothing -> it "requires the isolated migrated integration runner" $
        pendingWith "Run scripts/test-venue-search.sh"
      Just _ | any (maybe False (not . null)) routingOverrides ->
        it "refuses inherited libpq routing overrides" $
          expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
      Just connection | not (safeConnection connection) ->
        it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
      Just connection -> beforeAll (open connection) $ afterAll destroyAllResources $ do
        it "folds every Unicode scalar value as Data.Text.toCaseFold does" $ \pool -> do
          let scalars = ['\x1' .. '\xD7FF'] ++ ['\xE000' .. '\x10FFFF']
              joined = T.intercalate "\n" . map T.singleton
              inputs =
                [joined ['\x1' .. '\x7F'], joined ['\x1' .. '\x24F']]
                  ++ concat [[body, body <> "\n\233", body <> "\n\20013"] | body <- map joined (chunksOf 4096 scalars)]
          length scalars `shouldBe` 1112063
          forM_ inputs $ \input -> do
            folded <- runSqlPool (rawSql "SELECT venue_search_fold(?)" [PersistText input]) pool
            case folded of
              [Single actual] | actual == T.toCaseFold input -> pure ()
              [Single actual] ->
                expectationFailure
                  ("fold differs: " <> show (take 5 (differing (T.toCaseFold input) actual)))
              other -> expectationFailure ("unexpected fold answer: " <> show (length (other :: [Single Text])))

        modifyMaxSuccess (const 1500) $
          it "answers as the specification does, outside the stated tolerance" $ \pool ->
            forAll genCase $ \(rows, query) -> ioProperty $ do
              (ordered, full, page) <- runSqlPool (do
                seed rows
                ordered <- allVenues
                full <- loadVenueSearch query{vsqLimit = length rows + 1, vsqOffset = 0}
                page <- loadVenueSearch query
                pure (ordered, full, page)) pool
              let fullIds = map entityId full
                  members = Set.fromList fullIds
                  verdicts = [(entityId row, expectation query (entityFields row)) | row <- ordered]
                  wrong =
                    [ (ident, verdict)
                    | (ident, verdict) <- verdicts
                    , case verdict of
                        Must present -> present /= Set.member ident members
                        Either'      -> False
                    ]
                  exact = all ((/= Either') . snd) verdicts
              pure $
                classify exact "no venue inside the tolerance band" $
                classify (isJust (vsqNear query) && not (null full)) "distance query with matches" $
                classify (maybe False (not . T.null) (vsqNeedle query) && not (null full)) "text query with matches" $
                conjoin
                  [ counterexample ("membership " <> show wrong) (null wrong)
                  , counterexample "order" (fullIds === filter (`Set.member` members) (map entityId ordered))
                  , counterexample "page"
                      (map entityId page === take (vsqLimit query) (drop (vsqOffset query) fullIds))
                  , counterexample "exact"
                      (not exact .||. map entityId page
                        === map entityId (venueSearchSpec entityFields query (filter (admissible query . entityFields) ordered)))
                  ]

        it "equals the specification on 2,000 venues and finds the venues a cap would drop" $ \pool -> do
          let needle = T.toCaseFold "\209AND\218"
              queries =
                [ VenueSearchQuery Nothing (Just needle) Nothing 500 0
                , VenueSearchQuery Nothing (Just needle) Nothing 500 250
                , VenueSearchQuery (Just "Otra") Nothing (Just (-0.1, -78.5, 5)) 500 0
                , VenueSearchQuery Nothing (Just needle) (Just (-0.05, -78.5, 12.5)) 200 3
                , VenueSearchQuery Nothing Nothing Nothing 200 1900
                ]
          forM_ [2000, 2500 :: Int] $ \size -> do
            (ordered, answers) <- runSqlPool (do
              seedSeries size
              ordered <- allVenues
              answers <- mapM loadVenueSearch queries
              pure (ordered, answers)) pool
            length ordered `shouldBe` size
            forM_ (zip queries answers) $ \(query, answer) -> do
              [() | row <- ordered, Either' <- [expectation query (entityFields row)]] `shouldBe` []
              map entityId answer `shouldBe` map entityId (venueSearchSpec entityFields query ordered)
          ordered <- runSqlPool allVenues pool
          let beyond = VenueSearchQuery Nothing (Just needle) Nothing 500 300
          found <- runSqlPool (loadVenueSearch beyond) pool
          map entityId found `shouldSatisfy` (not . null)
          venueSearchSpec entityFields beyond (take 2000 ordered) `shouldSatisfy` null

        it "never raises on coordinates PostgreSQL arithmetic would reject" $ \pool -> do
          let extremes = [0, 5.0e-324, -1.0e-200, 1.0e-160, 90, -180, 1.0e10, 1.0e300, 1.0e301, 1.7e308, 0 / 0, 1 / 0, -1 / 0]
              rows =
                [ Stored n (VenueSearchFields "v" Nothing Nothing (Just lat) (Just lng))
                | (n, (lat, lng)) <- zip [1 ..] [(lat, lng) | lat <- extremes, lng <- extremes]
                ]
          forM_ [(0, 0, 1.0e-300), (5.0e-324, -1.0e-200, 1), (90, 180, 1000), (-1.0e-160, 0, 25)] $ \near -> do
            answer <- runSqlPool (seed rows >> loadVenueSearch (VenueSearchQuery Nothing Nothing (Just near) 500 0)) pool
            length answer `shouldSatisfy` (<= length rows)

        it "is off until the flag row is enabled for the environment, and off without the function" $ \pool -> do
          observed <- runSqlPool (do
            initial <- mapM venueSqlSearchEnabled ["sandbox", "production"]
            rawExecute "UPDATE revenue_feature_flag SET enabled = TRUE WHERE flag_key = ? AND environment = 'sandbox'"
              [PersistText venueSqlSearchFlag]
            enabled <- mapM venueSqlSearchEnabled ["sandbox", "production"]
            rawExecute "ALTER FUNCTION venue_search_fold(text) RENAME TO venue_search_fold_absent" []
            withoutFunction <- venueSqlSearchEnabled "sandbox"
            transactionUndo
            restored <- venueSqlSearchEnabled "sandbox"
            pure (initial, enabled, withoutFunction, restored)) pool
          observed `shouldBe` ([False, False], [True, False], False, False)

        it "keeps the capped in-memory path while the flag is off and lifts the cap when it is on" $ \pool -> do
          runSqlPool (seedSeries 2500) pool
          ordered <- runSqlPool allVenues pool
          let needle = "\209and\218"
              list = listVenues pool Nothing Nothing (Just needle) (Just 500) (Just 0)
              legacy = VenueSearchQuery Nothing (Just (T.toCaseFold needle)) Nothing 500 0
          off <- list
          map venueId off `shouldBe` map (Just . T.pack . show . entityId)
            (venueSearchSpec entityFields legacy (take 2000 ordered))
          plain <- listVenues pool Nothing Nothing Nothing (Just 3) (Just 2100)
          map venueName plain `shouldBe` map (vsfName . entityFields) (take 3 (drop 2100 ordered))
          runSqlPool (rawExecute "UPDATE revenue_feature_flag SET enabled = TRUE WHERE flag_key = ?"
            [PersistText venueSqlSearchFlag]) pool
          on <- list
          map venueId on `shouldBe` map (Just . T.pack . show . entityId)
            (venueSearchSpec entityFields legacy ordered)
          length on `shouldSatisfy` (> length off)
          runSqlPool (rawExecute "UPDATE revenue_feature_flag SET enabled = FALSE WHERE flag_key = ?"
            [PersistText venueSqlSearchFlag]) pool
          offAgain <- list
          map venueId offAgain `shouldBe` map venueId off

-- | What the SQL answer must say about one venue.
data Verdict = Must Bool | Either' deriving (Eq, Show)

-- | The specification decides, except where the contract leaves the answer
-- open: a distance within the tolerance of the radius, and an out-of-range
-- coordinate whose haversine term sits on the edge of its domain. A coordinate
-- above the ceiling or not finite never matches.
expectation :: VenueSearchQuery -> VenueSearchFields -> Verdict
expectation query venue
  | not (inCity query venue && hasText) = Must False
  | otherwise = case (vsqNear query, vsfLatitude venue, vsfLongitude venue) of
      (Nothing, _, _) -> Must True
      (Just (lat, lng, radiusKm), Just venueLat, Just venueLng)
        | absurd venue -> Must False
        | abs (distance - radiusKm) <= venueSearchToleranceKm -> Either'
        | outOfRange && (isNaN distance || distance <= venueSearchToleranceKm) -> Either'
        | otherwise -> Must (distance <= radiusKm)
        where
          distance = haversineDistanceKm lat lng venueLat venueLng
          outOfRange = abs venueLat > 90 || abs venueLng > 180
      _ -> Must False
  where
    hasText = case vsqNeedle query of
      Just needle | not (T.null needle) -> matchesVenueText needle venue
      _                                -> True

inCity :: VenueSearchQuery -> VenueSearchFields -> Bool
inCity query venue = maybe True (\city -> vsfCity venue == Just city) (vsqCity query)

-- | Outside a distance query every venue is admissible; inside one, a venue
-- with an absurd coordinate is not.
admissible :: VenueSearchQuery -> VenueSearchFields -> Bool
admissible query venue = not (isJust (vsqNear query) && absurd venue)

absurd :: VenueSearchFields -> Bool
absurd venue = any (maybe False beyond) [vsfLatitude venue, vsfLongitude venue]
  where
    beyond value = not (abs value <= venueSearchCoordinateCeiling)

data Stored = Stored { storedId :: Int64, storedFields :: VenueSearchFields } deriving (Eq, Show)

genCase :: Gen ([Stored], VenueSearchQuery)
genCase = do
  near <- genNear
  size <- frequency [(1, pure 0), (8, choose (1, 30))]
  fields <- vectorOf size (genFields near)
  idents <- shuffle [1 .. fromIntegral size]
  needle <- frequency
    [ (3, pure Nothing)
    , (4, if null fields then pure Nothing else Just <$> (elements fields >>= genInfix))
    , (2, Just . T.toCaseFold <$> genText)
    , (1, Just <$> genText)
    ]
  city <- frequency [(3, pure Nothing), (1, Just <$> elements cities)]
  useNear <- frequency [(2, pure False), (3, pure True)]
  limit <- frequency [(3, choose (1, 8)), (1, pure 500)]
  offset <- frequency [(2, pure 0), (2, choose (0, 6))]
  pure
    ( zipWith Stored idents fields
    , VenueSearchQuery city needle (if useNear then Just near else Nothing) limit offset
    )

genNear :: Gen (Double, Double, Double)
genNear = do
  lat <- frequency
    [ (6, choose (-90, 90))
    , (3, elements [0, 90, -90, 89.9999, -89.9999, 81.02, 45, -0.2, 1.0e-200, 5.0e-324, -1.0e-160])
    ]
  lng <- frequency
    [ (6, choose (-180, 180))
    , (3, elements [0, 180, -180, 179.9999, -179.9999, -78.5, 1.0e-200, 5.0e-324])
    ]
  radiusKm <- frequency
    [ (5, choose (0.001, 1000))
    , (3, elements [1000, 999.999, 25, 1, 1.0e-3, 1.0e-9, 1.0e-300])
    ]
  pure (lat, lng, radiusKm)

-- | In-range coordinates, most of them on or beside the edge of the circle.
genInRangeCoordinates :: (Double, Double, Double) -> Gen (Double, Double)
genInRangeCoordinates near@(_, _, radiusKm) = frequency
  [ (8, do
      bearing <- choose (0, 2 * pi)
      factor <- elements [1, 1, 1, 0.999999, 1.000001, 0.5, 0.01]
      nudge <- elements [0, 0, 1, -1, 2, -2, 10, -10, 1000, -1000]
      pure (destination near bearing (max 0 (radiusKm * factor + nudge * 2 * venueSearchToleranceKm))))
  , (3, (,) <$> choose (-90, 90) <*> choose (-180, 180))
  , (1, (,) <$> elements [0, 90, -90, 5.0e-324, 1.0e-200] <*> elements [0, 180, -180, -1.0e-160])
  ]

-- | The point at a distance along a bearing, with the longitude in -180..180.
destination :: (Double, Double, Double) -> Double -> Double -> (Double, Double)
destination (lat, lng, _) bearing distanceKm =
  let rad x = x * pi / 180
      deg x = x * 180 / pi
      angular = distanceKm / earthRadiusKm
      lat2 = asin (max (-1) (min 1 (sin (rad lat) * cos angular + cos (rad lat) * sin angular * cos bearing)))
      lng2 = rad lng + atan2 (sin bearing * sin angular * cos (rad lat)) (cos angular - sin (rad lat) * sin lat2)
      wrapped = deg lng2 - 360 * fromIntegral (floor ((deg lng2 + 180) / 360) :: Integer)
   in (max (-90) (min 90 (deg lat2)), max (-180) (min 180 wrapped))

genCoordinates :: (Double, Double, Double) -> Gen (Maybe Double, Maybe Double)
genCoordinates near = frequency
  [ (12, (\(lat, lng) -> (Just lat, Just lng)) <$> genInRangeCoordinates near)
  , (1, pure (Nothing, Nothing))
  , (1, (,) <$> (Just <$> choose (-90, 90)) <*> pure Nothing)
  , (1, (,) Nothing <$> (Just <$> choose (-180, 180)))
  , (3, (,) <$> (Just <$> elements special) <*> (Just <$> elements special))
  ]
  where
    special =
      [ 0, 90, -90, 180, -180, 5.0e-324, 1.0e-200, -1.0e-160, 95, -200, 540, 1.0e10, -1.0e15
      , 1.0e300, 1.0e301, 1.7e308, 0 / 0, 1 / 0, -1 / 0 ]

genFields :: (Double, Double, Double) -> Gen VenueSearchFields
genFields near = do
  name <- frequency [(2, elements ["Teatro", "teatro", "TEATRO", "Sala %", "Stra\223e"]), (5, genText)]
  city <- frequency [(1, pure Nothing), (2, Just <$> elements cities), (1, Just <$> genText)]
  address <- frequency [(1, pure Nothing), (2, Just <$> genText)]
  (lat, lng) <- genCoordinates near
  pure (VenueSearchFields name city address lat lng)

cities :: [Text]
cities = ["Quito", "quito", "QUITO", "Guayaquil", "Cuenca \201"]

-- | Text over the characters where case folding, LIKE and SQL quoting differ.
genText :: Gen Text
genText = do
  size <- choose (0, 12)
  T.pack <$> vectorOf size (frequency [(8, elements alphabet), (1, arbitraryUnicodeChar `suchThat` (/= '\0'))])
  where
    alphabet =
      "aAbBcCzZ 09%_!\\'\"-?" ++ "\233\201\241\209\252\220\223\7838\304\305\383\962\963\931\181\956"
        ++ "\64257\64256\453\454\329\496\912\837\8490\8491\197\769\775"
        ++ "\20013\25991\128512\5024\43888\66560\66600"

-- | A piece of one of a venue's folded columns, so that text queries match.
genInfix :: VenueSearchFields -> Gen Text
genInfix venue = do
  source <- elements (vsfName venue : maybe [] pure (vsfCity venue) ++ maybe [] pure (vsfAddress venue))
  let folded = T.toCaseFold source
  start <- choose (0, max 0 (T.length folded - 1))
  size <- choose (1, 4)
  pure (T.take size (T.drop start folded))

boxKeeps :: (Double, Double, Double) -> (Double, Double) -> Expectation
boxKeeps near@(lat, lng, radiusKm) (venueLat, venueLng) = do
  matchesVenueNear near (VenueSearchFields "v" Nothing Nothing (Just venueLat) (Just venueLng)) `shouldBe` True
  inVenueBoundingBox (venueBoundingBox lat lng radiusKm) venueLat venueLng `shouldBe` True

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size items = let (chunk, rest) = splitAt size items in chunk : chunksOf size rest

differing :: Text -> Text -> [(Text, Text)]
differing expected actual =
  [(want, got) | (want, got) <- zip (T.splitOn "\n" expected) (T.splitOn "\n" actual), want /= got]

entityId :: Entity SM.Venue -> Int64
entityId = fromSqlKey . entityKey

entityFields :: Entity SM.Venue -> VenueSearchFields
entityFields (Entity _ venue) =
  VenueSearchFields
    (SM.venueName venue) (SM.venueCity venue) (SM.venueAddress venue)
    (SM.venueLatitude venue) (SM.venueLongitude venue)

allVenues :: SqlPersistT IO [Entity SM.Venue]
allVenues = rawSql "SELECT ?? FROM venue ORDER BY venue.name, venue.id" []

-- | Replace the session's private venue table with the given rows.
seed :: [Stored] -> SqlPersistT IO ()
seed rows = do
  rawExecute "TRUNCATE pg_temp.venue" []
  forM_ rows $ \(Stored ident venue) ->
    rawExecute
      "INSERT INTO pg_temp.venue (id, name, city, address, latitude, longitude) VALUES (?, ?, ?, ?, ?, ?)"
      [ PersistInt64 ident
      , PersistText (vsfName venue)
      , maybe PersistNull PersistText (vsfCity venue)
      , maybe PersistNull PersistText (vsfAddress venue)
      , maybe PersistNull PersistDouble (vsfLatitude venue)
      , maybe PersistNull PersistDouble (vsfLongitude venue)
      ]

-- | Venues along a meridian; every seventh is in a city the text query finds.
seedSeries :: Int -> SqlPersistT IO ()
seedSeries size = do
  rawExecute "TRUNCATE pg_temp.venue" []
  rawExecute
    "INSERT INTO pg_temp.venue (id, name, city, latitude, longitude) \
    \SELECT g, 'Venue ' || lpad(g::text, 5, '0'), \
    \CASE WHEN g % 7 = 0 THEN U&'Ciudad \\00D1and\\00FA' ELSE 'Otra' END, \
    \-0.2 + g * 0.0001, -78.5 FROM generate_series(1, ?) g"
    [PersistInt64 (fromIntegral size)]

-- | One connection, so that the session-private copies of the venue and flag
-- tables stand in for the real ones in every statement, including the
-- handler's. The database's own rows are never read or written.
open :: String -> IO ConnectionPool
open connection = do
  pool <- runNoLoggingT (createPostgresqlPool (BS.pack connection) 1)
  runSqlPool (do
    rawExecute "CREATE TEMP TABLE venue (LIKE venue INCLUDING DEFAULTS)" []
    rawExecute "CREATE TEMP TABLE revenue_feature_flag (LIKE revenue_feature_flag INCLUDING DEFAULTS)" []
    rawExecute
      "INSERT INTO pg_temp.revenue_feature_flag (flag_key, enabled, environment, reason) \
      \VALUES (?, FALSE, 'sandbox', 'test'), (?, FALSE, 'production', 'test')"
      [PersistText venueSqlSearchFlag, PersistText venueSqlSearchFlag]) pool
  pure pool

listVenues
  :: ConnectionPool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> IO [VenueDTO]
listVenues pool city near needle limit offset = do
  cfg <- loadConfig
  let actor = AuthedUser (toSqlKey 7501) [Fan] (modulesForRoles [Fan]) Nothing Nothing
  answer <- case socialEventsServer actor of
    _events :<|> _cities :<|> _sources :<|> _research :<|> (list :<|> _venueRest) :<|> _rest ->
      runHandler (runReaderT (list city near needle limit offset) (Env pool cfg))
  either (\err -> fail ("unexpected refusal: " <> show (err :: ServerError))) pure answer

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres@127.0.0.1:", "postgresql://postgres:postgres@postgres:5432/"]
