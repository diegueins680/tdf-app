{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | COMMUNITY-ARTIST-SIMILARITY-001 on PostgreSQL: the job, the stored read
-- and the reference, run through the real loaders against
-- scripts/artist-similarity-fixture.sql. Driven by
-- scripts/test-artist-similarity-postgres.sh.
import           Control.Monad                  (forM_, unless)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Logger           (runNoLoggingT)
import qualified Data.ByteString.Char8          as BS
import           Data.Int                       (Int64)
import           Data.List                      (sort)
import qualified Data.Text                      as T
import           Database.Persist               (PersistValue, toPersistValue)
import           Database.Persist.Postgresql
import           System.Environment             (getEnv)

import           TDF.Fans.ArtistRecommendations
import           TDF.Fans.ArtistSimilarity

main :: IO ()
main = do
  connection <- BS.pack <$> getEnv "TDF_ARTIST_SIMILARITY_TEST_DATABASE_URL"
  runNoLoggingT $ withPostgresqlPool connection 2 $ \pool -> liftIO $ do
    let run :: SqlPersistT IO a -> IO a
        run action = runSqlPool action pool
        generate = runSqlPoolWithIsolation runSimilarityGeneration pool RepeatableRead
        execute statement = run (rawExecute statement [])
        check label condition = unless condition (fail label)
        fans = [100 .. 106] :: [Int64]
        recommend source fan =
          run (loadSimilarityRecommendations source (toSqlKey fan) maxRecommendationLimit)
        stored = recommend StoredGeneration
        live = recommend LiveSignals
        ids = map recArtistId
        reasons = map recReason
        close a b = abs (a - b) < (1e-9 :: Double)

    -- Before any generation: the fallback only.
    empty <- stored 100
    check "no generation must give the popularity fallback" (all ((== Popular) . recReason) empty)
    check "fallback without generation" (ids empty == [2, 4])

    -- The job, in one REPEATABLE READ transaction.
    first <- generate
    check "generation summary" (gsArtists first == 6 && gsPairs first == 6)
    norms :: [(Single Int64, Single Int64)] <- run $ rawSql
      "SELECT artist_party_id, norm2 FROM artist_similarity_norm WHERE generation = ? ORDER BY 1"
      [toPersistValue (gsGeneration first)]
    check "stored norms"
      ([(a, n) | (Single a, Single n) <- norms]
        == [(1, 384), (2, 128), (3, 128), (4, 64), (5, 32), (6, 13)])
    pairs :: [(Single Int64, Single Int64, Single Int64, Single Int64)] <- run $ rawSql
      "SELECT artist_a, artist_b, dot, support::bigint FROM artist_similarity_pair \
      \WHERE generation = ? ORDER BY 1, 2"
      [toPersistValue (gsGeneration first)]
    check "stored pairs"
      ([(a, b, d, s) | (Single a, Single b, Single d, Single s) <- pairs]
        == [(1, 2, 128, 2), (1, 3, 128, 2), (1, 5, 64, 2), (1, 6, 40, 2), (2, 6, 40, 2), (3, 5, 64, 2)])

    -- Refinement: the stored read equals the reference for every fan.
    forM_ fans $ \fan -> do
      fromTable <- stored fan
      fromSignals <- live fan
      check ("stored read differs from the reference for fan " <> show fan)
        (fromTable == fromSignals)
      check ("a hidden artist was returned or named for fan " <> show fan)
        (all (\item -> recArtistId item /= 7 && notElem 7 (recSeedArtistIds item)) fromTable)

    -- Fan 100 follows artist 1 (and the hidden 7).
    hundred <- stored 100
    check "ranking of fan 100" (ids hundred == [2, 3, 5, 6, 4])
    check "reasons of fan 100"
      (reasons hundred == [SimilarFans, SimilarFans, SimilarFans, SimilarFans, Popular])
    check "seeds of fan 100" (map recSeedArtistIds hundred == [[1], [1], [1], [1], []])
    check "scores of fan 100" $ case map recScore hundred of
      [a, b, c, d, e] ->
        close a (128 / sqrt (384 * 128)) && b == a && c == a
          && close d (40 / sqrt (384 * 13)) && e == 0
      _ -> False

    -- Opt-out: fan 106 follows artist 1 and gets exactly the fallback.
    optedOut <- stored 106
    optedOutLive <- live 106
    check "opt-out must give exactly the popularity fallback"
      (ids optedOut == [2, 4] && all ((== Popular) . recReason) optedOut && optedOut == optedOutLive)

    -- The per-fan signal queries are the global ones restricted to the fan.
    favourites :: [(Single Int64, Single Int64)] <- run (rawSql favouriteSignalsSql [])
    rsvps :: [(Single Int64, Single Int64)] <- run (rawSql rsvpSignalsSql [])
    windowRows :: [Single PersistValue] <- run (rawSql "SELECT now() - interval '180 days'" [])
    window <- case windowRows of
      [Single value] -> pure value
      _ -> fail "window start"
    plays :: [(Single Int64, Single Int64, Single Int64)] <- run (rawSql playSignalsSql [window])
    check "favourite signals"
      (sort [(f, a) | (Single f, Single a) <- favourites] == [(103, 3), (104, 3), (106, 3)])
    check "RSVP signals" (sort [(f, a) | (Single f, Single a) <- rsvps] == [(103, 5), (104, 5)])
    check "play signals"
      (sort [(f, a, n) | (Single f, Single a, Single n) <- plays] == [(101, 6, 3), (102, 6, 12)])
    forM_ fans $ \fan -> do
      mine :: [Single Int64] <- run (rawSql favouriteArtistsForFanSql [toPersistValue fan])
      check ("favourites of fan " <> show fan)
        (sort [a | Single a <- mine] == sort [a | (Single f, Single a) <- favourites, f == fan])
      answered :: [Single Int64] <-
        run (rawSql rsvpArtistsForFanSql [toPersistValue (T.pack (show fan))])
      check ("RSVPs of fan " <> show fan)
        (sort [a | Single a <- answered] == sort [a | (Single f, Single a) <- rsvps, f == fan])
      played :: [(Single Int64, Single Int64)] <-
        run (rawSql playCountsForFanSql [toPersistValue fan, window])
      check ("plays of fan " <> show fan)
        (sort [(a, n) | (Single a, Single n) <- played]
          == sort [(a, n) | (Single f, Single a, Single n) <- plays, f == fan])

    -- Writes after the snapshot: the stored similarity does not move, the
    -- caller's own exclusions and the listing apply at once.
    execute "INSERT INTO fan_follow (fan_party_id, artist_party_id) VALUES (103, 4), (104, 4)"
    afterWrites <- stored 100
    check "a later follow of other fans must not change the stored ranking"
      (afterWrites == hundred)
    liveAfterWrites <- live 100
    check "the reference on the live rows sees the new follows"
      (4 `elem` [recArtistId item | item <- liveAfterWrites, recReason item == SimilarFans])
    execute "INSERT INTO fan_follow (fan_party_id, artist_party_id) VALUES (100, 2)"
    followed <- stored 100
    check "an artist followed after the snapshot is excluded at once" (2 `notElem` ids followed)
    execute
      "INSERT INTO directory_profile (subject_party_id, profile_status, visibility, moderation_status) \
      \VALUES (3, 'draft', 'public', 'allowed')"
    hidden <- stored 100
    check "an artist hidden after the snapshot is dropped at once"
      (all (\item -> recArtistId item /= 3 && notElem 3 (recSeedArtistIds item)) hidden)

    -- The next run catches up, and refinement holds again.
    second <- generate
    check "second generation is newer" (gsGeneration second > gsGeneration first)
    forM_ fans $ \fan -> do
      fromTable <- stored fan
      fromSignals <- live fan
      check ("after the second run the stored read differs for fan " <> show fan)
        (fromTable == fromSignals)

    -- Two generations are kept.
    _ <- generate
    kept :: [Single Int64] <- run (rawSql "SELECT count(*)::bigint FROM artist_similarity_generation" [])
    check "exactly two generations are kept" (kept == [Single 2])
    orphans :: [Single Int64] <- run $ rawSql
      "SELECT count(*)::bigint FROM artist_similarity_pair pair WHERE NOT EXISTS \
      \(SELECT 1 FROM artist_similarity_generation g WHERE g.id = pair.generation)" []
    check "no rows of a pruned generation remain" (orphans == [Single 0])

    -- A generation older than the limit is not served.
    execute
      "UPDATE artist_similarity_generation SET snapshot_at = now() - interval '49 hours', \
      \completed_at = now() - interval '49 hours'"
    stale <- stored 101
    check "a stale generation must give the popularity fallback"
      (all ((== Popular) . recReason) stale)
    execute
      "UPDATE artist_similarity_generation SET snapshot_at = now() - interval '47 hours', \
      \completed_at = now() - interval '47 hours'"
    fresh <- stored 101
    check "a generation inside the limit is served"
      (any ((== SimilarFans) . recReason) fresh)
    putStrLn "Artist similarity on PostgreSQL: job, stored read, reference, opt-out, staleness checked"
