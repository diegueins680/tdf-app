-- EVT-VENUE-SEARCH-001: print the venue_search_fold(text) definition derived from
-- Data.Text.toCaseFold. Run from tdf-hq/:
--   stack exec -- runghc -isrc ../scripts/render-venue-search-fold.hs
-- The text must appear unchanged in tdf-hq/sql/2026-11-21_venue_search_sql.sql;
-- TDF.Venues.SearchSpec fails when it does not.
import qualified Data.Text.IO as TIO
import TDF.Venues.SearchFold (venueSearchFoldFunctionSql)

main :: IO ()
main = TIO.putStr venueSearchFoldFunctionSql
