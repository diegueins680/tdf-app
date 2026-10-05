-- A deliberately narrow direct-harness admission boundary. Do not parse or
-- interpolate arbitrary libpq strings: only these complete URLs are supported.
module TDF.DisposableTicketDatabase (safeTicketDatabase) where

safeTicketDatabase :: Bool -> [Maybe String] -> String -> Bool
safeTicketDatabase ci overrides dsn =
  not (any (maybe False (not . null)) overrides)
  && dsn `elem` (local ++ if ci then docker else [])
  where
    database = "/tdf_ticket_admission_test"
    local = [scheme ++ host ++ port ++ database
      | scheme <- ["postgresql://", "postgres://"]
      , host <- ["127.0.0.1", "localhost"]
      , port <- ["", ":5432"]]
    docker = ["postgresql://postgres:postgres@postgres:5432" ++ database]
