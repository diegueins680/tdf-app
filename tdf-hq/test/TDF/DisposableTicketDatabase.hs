-- A deliberately narrow direct-harness admission boundary. Do not parse or
-- interpolate arbitrary libpq strings: only these complete URLs are supported.
module TDF.DisposableTicketDatabase (safeTicketDatabase, safeConfirmationDatabase) where

safeTicketDatabase :: Bool -> [Maybe String] -> String -> Bool
safeTicketDatabase = safeNamedDatabase "/tdf_ticket_admission_test"

safeConfirmationDatabase :: Bool -> [Maybe String] -> String -> Bool
safeConfirmationDatabase = safeNamedDatabase "/tdf_ticket_confirmation_worker_test"

safeNamedDatabase :: String -> Bool -> [Maybe String] -> String -> Bool
safeNamedDatabase database ci overrides dsn =
  not (any (maybe False (not . null)) overrides)
  && dsn `elem` (local ++ if ci then docker else [])
  where
    local = [scheme ++ host ++ port ++ database
      | scheme <- ["postgresql://", "postgres://"]
      , host <- ["127.0.0.1", "localhost"]
      , port <- ["", ":5432"]]
    docker = ["postgresql://postgres:postgres@postgres:5432" ++ database]
