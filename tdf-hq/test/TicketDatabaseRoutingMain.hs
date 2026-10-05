module Main (main) where

import Control.Monad (forM_, unless)
import TDF.DisposableTicketDatabase (safeTicketDatabase)

main :: IO ()
main = do
  let local = "postgresql://127.0.0.1/tdf_ticket_admission_test"
      docker = "postgresql://postgres:postgres@postgres:5432/tdf_ticket_admission_test"
      check label condition = unless condition (fail label)
  check "local admission" (safeTicketDatabase False [] local)
  check "CI admission" (safeTicketDatabase True [] docker)
  check "CI requires opt-in" (not (safeTicketDatabase False [] docker))
  forM_ ["postgresql://remote.example/tdf_ticket_admission_test",
         "postgresql://127.0.0.1/production", "host=127.0.0.1 dbname=tdf_ticket_admission_test",
         local ++ "?host=remote.example", local ++ "#fragment",
         "postgresql://127.0.0.1/%74df_ticket_admission_test",
         "postgresql://127.0.0.1/../tdf_ticket_admission_test"] $ \dsn ->
    check "unsafe URI rejected" (not (safeTicketDatabase True [] dsn))
  forM_ [[Just "remote", Nothing, Nothing], [Nothing, Just "remote", Nothing],
         [Nothing, Nothing, Just "/synthetic/service"]] $ \overrides ->
    check "inherited routing override rejected" (not (safeTicketDatabase True overrides local))
  putStrLn "Ticket direct-harness routing: 13 admission/negative controls passed"
