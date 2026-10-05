module Main (main) where

import Control.Monad (forM_, unless)
import TDF.DisposableTicketDatabase (safeTicketDatabase, safeConfirmationDatabase)

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
  let confirmation = "postgresql://127.0.0.1/tdf_ticket_confirmation_worker_test"
  check "confirmation local" (safeConfirmationDatabase False [] confirmation)
  check "confirmation CI" (safeConfirmationDatabase True [] "postgresql://postgres:postgres@postgres:5432/tdf_ticket_confirmation_worker_test")
  check "admission name is not confirmation" (not (safeConfirmationDatabase True [] local))
  forM_ ["postgresql://remote.example/tdf_ticket_confirmation_worker_test",
         confirmation ++ "?host=remote.example", confirmation ++ "#fragment",
         "host=127.0.0.1 dbname=tdf_ticket_confirmation_worker_test"] $ \dsn ->
    check "unsafe confirmation URI rejected" (not (safeConfirmationDatabase True [] dsn))
  check "confirmation overrides rejected" (not (safeConfirmationDatabase True [Just "remote"] confirmation))
  putStrLn "Ticket direct-harness routing: admission and confirmation controls passed"
