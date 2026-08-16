BEGIN;

DROP TRIGGER IF EXISTS event_research_pilot_limit_trigger ON event_research_candidate;
DROP FUNCTION IF EXISTS enforce_event_research_pilot_limit();
DROP TABLE IF EXISTS event_research_change;
DROP TABLE IF EXISTS event_research_candidate;
DROP TABLE IF EXISTS event_research_run;
DROP TABLE IF EXISTS event_research_pilot_audit;
DROP TABLE IF EXISTS event_research_pilot_control;

DELETE FROM event_discovery_source
WHERE source_key IN (
  'meet2go-web',
  'passline-ec-web',
  'buenplan-social-web',
  'feelthetickets-web',
  'ticketshow-web',
  'ontime-tickets-web',
  'output-concerts-web'
)
AND source_type = 'web'
AND last_success_at IS NULL;

COMMIT;
