CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE party (id BIGINT PRIMARY KEY);
CREATE TABLE workflow_state (id UUID PRIMARY KEY, code TEXT NOT NULL);
CREATE TABLE social_event (
  id BIGINT PRIMARY KEY,
  organizer_party_id TEXT NULL,
  workflow_state_id UUID NULL REFERENCES workflow_state(id)
);
CREATE TABLE event_invitation (
  id BIGINT PRIMARY KEY,
  event_id BIGINT NOT NULL REFERENCES social_event(id)
);
CREATE TABLE event_logistics_activity (
  id BIGINT PRIMARY KEY,
  event_id BIGINT NOT NULL REFERENCES social_event(id),
  status TEXT NOT NULL,
  version INTEGER NOT NULL
);
CREATE TABLE event_logistics_dependency (
  id BIGSERIAL PRIMARY KEY,
  activity_id BIGINT NOT NULL REFERENCES event_logistics_activity(id),
  depends_on_activity_id BIGINT NOT NULL REFERENCES event_logistics_activity(id),
  UNIQUE (activity_id, depends_on_activity_id)
);

INSERT INTO party(id) VALUES (1), (2), (3);
INSERT INTO workflow_state(id, code) VALUES
  ('00000000-0000-0000-0000-000000000001', 'planning'),
  ('00000000-0000-0000-0000-000000000002', 'live');
INSERT INTO social_event(id, organizer_party_id, workflow_state_id) VALUES
  (10, '1', '00000000-0000-0000-0000-000000000001'),
  (11, NULL, '00000000-0000-0000-0000-000000000002');
INSERT INTO event_invitation(id, event_id) VALUES (50, 10);
INSERT INTO event_logistics_activity(id, event_id, status, version) VALUES
  (100, 10, 'planned', 1),
  (101, 10, 'planned', 1),
  (102, 11, 'planned', 1),
  (103, 10, 'planned', 1),
  (104, 10, 'planned', 1);
