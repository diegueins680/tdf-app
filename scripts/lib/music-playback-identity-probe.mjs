import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';

export async function probePlaybackIdentity({ request, sql, anonymousEvent, authenticatedEvent, member, outsider }) {
  const authenticatedPath = '/music/me/playback-events';
  const authenticated = { token: member.token, method: 'POST', country: 'EC' };
  const rejected = async (path, options) => {
    const result = await request(path, { ...options, expected: 409 });
    assert.equal(result.code, 'playback_identity_conflict');
    assert.doesNotMatch(JSON.stringify(result), /SELECT|INSERT|anonymous-music-e2e|party_id/);
  };
  const before = sql('SELECT count(*) FROM music_playback_event');
  await request(authenticatedPath, { ...authenticated, json: authenticatedEvent });
  await rejected(authenticatedPath, { ...authenticated, token: outsider.token, json: authenticatedEvent });
  await rejected(authenticatedPath, { ...authenticated, json: { ...anonymousEvent,
    eventId: randomUUID(), sequenceNumber: 2 } });
  await rejected('/music/playback-events', { method: 'POST', country: 'EC', json: {
    ...anonymousEvent, anonymousId: 'different-anonymous-identity', eventId: randomUUID(), sequenceNumber: 2 } });
  await rejected('/music/playback-events', { method: 'POST', country: 'EC', json: {
    ...anonymousEvent, sessionId: authenticatedEvent.sessionId, eventId: randomUUID(), sequenceNumber: 2 } });
  await rejected(authenticatedPath, { ...authenticated, json: { ...authenticatedEvent, eventId: randomUUID() } });
  await rejected(authenticatedPath, { ...authenticated, json: { ...authenticatedEvent, positionMs: 10 } });
  assert.equal(sql('SELECT count(*) FROM music_playback_event'), before);

  // Distinct event IDs/sequences racing to own one fresh session: one identity wins.
  const sessionId = randomUUID();
  const race = await Promise.all([member, outsider].map((user, sequenceNumber) => request(authenticatedPath, {
    token: user.token, method: 'POST', country: 'EC', expected: [200, 409], json: {
      ...authenticatedEvent, eventId: randomUUID(), sessionId, sequenceNumber, eventType: 'pause',
    },
  })));
  assert.equal(race.filter(result => result?.code === 'playback_identity_conflict').length, 1);
  assert.equal(sql(`SELECT count(*) FROM music_playback_event WHERE session_id='${sessionId}'`), '1');

  // Event and history must roll back together if the history writer fails.
  const failedEvent = { ...authenticatedEvent, eventId: randomUUID(), sessionId: randomUUID(), sequenceNumber: 0 };
  sql(`CREATE FUNCTION music_e2e_history_failure() RETURNS trigger LANGUAGE plpgsql AS $$
    BEGIN
      IF EXISTS(SELECT 1 FROM music_playback_event WHERE event_id='${failedEvent.eventId}') THEN
        RAISE EXCEPTION 'synthetic_history_private_marker';
      END IF;
      RETURN NEW;
    END $$;
    CREATE TRIGGER music_e2e_history_failure BEFORE INSERT OR UPDATE ON music_playback_history
      FOR EACH ROW EXECUTE FUNCTION music_e2e_history_failure();`);
  try {
    const failure = await request(authenticatedPath, { ...authenticated, expected: 500, json: failedEvent });
    assert.doesNotMatch(String(failure), /synthetic_history_private_marker|INSERT|SELECT/);
    assert.equal(sql(`SELECT count(*) FROM music_playback_event WHERE event_id='${failedEvent.eventId}'`), '0');
  } finally {
    sql('DROP TRIGGER music_e2e_history_failure ON music_playback_history; DROP FUNCTION music_e2e_history_failure();');
  }
  await request(authenticatedPath, { ...authenticated, json: failedEvent });
  await request(authenticatedPath, { ...authenticated, json: failedEvent });
  assert.equal(sql(`SELECT count(*) FROM music_playback_event WHERE event_id='${failedEvent.eventId}'`), '1');

  const newer = { ...authenticatedEvent, eventId: randomUUID(), sessionId: randomUUID(),
    sequenceNumber: 0, eventType: 'pause', positionMs: 1500, occurredAt: new Date().toISOString() };
  await request(authenticatedPath, { ...authenticated, json: newer });
  const history = () => sql(`SELECT row_to_json(h) FROM music_playback_history h
    WHERE recording_id='${authenticatedEvent.recordingId}' AND party_id=(
      SELECT party_id FROM music_playback_event WHERE event_id='${authenticatedEvent.eventId}')`);
  const historyBefore = history();
  await request(authenticatedPath, { ...authenticated, json: { ...newer, eventId: randomUUID(),
    sequenceNumber: 1, positionMs: 200, occurredAt: new Date(Date.parse(newer.occurredAt) - 1000).toISOString() } });
  assert.equal(history(), historyBefore, 'Late telemetry must not rewind playback history');
  console.log('PASS playback identity → anonymous/account isolation, strict replay, concurrent ownership, event/history rollback and late-event ordering');
}
