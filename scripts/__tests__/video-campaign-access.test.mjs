import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';
import { checkCampaignAccessSource, verifyCampaignAccess } from '../verify-video-campaign-access.mjs';

test('enumerates the bounded manager policy and catches every removed guard', () => {
  const result = verifyCampaignAccess();
  assert.equal(result.states, 16384);
  assert.equal(Object.keys(result.guardRemovalControls).length, 13);
  assert.ok(Object.values(result.guardRemovalControls).every((count) => count > 0));
});
test('rejects source missing canonical identity, private access, publication or locks', () => {
  const source = readFileSync(new URL('../../tdf-hq/src/TDF/VideoCampaigns.hs', import.meta.url), 'utf8');
  for (const clause of ['manager.account_party_id=?', 'profile.subject_party_id=?', 'manager.can_view_private', 'manager.can_publish', 'FOR SHARE OF manager,profile']) {
    assert.throws(() => checkCampaignAccessSource(source.replace(clause, 'TRUE')));
  }
});
