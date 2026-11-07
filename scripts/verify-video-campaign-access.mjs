import { readFileSync } from 'node:fs';
import { fileURLToPath, pathToFileURL } from 'node:url';
import path from 'node:path';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const clauses = {
  actor: 'manager.account_party_id=?', artist: 'profile.subject_party_id=?',
  active: 'manager.active', private: 'manager.can_view_private', edit: 'manager.can_edit',
  kind: "profile.profile_kind IN ('artist','band','project')",
  lifecycle: "profile.profile_status NOT IN ('archived','suspended','merged')",
  moderation: "profile.moderation_status<>'blocked'", publish: 'manager.can_publish',
  published: "profile.profile_status='published'", allowed: "profile.moderation_status='allowed'",
  visibility: "profile.visibility IN ('public','unlisted')",
};

export function checkCampaignAccessSource(source) {
  const sql = /campaignManagerAccess actor artist publishing = do\s+rows <- rawSql\s+"([^"]+)"/.exec(source)?.[1];
  if (!sql) throw new Error('Missing actual manager SQL');
  for (const [name, clause] of Object.entries(clauses)) {
    if (!sql.includes(clause)) throw new Error(`Missing campaign guard: ${name}`);
  }
  if (!sql.includes('FOR SHARE OF manager,profile')) throw new Error('Manager/profile locks missing');
  const admission = source.slice(source.indexOf('    queryWith publishing action = do'), source.indexOf('    list = query'));
  if ((admission.match(/withCurrentAuthorization/g) ?? []).length !== 2
    || !admission.includes('campaignScope live target')
    || !admission.includes('campaignManagerAccess (auPartyId user) artist publishing')
    || !admission.includes('if allowed then perform else pure (Left err403)')
    || !source.includes('queryWith True $ \\artist -> do')) throw new Error('Current-session or publication admission changed');
}

// Finite Boolean abstraction of the declared manager predicate, not SQL semantics
// or a proof of the authentication primitive, lock implementation or all executions.
export function verifyCampaignAccessModel() {
  const names = [...Object.keys(clauses), 'session'];
  const counters = Object.fromEntries(names.map((key) => [key, 0]));
  let states = 0;
  for (let mask = 0; mask < 2 ** names.length; mask += 1) {
    const x = Object.fromEntries(names.map((key, index) => [key, Boolean(mask & (1 << index))]));
    for (const publishing of [false, true]) {
      const base = ['actor', 'artist', 'active', 'private', 'edit', 'kind', 'lifecycle', 'moderation', 'session'];
      const required = publishing ? [...base, 'publish', 'published', 'allowed', 'visibility'] : base;
      const specification = x.session && x.actor && x.artist && x.active && x.private && x.edit
        && x.kind && x.lifecycle && x.moderation
        && (!publishing || (x.publish && x.published && x.allowed && x.visibility));
      if (required.every((key) => x[key]) !== specification) throw new Error('Manager policy mismatch');
      for (const mutation of names) {
        if (required.filter((key) => key !== mutation).every((key) => x[key]) && !specification) counters[mutation] += 1;
      }
      states += 1;
    }
  }
  if (Object.values(counters).some((count) => count === 0)) throw new Error('A guard-removal mutant escaped');
  return { states, guardRemovalControls: counters, scope: 'Finite manager predicate and source-presence binding; no universal implementation or interleaving proof.' };
}

export function verifyCampaignAccess() {
  checkCampaignAccessSource(readFileSync(path.join(root, 'tdf-hq/src/TDF/VideoCampaigns.hs'), 'utf8'));
  return verifyCampaignAccessModel();
}
if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  console.log(JSON.stringify(verifyCampaignAccess()));
}
