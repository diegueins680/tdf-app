#!/usr/bin/env node
// Deliberate renderer double for worker commit/revocation tests. NOT valid ERN.
import { writeFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
const [exportId, xml, manifest] = process.argv.slice(2);
if (!/^postgresql:\/\/127\.0\.0\.1:5432\/tdf_music_worker_[a-z0-9_]+\?/.test(process.env.DATABASE_URL ?? '')) {
  throw new Error('Disposable worker test database required');
}
if (process.env.MUSIC_TEST_REVOKE_RECIPIENT === 'true') {
  const result = spawnSync('psql', [process.env.DATABASE_URL, '-Xq', '-v', 'ON_ERROR_STOP=1',
    '-v', `export_id=${exportId}`, '-f', '-'], { encoding: 'utf8', input:
    "UPDATE music_ddex_party_registry SET active=false WHERE id=(SELECT recipient_registry_id FROM music_ddex_export WHERE id=:'export_id'::uuid);" });
  if (result.status !== 0) throw new Error('Synthetic registry revocation failed');
}
writeFileSync(xml, '<SyntheticWorkerCommitFixture/>');
writeFileSync(manifest, '');
