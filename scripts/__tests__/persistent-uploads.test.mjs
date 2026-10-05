import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
import { parse } from 'yaml';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const guard = path.join(root, 'tdf-hq/persistent-uploads.sh');

test('the actual shell guard admits one writable mount and rejects ephemeral/ambiguous storage', context => {
  const directory = mkdtempSync(path.join(tmpdir(), 'tdf-upload-mount-'));
  context.after(() => rmSync(directory, { recursive: true, force: true }));
  const target = path.join(directory, 'uploads');
  const table = path.join(directory, 'mountinfo');
  mkdirSync(target);
  const row = (mount = target, mode = 'rw', filesystem = 'ext4') => `10 9 0:1 / ${mount} ${mode},nosuid - ${filesystem} /dev/fixture ${mode}\n`;
  const run = source => {
    writeFileSync(table, source);
    return spawnSync('sh', ['-c', '. "$1"; require_private_upload_mount "$2" "$3"',
      'mount-test', guard, table, target], { encoding: 'utf8' });
  };
  assert.equal(run(row()).status, 0);
  for (const [name, source] of [
    ['writable container layer', row('/')],
    ['sibling mount', row(`${target}-other`)],
    ['read-only mount', row(target, 'ro')],
    ['similar option is not rw', row(target, 'notrw')],
    ['temporary memory filesystem', row(target, 'rw', 'tmpfs')],
    ['container overlay filesystem', row(target, 'rw', 'overlay')],
    ['ambiguous stacked mounts', row() + row(target, 'ro')],
    ['missing mount', ''],
  ]) {
    const result = run(source);
    assert.equal(result.status, 1, `${name}: ${result.stderr}`);
  }
  rmSync(target, { recursive: true });
  assert.equal(run(row()).status, 1, 'A claimed mount with no directory cannot pass');
});

test('every configured production application service persists private uploads', () => {
  const compose = parse(readFileSync(path.join(root, 'ops/hetzner/compose.production.yaml'), 'utf8'));
  const applications = Object.entries(compose.services).filter(([, service]) =>
    service.environment?.APP_ENV === 'production' && service.environment?.TDF_MIGRATION_PRECHECK_ONLY !== 'true');
  assert.deepEqual(applications.map(([name]) => name).sort(), ['api', 'canary']);
  for (const [name, service] of applications) {
    const mounts = service.volumes.filter(value => typeof value === 'object' && value.target === '/app/uploads');
    assert.equal(mounts.length, 1, `${name}: exactly one private upload mount`);
    assert.equal(mounts[0].type, 'bind', name);
    assert.equal(mounts[0].source, './uploads', name);
    assert.equal(mounts[0].bind.create_host_path, false, name);
    assert.notEqual(mounts[0].read_only, true, `${name}: application requires writable storage`);
  }
  for (const name of ['Dockerfile', 'Dockerfile.runtime']) {
    assert.match(readFileSync(path.join(root, 'tdf-hq', name), 'utf8'),
      /^COPY tdf-hq\/persistent-uploads\.sh \/app\/persistent-uploads\.sh$/m);
  }
});
