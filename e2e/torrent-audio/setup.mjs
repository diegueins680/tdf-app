import { execFileSync } from 'node:child_process';
export default function setup() {
  execFileSync(process.execPath, ['scripts/build-torrent-audio-fixture.mjs'], { stdio: 'inherit' });
}
