import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import { fileURLToPath } from 'node:url';
import { checkAptTrust, checkBuildTrust, imageRecipes } from '../check-build-trust.mjs';
const root = fileURLToPath(new URL('../../', import.meta.url));

test('current image recipes and package authority satisfy their scoped contract', () => checkBuildTrust(root));
for (const recipe of imageRecipes) {
  const source = fs.readFileSync(new URL(`../../${recipe}`, import.meta.url), 'utf8');
  for (const [name, change] of [
    ['unsigned repositories', s => s.replace('AllowInsecureRepositories=false', 'AllowInsecureRepositories=true')],
    ['unauthenticated packages', s => s.replace('APT::Get::AllowUnauthenticated=false', 'APT::Get::AllowUnauthenticated=true')],
    ['expired metadata', s => s.replace('Check-Valid-Until=true', 'Check-Valid-Until=false')],
    ['override appended later', s => `${s}\nRUN apt-get --allow-unauthenticated install curl\n`],
    ['trusted repository override', s => `${s}\nRUN echo 'deb [trusted=yes] http://untrusted.invalid stable main' > /etc/apt/sources.list\n`],
    ['transient mirror failure tolerated', s => s.replace('--error-on=any ', '')],
    ['removed explicit policy', s => s.replace('-o Acquire::AllowInsecureRepositories=false ', '')],
  ]) test(`${recipe}: rejects ${name}`, () => {
    const mutation = change(source);
    assert.notEqual(mutation, source);
    assert.throws(() => checkAptTrust(mutation));
  });
}
