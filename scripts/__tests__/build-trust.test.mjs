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
    ['Deb822 trusted source', s => `${s}\nRUN sed -i '/^Types: deb/a Trusted: yes' /etc/apt/sources.list.d/debian.sources\n`],
    ['Deb822 expired metadata', s => `${s}\nRUN sed -i '/^Types: deb/a Check-Valid-Until: no' /etc/apt/sources.list.d/debian.sources\n`],
    ['Deb822 insecure source', s => `${s}\nRUN sed -i '/^Types: deb/a Allow-Insecure: yes' /etc/apt/sources.list.d/debian.sources\n`],
    ['Deb822 weak source', s => `${s}\nRUN sed -i '/^Types: deb/a Allow-Weak: yes' /etc/apt/sources.list.d/debian.sources\n`],
    ['removed explicit policy', s => s.replace('-o Acquire::AllowInsecureRepositories=false ', '')],
  ]) test(`${recipe}: rejects ${name}`, () => {
    const mutation = change(source);
    assert.notEqual(mutation, source);
    assert.throws(() => checkAptTrust(mutation));
  });
}
