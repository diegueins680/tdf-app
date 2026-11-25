import assert from 'node:assert/strict';
import { randomBytes, randomUUID } from 'node:crypto';
import { readFile } from 'node:fs/promises';

const base = new URL(process.argv[2]);
assert.equal(base.protocol, 'http:');
assert.equal(base.hostname, '127.0.0.1');
const slug = 'tdf-records-estudio';
const profileId = 'a0000000-0000-4000-8000-000000000003';
const owner = 'synthetic-artist-listing-token', other = 'synthetic-studio-other';
const resource = '/directory/profiles/' + slug;
const write = '/directory/profiles/' + profileId + '/feed/' + randomUUID();
const files = await Promise.all(Array.from({length: 6}, (_, i) =>
  readFile(new URL('../../e2e/web/fixtures/studio-feed/' + (i + 1) + '.png', import.meta.url))));
async function request(path, method = 'GET', token, body) {
  const headers = token ? { Authorization: 'Bearer ' + token } : {};
  if (body && !(body instanceof FormData)) { headers['Content-Type'] = 'application/json'; body = JSON.stringify(body); }
  return fetch(new URL(path, base), { method, headers, body, redirect: 'error' });
}
async function expectStatus(response, statuses) {
  assert.ok(statuses.includes(response.status), response.url + ' HTTP ' + response.status + ' ' +
    (statuses.includes(response.status) ? '' : (await response.text()).slice(0, 220)));
  return response;
}
async function upload(bytes, name, token = owner, key = randomBytes(32).toString('hex')) {
  const form = new FormData();
  form.set('file', new Blob([bytes], {type: 'image/png'}), name);
  for (const [k,v] of Object.entries({authMode:'server', destination:'tdf', visibility:'private', idempotencyKey:key})) form.set(k,v);
  const response = await expectStatus(await request('/drive/upload','POST',token,form), [200,201]);
  return (await response.json()).duFileId;
}
const ids = [];
for (const [i, file] of files.entries()) {
  const key = randomBytes(32).toString('hex'), name = 'synthetic-' + i + '.png';
  ids.push(await upload(file, name, owner, key));
  if (i === 0) assert.equal(await upload(file, name, owner, key), ids[0]);
}
const feed = async () => (await (await expectStatus(await request(resource), [200])).json()).feed;
const image = id => resource + '/feed/' + id;
assert.deepEqual(await feed(), []);
for (const id of ids) await expectStatus(await request(image(id)), [404]);
const payload = {caption: 'Seis fotos sintéticas', images: ids};
await expectStatus(await request(write,'PUT',other,payload), [403,404]);
const otherFile = await upload(files[0], 'foreign.png', other);
await expectStatus(await request(write,'PUT',owner,{...payload, images:[otherFile]}), [404]);
await expectStatus(await request(write,'PUT',owner,{...payload, images:[randomUUID()]}), [404]);
const invalid = await upload(Buffer.from('synthetic text'), 'invalid.png');
await expectStatus(await request(write,'PUT',owner,{...payload,images:[invalid]}), [415]);
const oversized = await upload(Buffer.alloc(8388609), 'oversized.png');
await expectStatus(await request(write,'PUT',owner,{...payload,images:[oversized]}), [413]);
for (let i=0;i<2;i++) await expectStatus(await request(write,'PUT',owner,payload), [200,204]);
const [post, ...rest] = await feed();
assert.deepEqual(rest, []);
assert.deepEqual(post, {id: write.split('/').at(-1), ...payload});
for (const [i,id] of ids.entries()) {
  const response = await expectStatus(await request(image(id)), [200]);
  assert.match(response.headers.get('cache-control'), /no-store/);
  assert.equal(await response.json(), 'data:image/png;base64,' + files[i].toString('base64'));
}
await expectStatus(await request(write,'PATCH',other,{caption:'cambio',published:false}), [403,404]);
await expectStatus(await request(write,'PATCH',owner,{caption:'editado',published:true}), [200,204]);
assert.equal((await feed())[0].caption,'editado');
await expectStatus(await request(write,'PATCH',owner,{caption:'editado',published:false}), [200,204]);
assert.deepEqual(await feed(), []);
for (const id of ids) await expectStatus(await request(image(id)), [404]);
console.log('Studio feed HTTP passed: six real private uploads, retries, permission and media checks, anonymous read, edit, and withdrawal.');
