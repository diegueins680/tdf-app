// Real curl/MinIO protocol with a loopback-only TLS fault proxy. No cloud calls.
import assert from 'node:assert/strict';
import https from 'node:https';
import { spawn } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { Transform } from 'node:stream';

export async function probeWorkerUploadFaults({ root, certs, endpoint, bucket, source, env, check }) {
  const ca = readFileSync(join(certs, 'public.crt'));
  const sockets = new Set();
  let scenario, child, actions, attempts, reachedPart;
  const proxy = https.createServer({ cert: ca, key: readFileSync(join(certs, 'private.key')) }, (req, res) => {
    const url = new URL(req.url, endpoint);
    const action = req.method === 'DELETE' ? 'abort' : url.searchParams.has('uploads') ? 'create'
      : req.method === 'PUT' ? `part${url.searchParams.get('partNumber')}` : 'complete';
    actions.push(action);
    if (action === 'part2') {
      attempts += 1;
      if (scenario === 'retry' && attempts === 1) {
        req.resume(); res.writeHead(503, { 'Content-Type': 'application/xml' });
        res.end('<Error><Code>SlowDown</Code></Error>'); return;
      }
      if (scenario === 'cancel') { req.resume(); reachedPart(); return; }
    }
    if (scenario === 'embedded_error' && action === 'complete') {
      req.resume(); res.writeHead(200, { 'Content-Type': 'application/xml' });
      res.end('<Error><Code>InternalError</Code></Error>'); return;
    }
    // Keep the signed Host unchanged; route only to this run's MinIO origin.
    const upstream = https.request(url, { method: req.method, ca, agent: false, headers: req.headers }, (response) => {
      res.writeHead(response.statusCode, response.headers); response.pipe(res);
    });
    upstream.on('error', () => { if (!res.headersSent) res.writeHead(502); res.end(); });
    req.on('aborted', () => upstream.destroy());
    res.on('close', () => upstream.destroy());
    if (scenario === 'corruption' && action === 'part2') {
      let mutated = false;
      req.pipe(new Transform({ transform(chunk, _encoding, callback) {
        if (!mutated && chunk.length) { chunk = Buffer.from(chunk); chunk[0] ^= 255; mutated = true; }
        callback(null, chunk);
      } })).pipe(upstream);
    } else req.pipe(upstream);
  });
  proxy.on('connection', (socket) => { sockets.add(socket); socket.on('close', () => sockets.delete(socket)); });
  await new Promise((resolve, reject) => { proxy.once('error', reject); proxy.listen(0, '127.0.0.1', resolve); });
  const origin = `https://127.0.0.1:${proxy.address().port}`;
  try {
    for (scenario of ['retry', 'corruption', 'embedded_error', 'cancel']) {
      await check(`worker real S3 through TLS proxy: ${scenario}`, async () => {
        actions = []; attempts = 0;
        const partReached = new Promise((resolve) => { reachedPart = resolve; });
        const sourceBefore = readFileSync(source);
        child = spawn('perl', [join(root, 'scripts/music-s3-upload.pl'), source,
          bucket, `workers/fault-${scenario}`, 'audio/wav'], { env: { ...env,
          MUSIC_S3_ENDPOINT: origin, MUSIC_WORKER_TRANSFER_TIMEOUT_SECONDS: '30' },
          stdio: ['ignore', 'pipe', 'pipe'] });
        let stdout = '', stderr = '';
        child.stdout.on('data', (data) => { stdout += data; });
        child.stderr.on('data', (data) => { stderr += data; });
        let deadline;
        const done = new Promise((resolve, reject) => {
          deadline = setTimeout(() => { child.kill('SIGTERM'); reject(new Error('Worker fault probe timed out')); }, 60000);
          child.on('error', reject); child.on('close', (status) => { clearTimeout(deadline); resolve(status); });
        });
        try {
          if (scenario === 'cancel') {
            await Promise.race([partReached, done.then(() => { throw new Error('Worker exited before cancellation point'); })]);
            child.kill('SIGTERM');
          }
          const status = await done;
          assert.equal(status, scenario === 'retry' ? 0 : scenario === 'cancel' ? 143 : 1, stderr);
          assert.equal(actions.filter((action) => action === 'create').length, 1);
          assert.equal(actions.filter((action) => action === 'part1').length, 1);
          if (scenario === 'retry') {
            assert.equal(attempts, 2); assert.equal(JSON.parse(stdout).parts, 2);
            assert.equal(actions.at(-1), 'complete');
          } else {
            assert.equal(stdout, ''); assert.equal(actions.at(-1), 'abort');
            assert.match(stderr, /incomplete upload aborted/);
            if (scenario === 'corruption') { assert.equal(attempts, 4); assert.ok(!actions.includes('complete')); }
          }
          assert.deepEqual(readFileSync(source), sourceBefore);
        } finally { clearTimeout(deadline); }
      });
    }
  } finally {
    if (child?.exitCode === null) child.kill('SIGTERM');
    for (const socket of sockets) socket.destroy();
    await new Promise((resolve) => proxy.close(resolve));
  }
}
