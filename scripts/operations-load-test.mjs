#!/usr/bin/env node
import { randomUUID } from 'node:crypto';

const baseUrl = process.env.TDF_OPS_BASE_URL?.replace(/\/$/, '');
const token = process.env.TDF_OPS_TOKEN?.trim();
const organizationId = process.env.TDF_OPS_ORGANIZATION_ID?.trim();
const workItemId = process.env.TDF_OPS_WORK_ITEM_ID?.trim();
const assigneePartyId = Number(process.env.TDF_OPS_ASSIGNEE_PARTY_ID ?? '0');
const allowMutations = process.env.TDF_OPS_ALLOW_MUTATIONS === '1';

if (!baseUrl || !token) {
  throw new Error('TDF_OPS_BASE_URL and TDF_OPS_TOKEN are required');
}

const headers = { Authorization: token.startsWith('Bearer ') ? token : `Bearer ${token}`, 'Content-Type': 'application/json' };

async function request(path, options = {}) {
  const started = performance.now();
  const response = await fetch(`${baseUrl}${path}`, { ...options, headers: { ...headers, ...(options.headers ?? {}) } });
  const elapsed = performance.now() - started;
  const text = await response.text();
  if (!response.ok) throw new Error(`${options.method ?? 'GET'} ${path} returned ${response.status}: ${text.slice(0, 240)}`);
  return { elapsed, body: text ? JSON.parse(text) : null };
}

const percentile = (values, p) => {
  const sorted = [...values].sort((a, b) => a - b);
  return sorted[Math.min(sorted.length - 1, Math.ceil(sorted.length * p) - 1)] ?? 0;
};

async function boundedMap(count, concurrency, task) {
  const results = new Array(count);
  let cursor = 0;
  await Promise.all(Array.from({ length: concurrency }, async () => {
    while (cursor < count) {
      const index = cursor++;
      results[index] = await task(index);
    }
  }));
  return results;
}

const inboxSamples = await boundedMap(100, 10, async (index) => {
  const result = await request(`/operations/work-items?limit=40&status=${index % 2 ? 'new' : 'in_progress'}`);
  return result.elapsed;
});

const detailSamples = workItemId
  ? await boundedMap(50, 5, async () => (await request(`/operations/work-items/${encodeURIComponent(workItemId)}`)).elapsed)
  : [];

const quickActionSamples = [];
if (allowMutations && workItemId && Number.isInteger(assigneePartyId) && assigneePartyId > 0) {
  let detail = (await request(`/operations/work-items/${encodeURIComponent(workItemId)}`)).body;
  for (let index = 0; index < 20; index += 1) {
    const result = await request(`/operations/work-items/${encodeURIComponent(workItemId)}/assignment`, {
      method: 'PATCH',
      body: JSON.stringify({
        expectedVersion: detail.workItem.version,
        assigneePartyId,
        responsibleTeam: 'load-test',
        reason: 'Authorized staging load test',
        requestId: randomUUID(),
        sourceClient: 'operations-load-test',
      }),
    });
    quickActionSamples.push(result.elapsed);
    detail = { workItem: result.body };
  }
}

const visibilitySamples = [];
if (allowMutations && organizationId) {
  for (let index = 0; index < 10; index += 1) {
    const baseline = await request(`/operations/events?limit=1&organizationId=${encodeURIComponent(organizationId)}`);
    const afterId = baseline.body.lastEventId ?? 0;
    const started = performance.now();
    const correlationKey = `load-test:${randomUUID()}`;
    const created = await request('/operations/work-items', {
      method: 'POST',
      body: JSON.stringify({
        organizationId,
        uncorrelated: true,
        entityType: 'manual',
        correlationKey,
        titleEs: 'Prueba de carga autorizada',
        titleEn: 'Authorized load test',
        descriptionEs: 'Dato sintético marcado para staging.',
        descriptionEn: 'Synthetic staging-only data.',
        priority: 'low',
        metadata: { synthetic: true, testRun: 'operations-load-test' },
        requestId: randomUUID(),
        sourceClient: 'operations-load-test',
      }),
    });
    let visible = false;
    while (!visible && performance.now() - started < 5000) {
      const batch = await request(`/operations/events?afterId=${afterId}&limit=250&organizationId=${encodeURIComponent(organizationId)}`);
      visible = batch.body.events.some((event) => event.workItemId === created.body.id);
      if (!visible) await new Promise((resolve) => setTimeout(resolve, 50));
    }
    if (!visible) throw new Error(`Created work item ${created.body.id} was not visible within 5 seconds`);
    visibilitySamples.push(performance.now() - started);
  }
}

const results = {
  environment: baseUrl,
  generatedAt: new Date().toISOString(),
  inbox: { samples: inboxSamples.length, p95Ms: percentile(inboxSamples, 0.95), targetMs: 500 },
  detail: { samples: detailSamples.length, p95Ms: percentile(detailSamples, 0.95) },
  quickAction: { samples: quickActionSamples.length, p95Ms: percentile(quickActionSamples, 0.95), targetMs: 750 },
  visibility: { samples: visibilitySamples.length, p95Ms: percentile(visibilitySamples, 0.95), targetMs: 3000 },
};
console.log(JSON.stringify(results, null, 2));

const failed = results.inbox.p95Ms >= 500
  || (quickActionSamples.length > 0 && results.quickAction.p95Ms >= 750)
  || (visibilitySamples.length > 0 && results.visibility.p95Ms >= 3000);
if (failed) process.exitCode = 1;
