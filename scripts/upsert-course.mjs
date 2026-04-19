#!/usr/bin/env node

import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';

const DEFAULT_PAYLOAD = 'docs/courses/bateria-guillermo-diaz-abr-2026.course.json';
const payloadPath = resolve(process.argv[2] ?? DEFAULT_PAYLOAD);
const apiBase = (
  process.env.TDF_COURSE_API_BASE ??
  process.env.VITE_API_BASE ??
  'https://tdf-hq.fly.dev'
).replace(/\/+$/, '');
const token = process.env.TDF_ADMIN_TOKEN ?? process.env.ADMIN_TOKEN ?? '';

if (!token.trim()) {
  console.error('Set TDF_ADMIN_TOKEN to an admin bearer token before publishing a course.');
  process.exit(2);
}

let payload;
try {
  payload = JSON.parse(await readFile(payloadPath, 'utf8'));
} catch (error) {
  console.error(`Could not read course payload: ${payloadPath}`);
  console.error(error instanceof Error ? error.message : String(error));
  process.exit(2);
}

const response = await fetch(`${apiBase}/admin/courses`, {
  method: 'POST',
  headers: {
    Authorization: `Bearer ${token.trim()}`,
    'Content-Type': 'application/json',
  },
  body: JSON.stringify(payload),
});

const rawBody = await response.text();

if (!response.ok) {
  console.error(`Course upsert failed: HTTP ${response.status}`);
  if (rawBody.trim()) console.error(rawBody);
  process.exit(1);
}

let metadata = {};
try {
  metadata = rawBody.trim() ? JSON.parse(rawBody) : {};
} catch {
  metadata = {};
}

const slug = metadata.slug ?? payload.slug;
const landingUrl = metadata.landingUrl ?? payload.landingUrl ?? `https://tdf-app.pages.dev/curso/${slug}`;

console.log(`Published course: ${metadata.title ?? payload.title}`);
console.log(`Slug: ${slug}`);
console.log(`Landing: ${landingUrl}`);
