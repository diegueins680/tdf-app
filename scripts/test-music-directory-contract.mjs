#!/usr/bin/env node

import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import yaml from 'yaml';

const { parse } = yaml;

const spec = parse(readFileSync(new URL('../tdf-hq/docs/openapi/directory.yaml', import.meta.url), 'utf8'));
const sitemap = readFileSync(new URL('../tdf-hq-ui/public/sitemap.xml', import.meta.url), 'utf8');
const robots = readFileSync(new URL('../tdf-hq-ui/public/robots.txt', import.meta.url), 'utf8');
assert.match(sitemap, /<loc>https:\/\/tdf-app\.pages\.dev\/buscar<\/loc>/);
assert.match(robots, /Allow: \/directorio\//);
assert.match(robots, /Disallow: \/admin\//);
const publicPaths = [
  '/directory/search',
  '/directory/suggestions',
  '/directory/taxonomies',
  '/directory/profiles/{slug}',
  '/directory/classifieds/{slug}',
  '/directory/events/{eventId}',
  '/directory/venues/{venueId}',
];
for (const path of publicPaths) {
  assert.deepEqual(spec.paths[path].get.security, [], `${path} must explicitly override bearer auth`);
}

const publicSchemas = [
  'PublicLocation',
  'DirectorySearchItem',
  'PublicDirectoryProfile',
  'PublicClassified',
  'PublicDirectoryEvent',
  'PublicDirectoryVenue',
];
const forbidden = new Set([
  'exactAddress', 'privateLatitude', 'privateLongitude', 'primaryEmail',
  'primaryPhone', 'whatsapp', 'taxId', 'credential', 'apiToken',
  'evidence', 'reviewerNotes', 'moderatorNotes',
]);
const collectPropertyNames = (schema, names = new Set()) => {
  if (!schema || typeof schema !== 'object') return names;
  for (const key of Object.keys(schema.properties ?? {})) names.add(key);
  for (const value of Object.values(schema)) collectPropertyNames(value, names);
  return names;
};
for (const name of publicSchemas) {
  const schema = spec.components.schemas[name];
  assert.equal(schema.additionalProperties, false, `${name} must be a closed public DTO`);
  const names = collectPropertyNames(schema);
  for (const key of forbidden) assert(!names.has(key), `${name} exposes forbidden field ${key}`);
}

const response = spec.components.schemas.DirectorySearchResponse;
assert(response.required.includes('items'));
assert(response.required.includes('sponsoredItems'));
assert.notEqual(response.properties.items, response.properties.sponsoredItems);
assert.equal(spec.components.schemas.DirectorySearchItem.properties.sponsored.type, 'boolean');
assert.equal(spec.components.parameters.IdempotencyKey.required, true);

const taxonomy = spec.components.schemas.DirectoryTaxonomies;
for (const collection of [
  'professions', 'instruments', 'genres', 'serviceOfferings', 'classifiedCategories',
  'compensationTypes', 'currencies', 'cities',
]) {
  assert(taxonomy.required.includes(collection), `DirectoryTaxonomies requires ${collection}`);
  assert.equal(taxonomy.properties[collection].type, 'array', `${collection} is server-managed`);
}
assert.equal(spec.components.schemas.TaxonomyItem.properties.metadata.type, 'object');
assert.equal(spec.components.schemas.TaxonomyItem.properties.minorUnits.type, 'integer');

const idempotentOperations = [
  ['/directory/profiles', 'post'],
  ['/directory/classifieds', 'post'],
  ['/directory/classifieds/{classifiedId}/applications', 'post'],
  ['/directory/invitations', 'post'],
  ['/directory/contact', 'post'],
  ['/directory/saved-searches', 'post'],
  ['/directory/claims', 'post'],
  ['/directory/verifications', 'post'],
  ['/directory/reports', 'post'],
  ['/directory/admin/moderation/{caseId}/decisions', 'post'],
  ['/directory/admin/merges', 'post'],
];
for (const [path, method] of idempotentOperations) {
  const parameters = spec.paths[path][method].parameters ?? [];
  assert(parameters.some((parameter) => parameter.$ref === '#/components/parameters/IdempotencyKey'), `${method.toUpperCase()} ${path} requires Idempotency-Key`);
}

console.log('Music directory OpenAPI privacy, public-auth, taxonomy, sponsorship, and idempotency contracts passed.');
