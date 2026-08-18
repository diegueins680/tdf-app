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
  '/directory/profiles/{slug}/reviews',
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
  'PublicDirectoryReview',
  'PublicClassified',
  'PublicDirectoryEvent',
  'PublicDirectoryVenue',
];
const forbidden = new Set([
  'exactAddress', 'privateLatitude', 'privateLongitude', 'primaryEmail',
  'primaryPhone', 'whatsapp', 'taxId', 'credential', 'apiToken',
  'evidence', 'reviewerNotes', 'moderatorNotes', 'externalId', 'partyId',
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
  'compensationTypes', 'currencies', 'languages', 'cities',
]) {
  assert(taxonomy.required.includes(collection), `DirectoryTaxonomies requires ${collection}`);
  assert.equal(taxonomy.properties[collection].type, 'array', `${collection} is server-managed`);
}
for (const name of [
  'DirectoryPortfolioItem',
  'DirectoryProfileLink',
  'DirectoryProfessionInput',
  'DirectoryInstrumentInput',
  'DirectoryLanguageInput',
  'DirectoryServiceAreaInput',
  'DirectoryProfileUpsert',
  'ManagedDirectoryProfile',
]) {
  assert.equal(spec.components.schemas[name].additionalProperties, false, `${name} must be a closed write/private DTO`);
}
for (const collection of ['services', 'languages']) {
  assert(spec.components.schemas.PublicDirectoryProfile.required.includes(collection), `PublicDirectoryProfile requires ${collection}`);
}
for (const collection of ['professionDetails', 'instrumentDetails', 'languages', 'serviceAreas']) {
  assert(spec.components.schemas.ManagedDirectoryProfile.required.includes(collection), `ManagedDirectoryProfile requires ${collection}`);
}
assert.equal(spec.components.schemas.DirectoryProfileUpsert.properties.clearRates.default, undefined);
assert.deepEqual(spec.components.schemas.DirectoryProfileKind.enum, [
  'person', 'artist', 'band', 'project', 'organization', 'company',
  'venue', 'studio', 'agency', 'label', 'distributor', 'school',
]);
assert.equal(spec.components.schemas.DirectoryProfileUpsert.properties.profileKind.$ref, '#/components/schemas/DirectoryProfileKind');
assert.equal(spec.components.schemas.ManagedDirectoryProfile.properties.kind.$ref, '#/components/schemas/DirectoryProfileKind');
assert.equal(spec.components.schemas.PublicDirectoryProfile.properties.kind.$ref, '#/components/schemas/DirectoryProfileKind');
for (const [schemaName, fields] of Object.entries({
  DirectoryPortfolioItem: ['description', 'thumbnailUrl'],
  DirectoryProfessionInput: ['headline', 'yearsExperience', 'rateMinMinor', 'rateMaxMinor', 'currencyId'],
  DirectoryInstrumentInput: ['proficiency'],
  DirectoryLanguageInput: ['proficiency'],
  DirectoryServiceAreaInput: ['subdivisionId', 'cityId', 'metropolitanAreaId', 'sectorLabel', 'serviceRadiusKm'],
})) {
  for (const field of fields) assert.equal(spec.components.schemas[schemaName].properties[field].nullable, true, `${schemaName}.${field} must represent manager projection nulls`);
}
for (const [schemaName, field] of [['DirectoryPortfolioItem', 'url'], ['DirectoryProfileLink', 'url']]) {
  assert.equal(spec.components.schemas[schemaName].properties[field].format, 'uri-reference');
  assert.match(spec.components.schemas[schemaName].properties[field].pattern, /https\?/);
}
assert.equal(spec.components.schemas.TaxonomyItem.properties.metadata.type, 'object');
assert.equal(spec.components.schemas.TaxonomyItem.properties.minorUnits.type, 'integer');
assert.equal(spec.components.schemas.ManagedClassified.additionalProperties, false);
assert(spec.components.schemas.ManagedClassified.required.includes('authorProfileId'));
assert.equal(spec.components.schemas.DirectoryInvitation.additionalProperties, false);
for (const field of ['participantRole', 'senderProfile', 'targetProfile']) {
  assert(spec.components.schemas.DirectoryInvitation.required.includes(field));
}
assert.equal(spec.components.schemas.DirectoryReviewPage.additionalProperties, false);
assert.equal(spec.components.schemas.DirectoryReviewEligibility.additionalProperties, false);
assert(!collectPropertyNames(spec.components.schemas.DirectoryReviewEligibility).has('externalId'));
assert.equal(spec.components.schemas.DirectoryReview.additionalProperties, false);
for (const key of forbidden) assert(!collectPropertyNames(spec.components.schemas.DirectoryReview).has(key), `DirectoryReview exposes forbidden field ${key}`);
assert.deepEqual(spec.components.schemas.ReportCreate.properties.targetKind.enum.includes('review'), true);

const idempotentOperations = [
  ['/directory/profiles', 'post'],
  ['/directory/classifieds', 'post'],
  ['/directory/classifieds/{classifiedId}/applications', 'post'],
  ['/directory/invitations', 'post'],
  ['/directory/contact', 'post'],
  ['/directory/reviews', 'post'],
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
