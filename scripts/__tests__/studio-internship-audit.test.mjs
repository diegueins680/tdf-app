import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';
import {
  assertReusableAuditDraft,
  isAllowedDraftApiBase,
  parseAdditionalDraftHosts,
} from '../lib/studio-audit-safety.mjs';

const repo = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const fixtureDir = path.join(repo, 'test/internships/studio-audit');
const readJson = async (name) => JSON.parse(await readFile(path.join(fixtureDir, name), 'utf8'));

test('draft preparation sends credentials only to exact approved API hosts', () => {
  assert.equal(isAllowedDraftApiBase('http://localhost:3000'), true);
  assert.equal(isAllowedDraftApiBase('https://tdf-hq-studio-audit-staging.fly.dev'), true);
  assert.equal(isAllowedDraftApiBase('https://attacker.example/staging'), false);
  assert.equal(isAllowedDraftApiBase('https://staging.attacker.example'), false);
  assert.equal(isAllowedDraftApiBase('http://tdf-hq-studio-audit-staging.fly.dev'), false);
  assert.equal(isAllowedDraftApiBase('https://user:secret@tdf-hq-studio-audit-staging.fly.dev'), false);
  const additional = parseAdditionalDraftHosts(' audit-api.example.test,second.example.test ');
  assert.deepEqual(additional, ['audit-api.example.test', 'second.example.test']);
  assert.equal(isAllowedDraftApiBase('https://audit-api.example.test', additional), true);
  assert.equal(isAllowedDraftApiBase('https://audit-api.example.test.attacker.example', additional), false);
});

test('draft preparation reuses only the exact approved assignee and plan configuration', () => {
  const draft = {
    environment: 'staging',
    durationDaysFromActivation: 14,
    expectedEffortHours: { minimum: 20, maximum: 30 },
    midpointPercent: 50,
    finalReviewAndDemonstrationRequired: true,
  };
  const task = {
    itId: 'task-1',
    itProjectId: 'project-1',
    itProposedAssignee: 129,
  };
  const plan = {
    iapProjectId: 'project-1',
    iapTaskId: 'task-1',
    iapEnvironment: 'staging',
    iapDurationDays: 14,
    iapExpectedHoursMin: 20,
    iapExpectedHoursMax: 30,
    iapMidpointPercent: 50,
    iapProposedAssignee: 129,
    iapFinalReviewRequired: true,
  };
  assert.doesNotThrow(() => assertReusableAuditDraft({ task, plan, expectedPartyId: 129, draft }));
  assert.throws(
    () => assertReusableAuditDraft({
      task,
      plan: { ...plan, iapProposedAssignee: 130 },
      expectedPartyId: 129,
      draft,
    }),
    /plan proposed assignee/,
  );
  assert.throws(
    () => assertReusableAuditDraft({
      task,
      plan: { ...plan, iapMidpointPercent: 75 },
      expectedPartyId: 129,
      draft,
    }),
    /midpoint/,
  );
});

test('web signup links the recorded account-policy version to account-specific documents', async () => {
  const loginPage = await readFile(path.join(repo, 'tdf-hq-ui/src/pages/LoginPage.tsx'), 'utf8');
  const terms = await readFile(path.join(repo, 'tdf-hq-ui/public/account/terms.html'), 'utf8');
  const privacy = await readFile(path.join(repo, 'tdf-hq-ui/public/account/privacy.html'), 'utf8');
  assert.match(loginPage, /href="\/account\/terms\.html"/);
  assert.match(loginPage, /href="\/account\/privacy\.html"/);
  assert.match(terms, /tdf-account-terms-v1/);
  assert.match(terms, /TDF Records Account Terms/);
  assert.match(privacy, /tdf-account-terms-v1/);
  assert.match(privacy, /TDF Records Account Privacy Notice/);
});

test('inventory has evidence and an explicit scope/platform decision for every feature', async () => {
  const inventory = await readJson('studio-feature-inventory.json');
  const allowedPlatforms = new Set(['web_only', 'native_mobile', 'mobile_web_fallback', 'not_available_on_mobile', 'not_applicable']);
  assert.equal(inventory.length, 130);
  assert.equal(inventory.filter((item) => item.applicable).length, 125);
  assert.equal(new Set(inventory.map((item) => item.featureId)).size, inventory.length);
  for (const item of inventory) {
    assert.ok(item.scopeClassification);
    assert.ok(item.implementationClassifications.length > 0);
    assert.ok(item.platformClassifications.length > 0);
    assert.ok(item.platformClassifications.every((classification) => allowedPlatforms.has(classification)), `${item.featureId}: invalid platform classification`);
    assert.ok(item.evidence.length > 0);
  }
});

test('test plan is complete, stable, Spanish-visible, and fits the effort window', async () => {
  const cases = await readJson('test-cases.json');
  const required = [
    'stableId', 'module', 'feature', 'userRole', 'objective', 'businessPurpose', 'preconditions',
    'requiredTestData', 'environment', 'platform', 'browserOrDevice', 'language', 'detailedSteps',
    'expectedResult', 'expectedPersistedState', 'expectedNotificationsOrSideEffects',
    'cleanupInstructions', 'criticality', 'resultStatus', 'evidenceRequirements',
  ];
  assert.equal(cases.length, 174);
  assert.equal(new Set(cases.map((item) => item.stableId)).size, cases.length);
  assert.ok(cases.filter((item) => item.exploratory).length >= 14);
  assert.ok(cases.some((item) => item.featureId === 'feedback.public-compatibility'));
  assert.ok(cases.some((item) => item.featureId === 'intern.completion-gate'));
  const hours = cases.reduce((sum, item) => sum + item.estimatedMinutes, 0) / 60;
  assert.ok(hours >= 20 && hours <= 30, `expected 20–30 hours, received ${hours}`);
  for (const item of cases) {
    for (const field of required) assert.notEqual(item[field], undefined, `${item.stableId}: ${field}`);
    assert.equal(item.environment, 'staging');
    assert.ok(['es', 'en'].includes(item.language), `${item.stableId}: unsupported language code`);
    assert.match(item.stableId, /^STU-[A-Z]{3}-\d{3}$/);
  }
});

test('draft is non-active, non-notifying, runtime-verified, and cannot masquerade as assignment', async () => {
  const draft = await readJson('draft-project.json');
  const accountDraft = await readJson('draft-stuart-account.json');
  assert.equal(draft.activationStatus, 'draft');
  assert.equal(draft.assignmentStatus, 'draft');
  assert.equal(draft.notificationsEnabled, false);
  assert.equal(draft.assignee.status, 'production-identity-verified-runtime-only');
  assert.equal(draft.assignee.displayName, 'Stewart Moreira');
  assert.equal(draft.assignee.partyId, null);
  assert.equal(draft.assignee.email, null);
  assert.equal(draft.assignee.repositoryStoresProductionIdentifiers, false);
  assert.equal(draft.assignee.runtimeExactMatchRequired, true);
  assert.equal(draft.durationDaysFromActivation, 14);
  assert.deepEqual(draft.expectedEffortHours, { minimum: 20, maximum: 30 });
  const scheduleBounds = draft.schedule.map((day) => day.hours.split('–').map(Number));
  assert.equal(scheduleBounds.reduce((sum, [minimum]) => sum + minimum, 0), 20);
  assert.equal(scheduleBounds.reduce((sum, [, maximum]) => sum + maximum, 0), 30);
  assert.match(draft.principalAssignment.descriptionMarkdown, /## 24\. Cuándo detenerte y contactar a Diego/);
  assert.match(draft.principalAssignment.descriptionMarkdown, /## Ejemplo completo de error bien escrito/);
  assert.equal(accountDraft.status, 'existing-account-verified-runtime-only');
  assert.equal(accountDraft.displayName, 'Stewart Moreira');
  assert.equal(accountDraft.spellingDifferenceVerified, true);
  assert.equal(accountDraft.email, null);
  assert.equal(accountDraft.partyId, null);
  assert.equal(accountDraft.accountActive, true);
  assert.deepEqual(accountDraft.requiredPermanentRolesForAssignment, ['Intern']);
  assert.deepEqual(accountDraft.requiredPermanentModulesForAssignment, ['Internships']);
  assert.equal(accountDraft.leastPrivilegeReviewRequiredBeforeActivation, true);
  assert.equal(accountDraft.selfServiceRoleGrantAllowed, false);
  assert.equal(accountDraft.activationRequiresSeparateApproval, true);
});

test('staging fixture catalog is fictional, deterministic, and covers every requested domain', async () => {
  const fixture = await readJson('staging-fixtures.json');
  assert.equal(fixture.fictional, true);
  assert.equal(fixture.target, 'dedicated-staging-tenant-only');
  const requiredCollections = [
    'customers', 'artists', 'engineers', 'rooms', 'resources', 'services', 'availability',
    'bookings', 'sessions', 'packages', 'quotations', 'orders', 'invoices', 'payments',
    'inventory', 'equipmentReservations', 'notifications', 'relatedScenarios',
  ];
  for (const name of requiredCollections) assert.ok(fixture[name].length > 0, name);
  assert.ok(fixture.accounts.every((account) => !account.email || account.email.endsWith('@persona.test')));
  assert.equal(fixture.externalAdapters.ddexDelivery, 'disabled-local-validation-only');
});
