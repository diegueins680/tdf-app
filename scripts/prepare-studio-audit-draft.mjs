#!/usr/bin/env node

/**
 * Creates the reviewed project/task/plan as an inactive draft through TDF App.
 * The default mode is preview. Creation requires an exact intern identity match and
 * an explicit runtime confirmation; activation is intentionally not implemented here.
 */
import { readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const draft = JSON.parse(await readFile(path.join(root, 'test/internships/studio-audit/draft-project.json'), 'utf8'));
const cases = JSON.parse(await readFile(path.join(root, 'test/internships/studio-audit/test-cases.json'), 'utf8'));
const mode = process.argv[2] ?? 'preview';

if (mode === 'preview') {
  console.log(JSON.stringify({ ...draft, testCases: { count: cases.length, first: cases[0]?.stableId, last: cases.at(-1)?.stableId } }, null, 2));
  process.exit(0);
}
if (mode !== 'create') throw new Error('Usage: prepare-studio-audit-draft.mjs [preview|create]');

const apiBase = (process.env.TDF_STUDIO_AUDIT_API_BASE ?? '').replace(/\/$/, '');
const token = process.env.TDF_STUDIO_AUDIT_ADMIN_TOKEN ?? '';
const expectedPartyId = Number(process.env.TDF_STUDIO_AUDIT_STUART_PARTY_ID ?? '');
const expectedEmail = (process.env.TDF_STUDIO_AUDIT_STUART_EMAIL ?? '').trim().toLowerCase();
const confirmation = process.env.TDF_STUDIO_AUDIT_DRAFT_CONFIRM ?? '';

if (!apiBase || !token) throw new Error('Draft creation requires API base and an authorized runtime-only admin token.');
const host = new URL(apiBase).hostname.toLowerCase();
const safeHost = ['localhost', '127.0.0.1', '::1'].includes(host) || host.includes('staging') || host.includes('test');
if (!safeHost || /prod|production/.test(apiBase.toLowerCase())) {
  throw new Error('Refusing to create a draft outside an explicit local/test/staging API.');
}
if (!Number.isSafeInteger(expectedPartyId) || expectedPartyId <= 0 || !expectedEmail) {
  throw new Error('Exact verified Stewart party ID and email are required; identity must never be guessed.');
}
if (confirmation !== 'CREATE_INACTIVE_DRAFT_WITHOUT_NOTIFICATION') {
  throw new Error('Set TDF_STUDIO_AUDIT_DRAFT_CONFIRM=CREATE_INACTIVE_DRAFT_WITHOUT_NOTIFICATION after approval.');
}

async function request(endpoint, init = {}) {
  const response = await fetch(`${apiBase}${endpoint}`, {
    ...init,
    headers: { Authorization: `Bearer ${token}`, 'Content-Type': 'application/json', ...(init.headers ?? {}) },
  });
  const body = await response.text();
  if (!response.ok) throw new Error(`${init.method ?? 'GET'} ${endpoint} failed (${response.status}): ${body}`);
  return body ? JSON.parse(body) : undefined;
}

const interns = await request('/internships/interns');
const candidates = interns.filter((intern) =>
  intern.isPartyId === expectedPartyId
    && (intern.isEmail ?? '').trim().toLowerCase() === expectedEmail
    && intern.isRoles.includes('Intern'));
if (candidates.length !== 1) {
  throw new Error(`Expected one exact active Intern identity, found ${candidates.length}. No draft was created.`);
}

const projects = await request('/internships/projects');
let project = projects.find((item) => item.ipTitle === draft.title);
if (project && project.ipActivationStatus !== 'draft') {
  throw new Error('A matching non-draft project already exists; refusing to reuse or duplicate it.');
}
if (!project) {
  project = await request('/internships/projects', {
    method: 'POST',
    body: JSON.stringify({
      ipcTitle: draft.title,
      ipcDescription: draft.principalAssignment.descriptionMarkdown,
      ipcStatus: 'active',
      ipcActivationStatus: 'draft',
    }),
  });
}

const taskTitle = draft.principalAssignment.title;
const tasks = await request('/internships/tasks');
let task = tasks.find((item) => item.itProjectId === project.ipId && item.itTitle === taskTitle);
if (task && task.itActivationStatus !== 'draft') {
  throw new Error('A matching non-draft task already exists; refusing to reuse or duplicate it.');
}
if (!task) {
  task = await request('/internships/tasks', {
    method: 'POST',
    body: JSON.stringify({
      itcProjectId: project.ipId,
      itcTitle: taskTitle,
      itcDescription: draft.principalAssignment.descriptionMarkdown,
      itcProposedAssignee: expectedPartyId,
      itcActivationStatus: 'draft',
    }),
  });
}

const plans = await request('/internships/audit-plans');
let plan = plans.find((item) => item.iapTaskId === task.itId);
if (plan && plan.iapStatus !== 'draft') {
  throw new Error('A matching non-draft audit plan already exists; refusing to modify it.');
}
if (!plan) {
  plan = await request('/internships/audit-plans', {
    method: 'POST',
    body: JSON.stringify({
      iapcProjectId: project.ipId,
      iapcTaskId: task.itId,
      iapcEnvironment: draft.environment,
      iapcDurationDays: draft.durationDaysFromActivation,
      iapcExpectedHoursMin: draft.expectedEffortHours.minimum,
      iapcExpectedHoursMax: draft.expectedEffortHours.maximum,
      iapcMidpointPercent: draft.midpointPercent,
      iapcProposedAssignee: expectedPartyId,
      iapcFinalReviewRequired: draft.finalReviewAndDemonstrationRequired,
    }),
  });
}

const existingCases = await request(`/internships/audit-plans/${encodeURIComponent(plan.iapId)}/cases`);
const existingIds = new Set(existingCases.map((item) => item.itcStableId));
for (let index = 0; index < cases.length; index += 1) {
  const item = cases[index];
  if (existingIds.has(item.stableId)) continue;
  await request(`/internships/audit-plans/${encodeURIComponent(plan.iapId)}/cases`, {
    method: 'POST',
    body: JSON.stringify({
      itccStableId: item.stableId,
      itccModuleName: item.module,
      itccFeatureName: item.feature,
      itccUserRole: item.userRole,
      itccObjective: item.objective,
      itccBusinessPurpose: item.businessPurpose,
      itccPreconditions: item.preconditions,
      itccRequiredTestData: item.requiredTestData,
      itccEnvironment: item.environment,
      itccPlatform: item.platform,
      itccBrowserOrDevice: item.browserOrDevice,
      itccLanguage: item.language,
      itccDetailedSteps: item.detailedSteps,
      itccExpectedResult: item.expectedResult,
      itccExpectedPersistedState: item.expectedPersistedState,
      itccExpectedSideEffects: item.expectedNotificationsOrSideEffects,
      itccCleanupInstructions: item.cleanupInstructions,
      itccCriticality: item.criticality,
      itccEvidenceRequirement: item.evidenceRequirements,
      itccExploratoryCharter: item.exploratoryCharter ?? null,
      itccApplicable: true,
      itccSortOrder: index + 1,
    }),
  });
}

const preview = await request(`/internships/audit-plans/${encodeURIComponent(plan.iapId)}`);
if (preview.iapStatus !== 'draft') throw new Error('Safety assertion failed: created plan is not a draft.');
console.log(JSON.stringify({
  result: 'inactive-draft-created',
  notificationsSent: false,
  assigneeVerified: candidates[0],
  projectId: project.ipId,
  taskId: task.itId,
  planId: plan.iapId,
  testCaseCount: preview.iapCaseCount,
}, null, 2));
