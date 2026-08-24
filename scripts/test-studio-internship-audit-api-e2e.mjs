#!/usr/bin/env node

import assert from 'node:assert/strict';

const apiBase = (process.env.TDF_AUDIT_E2E_API_BASE ?? '').replace(/\/$/, '');
const password = process.env.TDF_AUDIT_E2E_PASSWORD ?? '';
const otherInternEmail = process.env.TDF_AUDIT_E2E_OTHER_INTERN_EMAIL ?? '';

assert.match(apiBase, /^http:\/\/(127\.0\.0\.1|localhost):\d+$/, 'E2E API must be an explicit loopback HTTP endpoint');
assert.ok(password.length >= 16, 'A runtime-only synthetic-persona password is required');

async function request(path, { token, method = 'GET', json, body, expected = 200 } = {}) {
  const headers = {};
  if (token) headers.Authorization = `Bearer ${token}`;
  if (json !== undefined) headers['Content-Type'] = 'application/json';
  const response = await fetch(`${apiBase}${path}`, {
    method,
    headers,
    body: json === undefined ? body : JSON.stringify(json),
  });
  const raw = await response.text();
  if (response.status !== expected) {
    throw new Error(`${method} ${path}: expected ${expected}, received ${response.status}: ${raw.slice(0, 500)}`);
  }
  if (!raw) return undefined;
  const contentType = response.headers.get('content-type') ?? '';
  return contentType.includes('json') ? JSON.parse(raw) : raw;
}

async function login(email) {
  const response = await request('/login', { method: 'POST', json: { username: email, password } });
  assert.ok(response.token, `Login did not return a token for ${email}`);
  return response;
}

const admin = await login('per-13.fernando@persona.test');
const intern = await login('per-11.martina@persona.test');
const otherIntern = otherInternEmail ? await login(otherInternEmail) : null;
assert.ok(admin.roles.includes('Studio Manager'));
assert.ok(intern.roles.includes('Intern'));

const catalogs = await request('/catalogs/batch?code=feedback-categories&code=feedback-severities&locale=es&page=1&pageSize=100');
const category = catalogs.catalogs.find((entry) => entry.catalog.code === 'feedback-categories')?.items.find((item) => item.active && item.workflowState === 'published');
const severity = catalogs.catalogs.find((entry) => entry.catalog.code === 'feedback-severities')?.items.find((item) => item.active && item.workflowState === 'published');
assert.ok(category?.id && severity?.id, 'Published feedback catalogs are required');

const publicFeedback = new FormData();
publicFeedback.append('title', 'E2E — compatibilidad del feedback público');
publicFeedback.append('description', 'Registro sintético para comprobar que el formulario público existente sigue legible.');
publicFeedback.append('categoryId', category.id);
publicFeedback.append('severityId', severity.id);
publicFeedback.append('contactEmail', 'audit.public@persona.test');
publicFeedback.append('consent', 'true');
await request('/feedback', { method: 'POST', body: publicFeedback, expected: 200 });
const legacyFeedback = await request('/feedback/internal/legacy', { token: admin.token });
assert.ok(legacyFeedback.some((entry) => entry.lfdTitle === 'E2E — compatibilidad del feedback público'));

const project = await request('/internships/projects', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    ipcTitle: 'E2E — Auditoría funcional del estudio',
    ipcDescription: 'Proyecto sintético y desechable para verificar el flujo interno.',
    ipcStatus: 'active',
    ipcActivationStatus: 'draft',
  },
});
assert.equal(project.ipActivationStatus, 'draft');

const task = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: project.ipId,
    itcTitle: 'E2E — Asignación principal',
    itcDescription: 'Usar únicamente datos persona.test y transporte de prueba.',
    itcProposedAssignee: intern.partyId,
    itcActivationStatus: 'draft',
  },
});
assert.equal(task.itActivationStatus, 'draft');
assert.equal(task.itAssignedTo ?? null, null);

const plan = await request('/internships/audit-plans', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    iapcProjectId: project.ipId,
    iapcTaskId: task.itId,
    iapcEnvironment: 'staging',
    iapcDurationDays: 14,
    iapcExpectedHoursMin: 20,
    iapcExpectedHoursMax: 30,
    iapcMidpointPercent: 50,
    iapcProposedAssignee: intern.partyId,
    iapcFinalReviewRequired: true,
  },
});
assert.equal(plan.iapStatus, 'draft');

const testCase = await request(`/internships/audit-plans/${plan.iapId}/cases`, {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itccStableId: 'STU-E2E-001',
    itccModuleName: 'Prácticas y feedback',
    itccFeatureName: 'Trazabilidad de un caso fallido y retest',
    itccUserRole: 'Intern',
    itccObjective: 'Verificar el ciclo completo con cuentas sintéticas.',
    itccBusinessPurpose: 'Evitar pérdida de resultados y exposición entre pasantes.',
    itccPreconditions: 'API local, base desechable y proveedores deshabilitados.',
    itccRequiredTestData: 'PER-11, PER-13 y catálogos ficticios.',
    itccEnvironment: 'staging',
    itccPlatform: 'web',
    itccBrowserOrDevice: 'API contract test',
    itccLanguage: 'es',
    itccDetailedSteps: 'Activar, fallar, reportar, aclarar, retestear y cerrar.',
    itccExpectedResult: 'Historial completo y permisos aplicados.',
    itccExpectedPersistedState: 'Dos ejecuciones y un reporte cerrado.',
    itccExpectedSideEffects: 'Sólo notificaciones internas y outbox de prueba.',
    itccCleanupInstructions: 'Eliminar toda la base de datos desechable.',
    itccCriticality: 'critical',
    itccEvidenceRequirement: 'strong',
    itccApplicable: true,
    itccSortOrder: 1,
  },
});

await request(`/internships/audit-plans/${plan.iapId}`, { token: intern.token, expected: 404 });
await request(`/internships/tasks/${task.itId}`, {
  token: intern.token,
  method: 'PATCH',
  expected: 403,
  json: { ituTitle: 'Cambio no permitido' },
});

const activated = await request(`/internships/audit-plans/${plan.iapId}/activate`, {
  token: admin.token,
  method: 'POST',
});
assert.equal(activated.iapStatus, 'active');
const internPlan = await request(`/internships/audit-plans/${plan.iapId}`, { token: intern.token });
assert.equal(internPlan.iapProposedAssignee, intern.partyId);
const internTasks = await request('/internships/tasks', { token: intern.token });
assert.ok(internTasks.some((candidate) => candidate.itId === task.itId));
if (otherIntern) {
  const otherInternTasks = await request('/internships/tasks', { token: otherIntern.token });
  assert.ok(otherInternTasks.every((candidate) => candidate.itId !== task.itId));
}

await request(`/internships/tasks/${task.itId}`, {
  token: intern.token,
  method: 'PATCH',
  expected: 403,
  json: { ituTitle: 'Cambio no permitido' },
});

const failedExecution = await request(`/internships/test-cases/${testCase.itcId}/executions`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    itecStatus: 'failed',
    itecActualResult: 'El guardado duplicó el registro ficticio.',
    itecPersistedStateObserved: 'Se observaron dos filas ficticias.',
    itecSideEffectsObserved: 'No se emitió ninguna llamada externa.',
    itecEvidenceSummary: 'EVIDENCIA-E2E-FAILED-001',
  },
});

const reportCreate = {
  ifcTitle: 'El guardado duplica el registro ficticio',
  ifcDescription: 'Al guardar una vez se observaron dos filas dentro de la base desechable.',
  ifcCategoryId: category.id,
  ifcProposedSeverityId: severity.id,
  ifcReportType: 'error',
  ifcModuleName: 'Prácticas y feedback',
  ifcFeatureName: 'Trazabilidad interna',
  ifcEnvironment: 'staging',
  ifcUrlOrScreen: '/feedback/interno/nuevo',
  ifcPlatform: 'web',
  ifcDevice: 'API contract test',
  ifcBrowser: 'Node fetch',
  ifcLanguage: 'es',
  ifcAccountRole: 'Intern',
  ifcReproductionSteps: '1. Abrir el caso. 2. Guardar una vez. 3. Recargar.',
  ifcExpectedResult: 'Una sola fila ficticia.',
  ifcActualResult: 'Dos filas ficticias.',
  ifcFrequency: 'Siempre en esta prueba sintética',
  ifcTestCaseId: testCase.itcId,
  ifcTestExecutionId: failedExecution.itexId,
  ifcInternshipProjectId: project.ipId,
  ifcInternshipTaskId: task.itId,
  ifcBlocking: false,
  ifcVideoLinks: 'https://evidence.example.test/studio-audit-e2e',
};
const draftReport = await request('/feedback/internal', {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: reportCreate,
});
const reportId = draftReport.ifrSummary.ifsId;
assert.equal(draftReport.ifrSummary.ifsState, 'draft');

const editedReport = await request(`/feedback/internal/${reportId}`, {
  token: intern.token,
  method: 'PATCH',
  json: { ifuDescription: `${reportCreate.ifcDescription} Borrador revisado.` },
});
assert.match(editedReport.ifrDescription, /Borrador revisado/);
await request(`/feedback/internal/${reportId}`, {
  token: intern.token,
  method: 'PATCH',
  expected: 403,
  json: { ifuPriority: 'urgent' },
});

const evidenceForm = new FormData();
evidenceForm.append('attachment', new Blob(['EVIDENCIA FICTICIA E2E'], { type: 'text/plain' }), 'STU-E2E-001.txt');
evidenceForm.append('caption', 'Documento ficticio pequeño');
const evidence = await request(`/feedback/internal/${reportId}/evidence`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  body: evidenceForm,
});
const evidenceBody = await request(`/feedback/internal/${reportId}/evidence/${evidence.ifeId}/file`, { token: intern.token });
assert.equal(evidenceBody, 'EVIDENCIA FICTICIA E2E');

const receivedReport = await request(`/feedback/internal/${reportId}/submit`, { token: intern.token, method: 'POST' });
assert.equal(receivedReport.ifrSummary.ifsState, 'received');
if (otherIntern) {
  await request(`/feedback/internal/${reportId}`, { token: otherIntern.token, expected: 404 });
}

const similarDraft = await request('/feedback/internal', {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: { ...reportCreate, ifcTitle: 'El guardado duplica registro ficticio' },
});
assert.notEqual(similarDraft.ifrSummary.ifsId, reportId);
assert.ok(similarDraft.ifrPotentialDuplicates.some((candidate) => candidate.ifsId === reportId));

const triaged = await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  json: {
    ifuState: 'confirmed',
    ifuAuthoritativeSeverityId: severity.id,
    ifuPriority: 'high',
    ifuAssignedTo: admin.partyId,
    ifuGithubIssueUrl: 'https://github.com/diegueins680/tdf-app/issues/999999999',
  },
});
assert.equal(triaged.ifrSummary.ifsPriority, 'high');
assert.equal(triaged.ifrAssignedTo, admin.partyId);

await request(`/feedback/internal/${reportId}/comments`, {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: { ifccKind: 'information_request', ifccBody: 'Confirma cuántas filas ficticias observaste.' },
});
const needsInfo = await request(`/feedback/internal/${reportId}`, { token: intern.token });
assert.equal(needsInfo.ifrSummary.ifsState, 'needs_information');
await request(`/feedback/internal/${reportId}`, {
  token: intern.token,
  method: 'PATCH',
  json: { ifuActualResult: 'Se observaron exactamente dos filas ficticias.' },
});
await request(`/feedback/internal/${reportId}/comments`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: { ifccKind: 'information_response', ifccBody: 'Confirmado: fueron dos filas ficticias.' },
});

for (const nextState of ['confirmed', 'in_progress', 'ready_for_retest']) {
  await request(`/feedback/internal/${reportId}`, {
    token: admin.token,
    method: 'PATCH',
    json: { ifuState: nextState },
  });
}

const verifiedExecution = await request(`/internships/test-cases/${testCase.itcId}/executions`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    itecStatus: 'verified',
    itecActualResult: 'El retest dejó una sola fila ficticia.',
    itecPersistedStateObserved: 'Una fila ficticia.',
    itecSideEffectsObserved: 'Sin proveedores externos.',
    itecEvidenceSummary: 'EVIDENCIA-E2E-RETEST-001',
  },
});
await request(`/feedback/internal/${reportId}/retests`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    ifrcExecutionId: verifiedExecution.itexId,
    ifrcResult: 'passed',
    ifrcNotes: 'Repetí los pasos y quedó una sola fila ficticia.',
    ifrcEvidenceSummary: 'EVIDENCIA-E2E-RETEST-001',
  },
});
for (const update of [
  { ifuState: 'verified', ifuResolution: 'Corregido y comprobado en base desechable.' },
  { ifuState: 'closed', ifuClosureReason: 'Retest aprobado con evidencia ficticia.' },
]) {
  await request(`/feedback/internal/${reportId}`, { token: admin.token, method: 'PATCH', json: update });
}

await request(`/internships/audit-plans/${plan.iapId}/daily-summaries`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    idscWorkDate: new Date().toISOString().slice(0, 10),
    idscMinutesWorked: 90,
    idscModulesTested: 'Prácticas y feedback',
    idscCasesCompleted: 1,
    idscReportsCreated: 2,
    idscBlockers: null,
    idscNextStep: 'Presentar el resumen final sintético.',
  },
});
await request(`/internships/audit-plans/${plan.iapId}/final-summary`, {
  token: intern.token,
  method: 'PUT',
  json: {
    ifsuConclusions: 'El flujo conservó permisos, evidencia, auditoría, aclaración y retest con datos ficticios.',
    ifsuSubmit: true,
  },
});

const completionPreview = await request(`/internships/audit-plans/${plan.iapId}`, { token: admin.token });
assert.equal(completionPreview.iapCalculatedProgress, 100);
assert.equal(completionPreview.iapCanComplete, true);
const completed = await request(`/internships/audit-plans/${plan.iapId}`, {
  token: admin.token,
  method: 'PATCH',
  json: { iapuStatus: 'completed' },
});
assert.equal(completed.iapStatus, 'completed');

const executions = await request(`/internships/test-cases/${testCase.itcId}/executions`, { token: admin.token });
assert.equal(executions.length, 2);
const finalReport = await request(`/feedback/internal/${reportId}`, { token: admin.token });
assert.equal(finalReport.ifrSummary.ifsState, 'closed');
assert.ok(finalReport.ifrHistory.length >= 10);
assert.equal(finalReport.ifrRetests.length, 1);
const csv = await request('/feedback/internal/export.csv', { token: admin.token });
assert.match(csv, /El guardado duplica el registro ficticio/);
const exported = await request('/feedback/internal/export.json', { token: admin.token });
assert.ok(exported.some((entry) => entry.ifsId === reportId));

console.log(JSON.stringify({
  result: 'passed',
  personas: { admin: 'PER-13', intern: 'PER-11', otherInternIsolation: Boolean(otherIntern) },
  draftActivation: 'verified',
  executionHistoryCount: executions.length,
  reportState: finalReport.ifrSummary.ifsState,
  reportHistoryCount: finalReport.ifrHistory.length,
  testTransportExpected: true,
}, null, 2));
