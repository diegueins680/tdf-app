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

async function requestStatus(path, { token, method = 'GET', json } = {}) {
  const headers = {};
  if (token) headers.Authorization = `Bearer ${token}`;
  if (json !== undefined) headers['Content-Type'] = 'application/json';
  const response = await fetch(`${apiBase}${path}`, {
    method,
    headers,
    body: json === undefined ? undefined : JSON.stringify(json),
  });
  return response.status;
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
const feedbackCategories = catalogs.catalogs.find((entry) => entry.catalog.code === 'feedback-categories')?.items ?? [];
const category = feedbackCategories.find((item) => item.code === 'bug' && item.active && item.workflowState === 'published');
const ideaCategory = feedbackCategories.find((item) => item.code === 'idea' && item.active && item.workflowState === 'published');
const severity = catalogs.catalogs.find((entry) => entry.catalog.code === 'feedback-severities')?.items.find((item) => item.active && item.workflowState === 'published');
assert.ok(category?.id && ideaCategory?.id && severity?.id, 'Published feedback catalogs are required');

const createAuditCase = (planId, stableId, overrides = {}, expected = 201) => request(`/internships/audit-plans/${planId}/cases`, {
  token: admin.token,
  method: 'POST',
  expected,
  json: {
    itccStableId: stableId,
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
    ...overrides,
  },
});

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

const activeProject = await request('/internships/projects', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    ipcTitle: 'E2E — Proyecto que no debe reiniciarse',
    ipcDescription: 'Fixture para rechazar la conversión destructiva de trabajo activo.',
    ipcStatus: 'active',
    ipcActivationStatus: 'draft',
  },
});
const activeTask = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: activeProject.ipId,
    itcTitle: 'E2E — Tarea activa que conserva su estado',
    itcDescription: 'No se debe convertir en auditoría ni reiniciar.',
    itcProposedAssignee: intern.partyId,
    itcActivationStatus: 'active',
  },
});
await request('/internships/audit-plans', {
  token: admin.token,
  method: 'POST',
  expected: 409,
  json: {
    iapcProjectId: activeProject.ipId,
    iapcTaskId: activeTask.itId,
    iapcEnvironment: 'staging',
    iapcProposedAssignee: intern.partyId,
  },
});

const ownershipRaceProject = await request('/internships/projects', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    ipcTitle: 'E2E — Serialización de propiedad de tarea de auditoría',
    ipcDescription: 'Crear el plan y cambiar la tarea no pueden aprobarse sobre estados distintos.',
    ipcStatus: 'active',
    ipcActivationStatus: 'draft',
  },
});
const ownershipRaceTask = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: ownershipRaceProject.ipId,
    itcTitle: 'E2E — Carrera entre creación del plan y cambio de tarea',
    itcDescription: 'La misma fila de tarea serializa ambas escrituras.',
    itcProposedAssignee: intern.partyId,
    itcActivationStatus: 'draft',
  },
});
const [ownershipPlanStatus, ownershipTaskUpdateStatus] = await Promise.all([
  requestStatus('/internships/audit-plans', {
    token: admin.token,
    method: 'POST',
    json: {
      iapcProjectId: ownershipRaceProject.ipId,
      iapcTaskId: ownershipRaceTask.itId,
      iapcEnvironment: 'staging',
      iapcProposedAssignee: intern.partyId,
    },
  }),
  requestStatus(`/internships/tasks/${ownershipRaceTask.itId}`, {
    token: admin.token,
    method: 'PATCH',
    json: { ituProgress: 1 },
  }),
]);
assert.equal([ownershipPlanStatus, ownershipTaskUpdateStatus].filter((status) => status === 409).length, 1);
const ownershipRaceTasks = await request('/internships/tasks', { token: admin.token });
const ownershipRaceTaskAfter = ownershipRaceTasks.find((candidate) => candidate.itId === ownershipRaceTask.itId);
const ownershipRacePlans = await request('/internships/audit-plans', { token: admin.token });
const ownershipRacePlanAfter = ownershipRacePlans.find((candidate) => candidate.iapTaskId === ownershipRaceTask.itId);
if (ownershipPlanStatus === 201) {
  assert.equal(ownershipTaskUpdateStatus, 409);
  assert.equal(ownershipRaceTaskAfter?.itProgress, 0);
  assert.ok(ownershipRacePlanAfter);
} else {
  assert.equal(ownershipPlanStatus, 409);
  assert.equal(ownershipTaskUpdateStatus, 200);
  assert.equal(ownershipRaceTaskAfter?.itProgress, 1);
  assert.equal(ownershipRacePlanAfter, undefined);
}

const cancellableTask = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: activeProject.ipId,
    itcTitle: 'E2E — Auditoría cancelable con trabajo hermano activo',
    itcDescription: 'Cancelar esta auditoría no debe cancelar el proyecto compartido.',
    itcProposedAssignee: intern.partyId,
    itcActivationStatus: 'draft',
  },
});
const cancellablePlan = await request('/internships/audit-plans', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    iapcProjectId: activeProject.ipId,
    iapcTaskId: cancellableTask.itId,
    iapcEnvironment: 'staging',
    iapcProposedAssignee: intern.partyId,
  },
});
await request(`/internships/audit-plans/${cancellablePlan.iapId}`, {
  token: admin.token,
  method: 'PATCH',
  json: { iapuStatus: 'cancelled' },
});
await request(`/internships/audit-plans/${cancellablePlan.iapId}/activate`, {
  token: admin.token,
  method: 'POST',
  expected: 409,
});
const tasksAfterRejectedActivation = await request('/internships/tasks', { token: admin.token });
const taskAfterRejectedActivation = tasksAfterRejectedActivation.find((candidate) => candidate.itId === cancellableTask.itId);
assert.equal(taskAfterRejectedActivation?.itStatus, 'cancelled');
assert.equal(taskAfterRejectedActivation?.itActivationStatus, 'draft');
assert.equal(taskAfterRejectedActivation?.itAssignedTo ?? null, null);
const projectsAfterCancellation = await request('/internships/projects', { token: admin.token });
assert.equal(
  projectsAfterCancellation.find((candidate) => candidate.ipId === activeProject.ipId)?.ipStatus,
  'active',
);

const mixedTerminalProject = await request('/internships/projects', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    ipcTitle: 'E2E — Proyecto con resultados terminales mixtos',
    ipcDescription: 'Una tarea completada y otra cancelada deben cerrar el proyecto como completado.',
    ipcStatus: 'active',
    ipcActivationStatus: 'draft',
  },
});
const completedSiblingTask = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: mixedTerminalProject.ipId,
    itcTitle: 'E2E — Trabajo hermano completado',
    itcDescription: 'Este resultado debe tener prioridad sobre una cancelación hermana.',
    itcProposedAssignee: intern.partyId,
    itcActivationStatus: 'active',
  },
});
await request(`/internships/tasks/${completedSiblingTask.itId}`, {
  token: admin.token,
  method: 'PATCH',
  json: { ituStatus: 'done', ituProgress: 100 },
});
const mixedCancelledTask = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: mixedTerminalProject.ipId,
    itcTitle: 'E2E — Auditoría hermana cancelada',
    itcDescription: 'Cancelar la última tarea no debe ocultar el trabajo hermano completado.',
    itcProposedAssignee: intern.partyId,
    itcActivationStatus: 'draft',
  },
});
const mixedCancelledPlan = await request('/internships/audit-plans', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    iapcProjectId: mixedTerminalProject.ipId,
    iapcTaskId: mixedCancelledTask.itId,
    iapcEnvironment: 'staging',
    iapcProposedAssignee: intern.partyId,
  },
});
await request(`/internships/audit-plans/${mixedCancelledPlan.iapId}`, {
  token: admin.token,
  method: 'PATCH',
  json: { iapuStatus: 'cancelled' },
});
const projectsAfterMixedTerminalOutcomes = await request('/internships/projects', { token: admin.token });
assert.equal(
  projectsAfterMixedTerminalOutcomes.find((candidate) => candidate.ipId === mixedTerminalProject.ipId)?.ipStatus,
  'completed',
);

const racingTask = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: activeProject.ipId,
    itcTitle: 'E2E — Carrera entre activación y cancelación',
    itcDescription: 'Sólo la transición ganadora puede producir sus efectos laterales.',
    itcProposedAssignee: intern.partyId,
    itcActivationStatus: 'draft',
  },
});
const racingPlan = await request('/internships/audit-plans', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    iapcProjectId: activeProject.ipId,
    iapcTaskId: racingTask.itId,
    iapcEnvironment: 'staging',
    iapcProposedAssignee: intern.partyId,
  },
});
await createAuditCase(racingPlan.iapId, 'STU-RACE-001', {
  itccFeatureName: 'Serialización de activación y cancelación',
  itccCriticality: 'low',
  itccEvidenceRequirement: 'light',
});
const [cancellationStatus, activationStatus] = await Promise.all([
  requestStatus(`/internships/audit-plans/${racingPlan.iapId}`, {
    token: admin.token,
    method: 'PATCH',
    json: { iapuStatus: 'cancelled' },
  }),
  requestStatus(`/internships/audit-plans/${racingPlan.iapId}/activate`, {
    token: admin.token,
    method: 'POST',
  }),
]);
assert.ok([cancellationStatus, activationStatus].includes(200));
assert.ok([200, 409].includes(cancellationStatus));
assert.ok([200, 409].includes(activationStatus));
const racingPlanAfterTransitions = await request(`/internships/audit-plans/${racingPlan.iapId}`, {
  token: admin.token,
});
const tasksAfterRace = await request('/internships/tasks', { token: admin.token });
const racingTaskAfterTransitions = tasksAfterRace.find((candidate) => candidate.itId === racingTask.itId);
if (racingPlanAfterTransitions.iapStatus === 'active') {
  assert.equal(cancellationStatus, 409);
  assert.equal(activationStatus, 200);
  assert.equal(racingTaskAfterTransitions?.itActivationStatus, 'active');
  assert.equal(racingTaskAfterTransitions?.itAssignedTo, intern.partyId);
} else {
  assert.equal(racingPlanAfterTransitions.iapStatus, 'cancelled');
  assert.equal(cancellationStatus, 200);
  assert.equal(racingTaskAfterTransitions?.itStatus, 'cancelled');
  if (activationStatus === 409) {
    assert.equal(racingTaskAfterTransitions?.itActivationStatus, 'draft');
    assert.equal(racingTaskAfterTransitions?.itAssignedTo ?? null, null);
  } else {
    assert.equal(racingTaskAfterTransitions?.itActivationStatus, 'active');
    assert.equal(racingTaskAfterTransitions?.itAssignedTo, intern.partyId);
  }
}

const nonApplicableProject = await request('/internships/projects', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    ipcTitle: 'E2E — Proyecto sin casos aplicables',
    ipcDescription: 'Fixture aislado del resultado de la carrera de activación y cancelación.',
    ipcStatus: 'active',
    ipcActivationStatus: 'draft',
  },
});
const nonApplicableTask = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: nonApplicableProject.ipId,
    itcTitle: 'E2E — Plan sin casos aplicables',
    itcDescription: 'Un plan con sólo casos no aplicables debe permanecer en borrador.',
    itcProposedAssignee: intern.partyId,
    itcActivationStatus: 'draft',
  },
});
const nonApplicablePlan = await request('/internships/audit-plans', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    iapcProjectId: nonApplicableProject.ipId,
    iapcTaskId: nonApplicableTask.itId,
    iapcEnvironment: 'staging',
    iapcProposedAssignee: intern.partyId,
  },
});
await createAuditCase(nonApplicablePlan.iapId, 'STU-NAPP-001', { itccApplicable: false });
await request(`/internships/audit-plans/${nonApplicablePlan.iapId}/activate`, {
  token: admin.token,
  method: 'POST',
  expected: 409,
});

const exceptionProject = await request('/internships/projects', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    ipcTitle: 'E2E — Proyecto completado con excepción documentada',
    ipcDescription: 'Fixture para conservar de forma inmutable una excepción administrativa.',
    ipcStatus: 'active',
    ipcActivationStatus: 'draft',
  },
});
const exceptionTask = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: exceptionProject.ipId,
    itcTitle: 'E2E — Plan con bloqueo externo aceptado',
    itcDescription: 'La excepción aprobada no puede reescribirse después del cierre.',
    itcProposedAssignee: intern.partyId,
    itcActivationStatus: 'draft',
  },
});
const exceptionPlan = await request('/internships/audit-plans', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    iapcProjectId: exceptionProject.ipId,
    iapcTaskId: exceptionTask.itId,
    iapcEnvironment: 'staging',
    iapcProposedAssignee: intern.partyId,
  },
});
await createAuditCase(exceptionPlan.iapId, 'STU-EXC-001');
await request(`/internships/audit-plans/${exceptionPlan.iapId}/activate`, {
  token: admin.token,
  method: 'POST',
});
const approvedExceptionJustification = 'El proveedor ficticio no está disponible; Diego acepta diferir este caso con el riesgo documentado.';
const completedWithException = await request(`/internships/audit-plans/${exceptionPlan.iapId}`, {
  token: admin.token,
  method: 'PATCH',
  json: {
    iapuCompletionJustification: approvedExceptionJustification,
    iapuApproveException: true,
    iapuStatus: 'completed',
  },
});
assert.equal(completedWithException.iapStatus, 'completed');
assert.equal(completedWithException.iapCompletionJustification, approvedExceptionJustification);
assert.equal(completedWithException.iapCompletionApprovedBy, admin.partyId);
assert.ok(completedWithException.iapCompletionApprovedAt);
await request(`/internships/audit-plans/${exceptionPlan.iapId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { iapuCompletionJustification: 'Texto posterior que no debe reemplazar la aprobación original.' },
});
const immutableExceptionPlan = await request(`/internships/audit-plans/${exceptionPlan.iapId}`, { token: admin.token });
assert.equal(immutableExceptionPlan.iapCompletionJustification, approvedExceptionJustification);
assert.equal(immutableExceptionPlan.iapCompletionApprovedBy, completedWithException.iapCompletionApprovedBy);
assert.equal(immutableExceptionPlan.iapCompletionApprovedAt, completedWithException.iapCompletionApprovedAt);

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

const siblingTask = await request('/internships/tasks', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    itcProjectId: project.ipId,
    itcTitle: 'E2E — Trabajo pendiente independiente',
    itcDescription: 'Esta tarea debe impedir que completar una auditoría cierre todo el proyecto.',
    itcActivationStatus: 'active',
  },
});
assert.equal(siblingTask.itStatus, 'todo');

await createAuditCase(plan.iapId, '123-INVALID', {}, 400);
const testCase = await createAuditCase(plan.iapId, 'STU-E2E-001');
const midpointRaceCase = await createAuditCase(plan.iapId, 'STU-E2E-002', {
  itccObjective: 'Validar la entrega única del aviso de punto medio bajo concurrencia.',
});

await request(`/internships/tasks/${task.itId}`, {
  token: admin.token,
  method: 'DELETE',
  expected: 409,
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
const projectsAfterPrimaryActivation = await request('/internships/projects', { token: admin.token });
const projectAfterPrimaryActivation = projectsAfterPrimaryActivation.find((candidate) => candidate.ipId === project.ipId);
assert.equal(projectAfterPrimaryActivation?.ipStatus, 'active');
assert.ok(projectAfterPrimaryActivation?.ipStartAt);
assert.ok(projectAfterPrimaryActivation?.ipDueAt);
await createAuditCase(plan.iapId, 'STU-E2E-LATE', {}, 409);
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
await request(`/internships/tasks/${task.itId}`, {
  token: intern.token,
  method: 'PATCH',
  expected: 409,
  json: { ituStatus: 'done' },
});
await request(`/internships/tasks/${task.itId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ituStatus: 'done' },
});
await request(`/internships/tasks/${task.itId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ituAssignedTo: admin.partyId },
});
await request(`/internships/tasks/${task.itId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ituProjectId: activeProject.ipId },
});
await request(`/internships/tasks/${task.itId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ituDueAt: '2099-01-01' },
});
for (const payload of [
  { ipuStatus: 'cancelled' },
  { ipuStartAt: '2099-01-01' },
  { ipuDueAt: '2099-01-02' },
]) {
  await request(`/internships/projects/${project.ipId}`, {
    token: admin.token,
    method: 'PATCH',
    expected: 409,
    json: payload,
  });
}
const projectsAfterRejectedAuditProjectEdits = await request('/internships/projects', { token: admin.token });
const projectAfterRejectedAuditProjectEdits = projectsAfterRejectedAuditProjectEdits.find(
  (candidate) => candidate.ipId === project.ipId,
);
assert.equal(projectAfterRejectedAuditProjectEdits?.ipStatus, projectAfterPrimaryActivation.ipStatus);
assert.equal(projectAfterRejectedAuditProjectEdits?.ipStartAt, projectAfterPrimaryActivation.ipStartAt);
assert.equal(projectAfterRejectedAuditProjectEdits?.ipDueAt, projectAfterPrimaryActivation.ipDueAt);

const siblingScheduleProject = await request('/internships/projects', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    ipcTitle: 'E2E — Proyecto con dos auditorías hermanas',
    ipcDescription: 'La segunda activación no debe reiniciar ni acortar el calendario compartido.',
    ipcStatus: 'active',
    ipcActivationStatus: 'draft',
  },
});
const createSiblingSchedulePlan = async (suffix, durationDays) => {
  const scheduleTask = await request('/internships/tasks', {
    token: admin.token,
    method: 'POST',
    expected: 201,
    json: {
      itcProjectId: siblingScheduleProject.ipId,
      itcTitle: `E2E — Auditoría hermana ${suffix}`,
      itcDescription: 'Fixture de calendario agregado para planes hermanos.',
      itcProposedAssignee: intern.partyId,
      itcActivationStatus: 'draft',
    },
  });
  const schedulePlan = await request('/internships/audit-plans', {
    token: admin.token,
    method: 'POST',
    expected: 201,
    json: {
      iapcProjectId: siblingScheduleProject.ipId,
      iapcTaskId: scheduleTask.itId,
      iapcEnvironment: 'staging',
      iapcDurationDays: durationDays,
      iapcProposedAssignee: intern.partyId,
    },
  });
  await createAuditCase(schedulePlan.iapId, `STU-SIBLING-SCHEDULE-${suffix}`, {
    itccCriticality: 'low',
    itccEvidenceRequirement: 'light',
  });
  return schedulePlan;
};
const longSiblingSchedulePlan = await createSiblingSchedulePlan('LONG', 30);
const shortSiblingSchedulePlan = await createSiblingSchedulePlan('SHORT', 7);
await request(`/internships/audit-plans/${longSiblingSchedulePlan.iapId}/activate`, {
  token: admin.token,
  method: 'POST',
});
const projectsAfterFirstSiblingActivation = await request('/internships/projects', { token: admin.token });
const projectAfterFirstSiblingActivation = projectsAfterFirstSiblingActivation.find(
  (candidate) => candidate.ipId === siblingScheduleProject.ipId,
);
assert.ok(projectAfterFirstSiblingActivation?.ipStartAt);
assert.ok(projectAfterFirstSiblingActivation?.ipDueAt);
await request(`/internships/audit-plans/${shortSiblingSchedulePlan.iapId}/activate`, {
  token: admin.token,
  method: 'POST',
});
const projectsAfterSecondSiblingActivation = await request('/internships/projects', { token: admin.token });
const projectAfterSecondSiblingActivation = projectsAfterSecondSiblingActivation.find(
  (candidate) => candidate.ipId === siblingScheduleProject.ipId,
);
assert.equal(projectAfterSecondSiblingActivation?.ipStartAt, projectAfterFirstSiblingActivation.ipStartAt);
assert.equal(projectAfterSecondSiblingActivation?.ipDueAt, projectAfterFirstSiblingActivation.ipDueAt);

const midpointExecutions = await Promise.all([
  request(`/internships/test-cases/${testCase.itcId}/executions`, {
    token: intern.token,
    method: 'POST',
    expected: 201,
    json: {
      itecStatus: 'passed',
      itecActualResult: 'El primer caso alcanzó el punto medio correctamente.',
      itecEvidenceSummary: 'EVIDENCIA-E2E-MIDPOINT-001',
    },
  }),
  request(`/internships/test-cases/${midpointRaceCase.itcId}/executions`, {
    token: intern.token,
    method: 'POST',
    expected: 201,
    json: {
      itecStatus: 'passed',
      itecActualResult: 'El segundo caso alcanzó el punto medio correctamente.',
      itecEvidenceSummary: 'EVIDENCIA-E2E-MIDPOINT-002',
    },
  }),
]);
assert.ok(midpointExecutions.every((execution) => execution.itexExecutionNumber === 1));

const concurrentExecutions = await Promise.all([
  request(`/internships/test-cases/${testCase.itcId}/executions`, {
    token: intern.token,
    method: 'POST',
    expected: 201,
    json: { itecStatus: 'pending' },
  }),
  request(`/internships/test-cases/${testCase.itcId}/executions`, {
    token: intern.token,
    method: 'POST',
    expected: 201,
    json: { itecStatus: 'pending' },
  }),
]);
assert.deepEqual(
  concurrentExecutions.map((execution) => execution.itexExecutionNumber).sort((left, right) => left - right),
  [2, 3],
);
const concurrentExecutionUpdates = await Promise.all([
  requestStatus(`/internships/test-executions/${concurrentExecutions[0].itexId}`, {
    token: intern.token,
    method: 'PATCH',
    json: {
      iteuStatus: 'passed',
      iteuActualResult: 'La ejecución concurrente terminó correctamente.',
      iteuEvidenceSummary: 'EVIDENCIA-E2E-CONCURRENT-PASS',
    },
  }),
  requestStatus(`/internships/test-executions/${concurrentExecutions[0].itexId}`, {
    token: intern.token,
    method: 'PATCH',
    json: {
      iteuStatus: 'passed',
      iteuActualResult: 'La ejecución concurrente confirmó una segunda respuesta válida.',
      iteuEvidenceSummary: 'EVIDENCIA-E2E-CONCURRENT-PASS-ALTERNATE',
    },
  }),
]);
assert.deepEqual(concurrentExecutionUpdates.sort((left, right) => left - right), [200, 409]);

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
await request(`/internships/test-executions/${failedExecution.itexId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: {
    iteuStatus: 'passed',
    iteuActualResult: 'Un administrador no puede reescribir un fallo terminal.',
    iteuEvidenceSummary: 'EVIDENCIA-E2E-ADMIN-TERMINAL-REWRITE-REJECTED',
  },
});
const executionsAfterAdminRewriteAttempt = await request(
  `/internships/test-cases/${testCase.itcId}/executions`,
  { token: admin.token },
);
assert.equal(
  executionsAfterAdminRewriteAttempt.find((execution) => execution.itexId === failedExecution.itexId)?.itexStatus,
  'failed',
);
await request(`/internships/test-cases/${testCase.itcId}/executions`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    itecStatus: 'passed',
    itecActualResult: 'Una ejecución posterior pasó, pero no debe ocultar el fallo sin reporte.',
    itecEvidenceSummary: 'EVIDENCIA-E2E-AFTER-UNREPORTED-FAILURE',
  },
});
const planWithHistoricalFailure = await request(`/internships/audit-plans/${plan.iapId}`, {
  token: admin.token,
});
assert.equal(planWithHistoricalFailure.iapFailedWithoutReport, 1);

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
await request('/feedback/internal', {
  token: admin.token,
  method: 'POST',
  expected: 400,
  json: {
    ...reportCreate,
    ifcTitle: 'No aceptar una ejecución de otra tarea',
    ifcTestCaseId: null,
    ifcInternshipTaskId: siblingTask.itId,
  },
});
await request('/feedback/internal', {
  token: intern.token,
  method: 'POST',
  expected: 400,
  json: { ...reportCreate, ifcCategoryId: ideaCategory.id },
});
const draftReport = await request('/feedback/internal', {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: reportCreate,
});
const reportId = draftReport.ifrSummary.ifsId;
assert.equal(draftReport.ifrSummary.ifsState, 'draft');
assert.equal(draftReport.ifrAuditPlanMutable, true);
const executionOnlyAdminDraft = await request('/feedback/internal', {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    ...reportCreate,
    ifcTitle: 'Reporte administrativo derivado sólo desde la ejecución',
    ifcInternshipProjectId: null,
    ifcInternshipTaskId: null,
    ifcTestCaseId: null,
  },
});
assert.equal(executionOnlyAdminDraft.ifrSummary.ifsInternshipProjectId, project.ipId);
assert.equal(executionOnlyAdminDraft.ifrSummary.ifsInternshipTaskId, task.itId);
assert.equal(executionOnlyAdminDraft.ifrSummary.ifsTestCaseId, testCase.itcId);
assert.equal(executionOnlyAdminDraft.ifrSummary.ifsTestExecutionId, failedExecution.itexId);
assert.equal(executionOnlyAdminDraft.ifrAuditPlanMutable, true);

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
const planWithReportedFailure = await request(`/internships/audit-plans/${plan.iapId}`, {
  token: admin.token,
});
assert.equal(planWithReportedFailure.iapFailedWithoutReport, 0);

const secondFailedExecution = await request(`/internships/test-cases/${testCase.itcId}/executions`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    itecStatus: 'blocked',
    itecActualResult: 'Una segunda ejecución quedó bloqueada después del primer reporte.',
    itecBlockerReason: 'El dato ficticio requerido no estuvo disponible.',
    itecEvidenceSummary: 'EVIDENCIA-E2E-BLOCKED-002',
  },
});
await request(`/internships/test-cases/${testCase.itcId}/executions`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    itecStatus: 'passed',
    itecActualResult: 'El pase posterior no debe reutilizar el reporte de la primera falla.',
    itecEvidenceSummary: 'EVIDENCIA-E2E-AFTER-SECOND-FAILURE',
  },
});
const planWithSecondHistoricalFailure = await request(`/internships/audit-plans/${plan.iapId}`, {
  token: admin.token,
});
assert.equal(planWithSecondHistoricalFailure.iapFailedWithoutReport, 1);
const secondFailureDraft = await request('/feedback/internal', {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    ...reportCreate,
    ifcTitle: 'La segunda ejecución quedó bloqueada sin datos ficticios',
    ifcDescription: 'La ejecución bloqueada necesita su propio reporte trazable.',
    ifcActualResult: 'No se pudo completar la segunda ejecución.',
    ifcTestExecutionId: secondFailedExecution.itexId,
  },
});
await request(`/feedback/internal/${secondFailureDraft.ifrSummary.ifsId}/submit`, {
  token: intern.token,
  method: 'POST',
});
const planWithBothFailuresReported = await request(`/internships/audit-plans/${plan.iapId}`, {
  token: admin.token,
});
assert.equal(planWithBothFailuresReported.iapFailedWithoutReport, 0);
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
const similarReceived = await request(`/feedback/internal/${similarDraft.ifrSummary.ifsId}/submit`, {
  token: intern.token,
  method: 'POST',
});
assert.equal(similarReceived.ifrSummary.ifsState, 'received');
const finalizedDraft = await request('/feedback/internal', {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: { ...reportCreate, ifcTitle: 'Borrador que debe quedar congelado al cerrar' },
});
assert.equal(finalizedDraft.ifrSummary.ifsState, 'draft');
await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 400,
  json: { ifuState: 'duplicate', ifuDuplicateOf: finalizedDraft.ifrSummary.ifsId },
});

const concurrentTransitions = await Promise.all([
  requestStatus(`/feedback/internal/${reportId}`, {
    token: admin.token,
    method: 'PATCH',
    json: { ifuState: 'duplicate', ifuDuplicateOf: similarDraft.ifrSummary.ifsId },
  }),
  requestStatus(`/feedback/internal/${reportId}`, {
    token: admin.token,
    method: 'PATCH',
    json: { ifuState: 'discarded' },
  }),
]);
assert.deepEqual(concurrentTransitions.sort((left, right) => left - right), [200, 409]);
await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  json: { ifuState: 'received' },
});

const duplicateSourceDraft = await request('/feedback/internal', {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: { ...reportCreate, ifcTitle: 'Reporte duplicado con destino canónico' },
});
const duplicateSourceReceived = await request(
  `/feedback/internal/${duplicateSourceDraft.ifrSummary.ifsId}/submit`,
  { token: intern.token, method: 'POST' },
);
const duplicateSource = await request(
  `/feedback/internal/${duplicateSourceReceived.ifrSummary.ifsId}`,
  {
    token: admin.token,
    method: 'PATCH',
    json: { ifuState: 'duplicate', ifuDuplicateOf: similarReceived.ifrSummary.ifsId },
  },
);
assert.equal(duplicateSource.ifrSummary.ifsState, 'duplicate');
assert.equal(duplicateSource.ifrSummary.ifsDuplicateOf, similarReceived.ifrSummary.ifsId);
await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 400,
  json: { ifuState: 'duplicate', ifuDuplicateOf: duplicateSource.ifrSummary.ifsId },
});
await request(`/feedback/internal/${similarReceived.ifrSummary.ifsId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ifuState: 'duplicate', ifuDuplicateOf: reportId },
});

const triaged = await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  json: {
    ifuState: 'confirmed',
    ifuAuthoritativeSeverityId: severity.id,
    ifuPriority: 'high',
    ifuAssignedTo: admin.partyId,
    ifuBlocking: true,
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
assert.equal(needsInfo.ifrSummary.ifsBlocking, true);
await request(`/feedback/internal/${reportId}`, {
  token: intern.token,
  method: 'PATCH',
  expected: 403,
  json: { ifuBlocking: false },
});
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

await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ifuState: 'verified' },
});
await request(`/feedback/internal/${reportId}/retests`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    ifrcResult: 'failed',
    ifrcNotes: 'El primer retest siguió mostrando dos filas ficticias.',
    ifrcEvidenceSummary: 'EVIDENCIA-E2E-RETEST-FAILED',
  },
});
await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ifuState: 'verified' },
});

const recordedRetest = await request(`/feedback/internal/${reportId}/retests`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    ifrcExecutionId: failedExecution.itexId,
    ifrcResult: 'passed',
    ifrcNotes: 'Repetí los pasos y quedó una sola fila ficticia.',
    ifrcEvidenceSummary: 'EVIDENCIA-E2E-RETEST-001',
  },
});
assert.ok(recordedRetest.ifrtExecutionId, 'A UI-style retest must create a linked immutable execution');
assert.notEqual(recordedRetest.ifrtExecutionId, failedExecution.itexId, 'A supplied stale execution must never be reused');
await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  json: { ifuState: 'verified', ifuResolution: 'Corregido y comprobado en base desechable.' },
});
for (const nextState of ['in_progress', 'ready_for_retest']) {
  await request(`/feedback/internal/${reportId}`, {
    token: admin.token,
    method: 'PATCH',
    json: { ifuState: nextState },
  });
}
await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ifuState: 'verified' },
});
const reopenedCycleRetest = await request(`/feedback/internal/${reportId}/retests`, {
  token: intern.token,
  method: 'POST',
  expected: 201,
  json: {
    ifrcResult: 'passed',
    ifrcNotes: 'El retest posterior a la reapertura volvió a confirmar una sola fila ficticia.',
    ifrcEvidenceSummary: 'EVIDENCIA-E2E-RETEST-REOPENED-001',
  },
});
assert.ok(reopenedCycleRetest.ifrtExecutionId, 'A reopened cycle must create a fresh immutable retest execution');
assert.notEqual(reopenedCycleRetest.ifrtExecutionId, recordedRetest.ifrtExecutionId);
await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  json: { ifuState: 'verified' },
});
await request(`/feedback/internal/${reportId}`, {
  token: admin.token,
  method: 'PATCH',
  json: { ifuState: 'closed', ifuClosureReason: 'Retest aprobado con evidencia ficticia.' },
});

await request(`/internships/audit-plans/${plan.iapId}/daily-summaries`, {
  token: admin.token,
  method: 'POST',
  expected: 403,
  json: {
    idscWorkDate: new Date().toISOString().slice(0, 10),
    idscMinutesWorked: 90,
    idscModulesTested: 'Prácticas y feedback',
    idscCasesCompleted: 1,
    idscReportsCreated: 2,
    idscBlockers: null,
    idscNextStep: 'La administración no debe suplir el registro diario de la persona asignada.',
  },
});
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
  token: admin.token,
  method: 'PUT',
  expected: 403,
  json: {
    ifsuConclusions: 'Un administrador revisa, pero no suplanta a la persona asignada.',
    ifsuSubmit: true,
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
assert.equal(completed.iapCompletionApprovedBy, admin.partyId);
assert.ok(completed.iapCompletionApprovedAt);
await request(`/internships/audit-plans/${plan.iapId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { iapuStatus: 'completed' },
});
const completedAfterRetry = await request(`/internships/audit-plans/${plan.iapId}`, { token: admin.token });
assert.equal(completedAfterRetry.iapCompletionApprovedBy, completed.iapCompletionApprovedBy);
assert.equal(completedAfterRetry.iapCompletionApprovedAt, completed.iapCompletionApprovedAt);
const projectsAfterCompletion = await request('/internships/projects', { token: admin.token });
const projectAfterCompletion = projectsAfterCompletion.find((candidate) => candidate.ipId === project.ipId);
assert.equal(projectAfterCompletion?.ipStatus, 'active');

const finalizedInternPlan = await request(`/internships/audit-plans/${plan.iapId}`, { token: intern.token });
assert.equal(finalizedInternPlan.iapStatus, 'completed');
const finalizedInternPlans = await request('/internships/audit-plans', { token: intern.token });
assert.ok(finalizedInternPlans.some((candidate) => candidate.iapId === plan.iapId));
const finalizedCases = await request(`/internships/audit-plans/${plan.iapId}/cases`, { token: intern.token });
assert.ok(finalizedCases.some((candidate) => candidate.itcId === testCase.itcId));
const finalizedDailySummaries = await request(`/internships/audit-plans/${plan.iapId}/daily-summaries`, { token: intern.token });
assert.equal(finalizedDailySummaries.length, 1);
const finalizedSummary = await request(`/internships/audit-plans/${plan.iapId}/final-summary`, { token: intern.token });
assert.equal(finalizedSummary.ifsApprovedBy, admin.partyId);
await request(`/internships/audit-plans/${plan.iapId}/daily-summaries`, {
  token: intern.token,
  method: 'POST',
  expected: 409,
  json: {
    idscWorkDate: new Date().toISOString().slice(0, 10),
    idscMinutesWorked: 15,
    idscModulesTested: 'Prácticas',
    idscCasesCompleted: 0,
    idscReportsCreated: 0,
    idscBlockers: null,
    idscNextStep: 'No debe persistirse después del cierre.',
  },
});
await request(`/internships/audit-plans/${plan.iapId}/final-summary`, {
  token: intern.token,
  method: 'PUT',
  expected: 409,
  json: { ifsuConclusions: 'No debe cambiar después del cierre.', ifsuSubmit: true },
});
await request('/feedback/internal', {
  token: intern.token,
  method: 'POST',
  expected: 409,
  json: { ...reportCreate, ifcTitle: 'No crear después del cierre' },
});
await request('/feedback/internal', {
  token: admin.token,
  method: 'POST',
  expected: 409,
  json: { ...reportCreate, ifcTitle: 'Administración tampoco crea después del cierre' },
});
await request(`/feedback/internal/${similarDraft.ifrSummary.ifsId}/comments`, {
  token: admin.token,
  method: 'POST',
  expected: 409,
  json: {
    ifccKind: 'information_request',
    ifccBody: 'No debe reabrir el reporte después del cierre de la auditoría.',
  },
});
await request(`/feedback/internal/${finalizedDraft.ifrSummary.ifsId}/submit`, {
  token: intern.token,
  method: 'POST',
  expected: 409,
});
await request(`/feedback/internal/${finalizedDraft.ifrSummary.ifsId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ifuPriority: 'low' },
});
await request(`/feedback/internal/${finalizedDraft.ifrSummary.ifsId}/comments`, {
  token: intern.token,
  method: 'POST',
  expected: 409,
  json: { ifccKind: 'comment', ifccBody: 'No debe añadirse después del cierre.' },
});
await request(`/feedback/internal/${finalizedDraft.ifrSummary.ifsId}/evidence-links`, {
  token: intern.token,
  method: 'POST',
  expected: 409,
  json: {
    ifelUrl: 'https://evidence.example.test/finalized',
    ifelCaption: 'No debe persistirse.',
    ifelKind: 'external_link',
  },
});

const executions = await request(`/internships/test-cases/${testCase.itcId}/executions`, { token: admin.token });
assert.deepEqual(
  executions.map((execution) => execution.itexExecutionNumber),
  [10, 9, 8, 7, 6, 5, 4, 3, 2, 1],
);
assert.equal(executions[0].itexId, reopenedCycleRetest.ifrtExecutionId);
assert.equal(executions[0].itexStatus, 'verified');
assert.ok(executions.some((execution) => execution.itexId === failedExecution.itexId));
assert.ok(executions.some((execution) => execution.itexId === secondFailedExecution.itexId));
await request(`/internships/test-cases/${testCase.itcId}/executions`, {
  token: admin.token,
  method: 'POST',
  expected: 409,
  json: { itecStatus: 'pending' },
});
await request(`/internships/test-executions/${concurrentExecutions[0].itexId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { iteuStatus: 'in_progress' },
});
await request(`/feedback/internal/${reportId}/retests`, {
  token: admin.token,
  method: 'POST',
  expected: 409,
  json: {
    ifrcExecutionId: failedExecution.itexId,
    ifrcResult: 'passed',
    ifrcNotes: 'No se debe registrar después del cierre.',
    ifrcEvidenceSummary: 'EVIDENCIA-E2E-RETEST-POST-CIERRE',
  },
});
const approvedSummary = await request(`/internships/audit-plans/${plan.iapId}/final-summary`, { token: admin.token });
assert.equal(approvedSummary.ifsApprovedBy, admin.partyId);
assert.ok(approvedSummary.ifsApprovedAt);
const finalReport = await request(`/feedback/internal/${reportId}`, { token: admin.token });
assert.equal(finalReport.ifrSummary.ifsState, 'closed');
assert.equal(finalReport.ifrAuditPlanMutable, false);
assert.ok(finalReport.ifrHistory.length >= 10);
assert.equal(finalReport.ifrRetests.length, 3);
const finalizedExecutionOnlyAdminDraft = await request(
  `/feedback/internal/${executionOnlyAdminDraft.ifrSummary.ifsId}`,
  { token: admin.token },
);
assert.equal(finalizedExecutionOnlyAdminDraft.ifrAuditPlanMutable, false);
await request(`/feedback/internal/${executionOnlyAdminDraft.ifrSummary.ifsId}`, {
  token: admin.token,
  method: 'PATCH',
  expected: 409,
  json: { ifuPriority: 'low' },
});
const csv = await request('/feedback/internal/export.csv', { token: admin.token });
assert.match(csv, /El guardado duplica el registro ficticio/);
const exported = await request('/feedback/internal/export.json', { token: admin.token });
assert.ok(exported.some((entry) => entry.ifsId === reportId));

console.log(JSON.stringify({
  result: 'passed',
  personas: { admin: 'PER-13', intern: 'PER-11', otherInternIsolation: Boolean(otherIntern) },
  draftActivation: 'verified',
  serializedActivationCancellation: `${activationStatus}/${cancellationStatus}`,
  executionHistoryCount: executions.length,
  reportState: finalReport.ifrSummary.ifsState,
  reportHistoryCount: finalReport.ifrHistory.length,
  testTransportExpected: true,
}, null, 2));
