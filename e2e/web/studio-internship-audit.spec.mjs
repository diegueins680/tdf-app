import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const planId = '11111111-1111-4111-8111-111111111111';
const projectId = '22222222-2222-4222-8222-222222222222';
const taskId = '33333333-3333-4333-8333-333333333333';
const caseId = '44444444-4444-4444-8444-444444444444';
const reportId = '55555555-5555-4555-8555-555555555555';

const plan = {
  iapId: planId, iapProjectId: projectId, iapTaskId: taskId, iapEnvironment: 'staging', iapStatus: 'active',
  iapDurationDays: 14, iapExpectedHoursMin: 20, iapExpectedHoursMax: 30, iapMidpointPercent: 50,
  iapProposedAssignee: 911, iapFinalReviewRequired: true, iapCompletionJustification: null,
  iapCompletionApprovedBy: null, iapCompletionApprovedAt: null, iapCaseCount: 174, iapExecutedCaseCount: 1,
  iapCriticalRemaining: 89, iapOpenBlockerCount: 0, iapFailedWithoutReport: 0, iapEvidenceMissing: 0,
  iapCalculatedProgress: 1, iapCanComplete: false,
  iapCreatedAt: '2026-08-21T12:00:00Z', iapUpdatedAt: '2026-08-21T12:00:00Z',
};
const testCase = {
  itcId: caseId, itcPlanId: planId, itcStableId: 'STU-SCH-001',
  itcModuleName: 'Calendario, salas, recursos y reservas', itcFeatureName: 'Conflicto simultáneo de sala',
  itcUserRole: 'Reception', itcObjective: 'Comprobar que una sala no se reserve dos veces.',
  itcBusinessPurpose: 'Proteger la operación y evitar promesas incompatibles.',
  itcPreconditions: 'Staging confirmado y dos sesiones ficticias.', itcRequiredTestData: 'AUDIT-BOOKING-HOLD',
  itcEnvironment: 'staging', itcPlatform: 'Web', itcBrowserOrDevice: 'Chrome de pruebas', itcLanguage: 'Español',
  itcDetailedSteps: '1. Abre dos sesiones.\n2. Confirma el mismo horario.\n3. Recarga.',
  itcExpectedResult: 'Sólo una reserva se confirma.', itcExpectedPersistedState: 'Existe una sola reserva confirmada.',
  itcExpectedSideEffects: 'Ninguna comunicación real.', itcCleanupInstructions: 'Destruye el tenant dedicado.',
  itcCriticality: 'critical', itcEvidenceRequirement: 'strong', itcExploratoryCharter: null,
  itcApplicable: true, itcSortOrder: 1, itcLatestExecution: null,
};

const catalogs = {
  catalogs: [
    {
      catalog: { id: 'cat-1', code: 'feedback-categories', name: 'Categorías' },
      items: [{ id: '66666666-6666-4666-8666-666666666666', code: 'bug', name: 'Error', active: true, workflowState: 'published', deprecatedAt: null }],
      defaults: [{ scopeKind: 'feedback-category', scopeId: 'global', localeId: null, entityId: '66666666-6666-4666-8666-666666666666' }],
    },
    {
      catalog: { id: 'cat-2', code: 'feedback-severities', name: 'Severidades' },
      items: [{ id: '77777777-7777-4777-8777-777777777777', code: 'high', name: 'Alta', active: true, workflowState: 'published', deprecatedAt: null }],
      defaults: [{ scopeKind: 'feedback-severity', scopeId: 'global', localeId: null, entityId: '77777777-7777-4777-8777-777777777777' }],
    },
  ],
};

async function mockShell(page, role, partyId) {
  await page.route('**/session', (route) => route.fulfill({
    json: {
      username: role === 'Intern' ? 'per-11.martina@persona.test' : 'per-13.fernando@persona.test',
      displayName: role === 'Intern' ? 'Martina Salazar' : 'Fernando Lema',
      roles: [role], modules: ['Internships'], featureFlags: [], partyId,
    },
  }));
  await page.route('**/health', (route) => route.fulfill({ json: { status: 'ok' } }));
  await page.route('**/session/preferences', (route) => route.fulfill({
    json: {
      localeId: '', locale: 'es', currencyId: '', currency: 'USD',
      timezone: 'America/Guayaquil', countryId: null, countryCode: 'EC',
    },
  }));
  await page.route('**/catalogs/batch?*', (route) => route.fulfill({ json: catalogs }));
  await page.route('**/catalog/*/items?*', (route) => route.fulfill({
    json: {
      catalog: { id: 'catalog-empty', code: 'empty', name: 'Catálogo de prueba' },
      items: [], defaults: [], page: 1, pageSize: 500, total: 0, revision: 1, locale: 'es',
    },
  }));
  await page.route('**/radio/streams*', (route) => route.fulfill({ json: [] }));
  await page.route('**/radio/presence*', (route) => route.fulfill({ json: null }));
  await page.route('**/radio/auto-stop-options*', (route) => route.fulfill({ json: { options: [] } }));
  await page.route('**/chat/threads', (route) => route.fulfill({ json: [] }));
  await page.route('**/fans/me/notifications/count', (route) => route.fulfill({ json: { ncUnread: 0 } }));
  await page.route('**/navigation/preferences', (route) => route.fulfill({ json: [] }));
  await page.route('**/navigation/preferences/*/visit', (route) => route.fulfill({
    json: {
      featureId: 'synthetic', favorite: false, pinned: false, pinOrder: null,
      lastVisitedAt: '2026-08-21T12:00:00Z', useCount: 1, updatedAt: '2026-08-21T12:00:00Z',
    },
  }));
  await page.route('**/me/notifications*', (route) => route.fulfill({ json: [] }));
  await page.route('https://api.frankfurter.dev/**', (route) => route.fulfill({ json: { base: 'USD', rates: { USD: 1 } } }));
}

async function expectNoSeriousAxeViolations(page) {
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => {
    const result = await globalThis.axe.run(document, { resultTypes: ['violations'] });
    return result.violations.filter((item) => item.impact === 'critical' || item.impact === 'serious').map((item) => item.id);
  });
  expect(violations).toEqual([]);
}

test('@critical synthetic intern records a failed case and reaches a trace-linked report draft', async ({ page }) => {
  await mockShell(page, 'Intern', 911);
  await page.route(`**/internships/audit-plans/${planId}`, (route) => route.fulfill({ json: plan }));
  await page.route(`**/internships/audit-plans/${planId}/cases`, (route) => route.fulfill({ json: [testCase] }));
  await page.route(`**/internships/audit-plans/${planId}/daily-summaries`, (route) => route.fulfill({ json: [] }));
  await page.route(`**/internships/audit-plans/${planId}/final-summary`, (route) => route.fulfill({ status: 404, json: { error: 'not submitted' } }));
  await page.route(`**/internships/test-cases/${caseId}/executions`, async (route) => {
    if (route.request().method() === 'GET') return route.fulfill({ json: [] });
    const payload = route.request().postDataJSON();
    expect(payload).toMatchObject({ itecStatus: 'failed', itecActualResult: 'Las dos reservas quedaron confirmadas.' });
    return route.fulfill({
      status: 201,
      json: {
        itexId: '88888888-8888-4888-8888-888888888888', itexTestCaseId: caseId, itexExecutionNumber: 1,
        itexExecutorPartyId: 911, itexStatus: 'failed', itexActualResult: payload.itecActualResult,
        itexPersistedStateObserved: payload.itecPersistedStateObserved, itexSideEffectsObserved: payload.itecSideEffectsObserved,
        itexBlockerReason: null, itexEvidenceSummary: payload.itecEvidenceSummary,
        itexStartedAt: null, itexCompletedAt: '2026-08-21T13:00:00Z',
        itexCreatedAt: '2026-08-21T13:00:00Z', itexUpdatedAt: '2026-08-21T13:00:00Z',
      },
    });
  });

  await page.goto(`/practicas/auditorias/${planId}`);
  await expect(page.getByRole('heading', { name: 'Auditoría funcional y de experiencia del manejo del estudio' })).toBeVisible();
  await expect(page.getByText('Avance calculado: 1%')).toBeVisible();
  await page.getByText('STU-SCH-001').click();
  await page.getByLabel('Resultado').last().click();
  await page.getByRole('option', { name: 'Fallido' }).click();
  await page.getByLabel('Qué ocurrió').fill('Las dos reservas quedaron confirmadas.');
  await page.getByLabel('Estado guardado que comprobaste').fill('Persisten dos filas confirmadas para la misma sala y hora.');
  await page.getByLabel('Resumen o enlace de evidencia').fill('Capturas ficticias AUDIT-2026 y consulta de estado.');
  await page.getByRole('button', { name: 'Guardar resultado' }).click();
  await expect(page.getByRole('alert').filter({ hasText: 'El resultado y la evidencia quedaron registrados.' })).toBeVisible();

  const reportLink = page.getByRole('link', { name: 'Crear reporte vinculado' });
  await expect(reportLink).toHaveAttribute('href', new RegExp(`testCaseId=${caseId}`));
  await reportLink.click();
  await expect(page).toHaveURL(/\/feedback\/interno\/nuevo\?/);
  await expect(page.getByRole('textbox', { name: /^Módulo(?: \*)?$/ })).toHaveValue('Calendario, salas, recursos y reservas');
  await expectNoSeriousAxeViolations(page);
});

test('@critical synthetic administrator can see all-report controls and prepare triage without activation', async ({ page }) => {
  await mockShell(page, 'StudioManager', 913);
  const report = {
    ifsId: reportId, ifsTitle: 'Conflicto de sala no bloqueado', ifsReportType: 'error', ifsState: 'received',
    ifsModuleName: 'Calendario', ifsFeatureName: 'Conflicto de sala', ifsEnvironment: 'staging', ifsPlatform: 'web',
    ifsProposedSeverityId: '77777777-7777-4777-8777-777777777777', ifsAuthoritativeSeverityId: null,
    ifsPriority: null, ifsBlocking: true, ifsReporterPartyId: 911, ifsReporterName: 'Martina Salazar',
    ifsInternshipProjectId: projectId, ifsInternshipTaskId: taskId, ifsTestCaseId: caseId,
    ifsTestExecutionId: null, ifsDuplicateOf: null,
    ifsCreatedAt: '2026-08-21T13:00:00Z', ifsUpdatedAt: '2026-08-21T13:00:00Z',
  };
  await page.route('**/feedback/internal/legacy', (route) => route.fulfill({ json: [] }));
  await page.route('**/feedback/internal?*', (route) => route.fulfill({ json: [report] }));
  await page.route('**/feedback/internal', (route) => route.fulfill({ json: [report] }));
  await page.route(`**/feedback/internal/${reportId}`, async (route) => {
    if (route.request().method() === 'PATCH') {
      expect(route.request().postDataJSON()).toMatchObject({ ifuState: 'confirmed', ifuPriority: 'high' });
      return route.fulfill({ json: { ifrSummary: { ...report, ifsState: 'confirmed', ifsPriority: 'high' } } });
    }
    return route.fulfill({ json: {
      ifrSummary: report, ifrDescription: 'Dos reservas simultáneas.', ifrCategoryId: null,
      ifrUrlOrScreen: '/estudio/calendario', ifrDevice: 'Desktop', ifrBrowser: 'Chrome', ifrLanguage: 'es',
      ifrAccountRole: 'Reception', ifrReproductionSteps: 'Abrir dos sesiones y confirmar.',
      ifrExpectedResult: 'Una confirmación.', ifrActualResult: 'Dos confirmaciones.', ifrFrequency: 'Siempre',
      ifrAssignedTo: null, ifrResolution: null, ifrRetestResult: null, ifrClosureReason: null,
      ifrGithubIssueUrl: null, ifrVideoLinks: null, ifrSubmittedAt: '2026-08-21T13:00:00Z', ifrClosedAt: null,
      ifrEvidence: [], ifrComments: [], ifrHistory: [], ifrRetests: [], ifrPotentialDuplicates: [],
    } });
  });

  await page.goto('/feedback/interno');
  await expect(page.getByRole('heading', { name: 'Reportes internos de pruebas' })).toBeVisible();
  await expect(page.getByRole('button', { name: 'CSV' })).toBeVisible();
  await expect(page.getByText('Reportó: Martina Salazar')).toBeVisible();
  await page.getByRole('link', { name: 'Abrir seguimiento' }).click();
  await expect(page.getByRole('heading', { name: 'Triage administrativo' })).toBeVisible();
  await page.getByLabel('Nuevo estado').click();
  await page.getByRole('option', { name: 'Confirmado' }).click();
  await page.getByLabel('Prioridad').click();
  await page.getByRole('option', { name: 'Alta' }).click();
  await page.getByRole('button', { name: 'Guardar triage' }).click();
  await expect(page.getByRole('alert')).toContainText('Cambio guardado y registrado en el historial.');
  await expectNoSeriousAxeViolations(page);
});
