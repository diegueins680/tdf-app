import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postEmptyMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postFormMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const patchMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const putMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  postEmpty: postEmptyMock,
  postForm: postFormMock,
  patch: patchMock,
  put: putMock,
}));
jest.unstable_mockModule('./authHeader', () => ({
  buildAuthorizationHeader: () => 'Bearer audit-token',
}));
jest.unstable_mockModule('../config/apiBase', () => ({
  resolveApiBase: () => 'https://api.audit.test',
}));

const { InternAudit } = await import('./internAudit');
const { InternalFeedback } = await import('./internalFeedback');

describe('studio internship audit API', () => {
  beforeEach(() => jest.clearAllMocks());

  it('uses encoded access-scoped plan, case, execution, daily, and final-summary routes', async () => {
    getMock.mockResolvedValue([]);
    postMock.mockResolvedValue({});
    patchMock.mockResolvedValue({});
    putMock.mockResolvedValue({});

    await InternAudit.listCases('plan/one');
    await InternAudit.completePlan('plan/one');
    await InternAudit.createExecution('case/one', { itecStatus: 'failed', itecActualResult: 'Falla visible' });
    await InternAudit.updateExecution('execution/one', { itecStatus: 'in_progress', itecEvidenceSummary: 'Captura segura' });
    await InternAudit.createDailySummary('plan/one', {
      idscWorkDate: '2026-08-21',
      idscMinutesWorked: 120,
      idscModulesTested: 'Reservas',
      idscCasesCompleted: 3,
      idscReportsCreated: 1,
      idscNextStep: 'Continuar conflictos',
    });
    await InternAudit.saveFinalSummary('plan/one', 'Conclusiones', true);

    expect(getMock).toHaveBeenCalledWith('/internships/audit-plans/plan%2Fone/cases');
    expect(patchMock).toHaveBeenCalledWith(
      '/internships/audit-plans/plan%2Fone',
      { iapuStatus: 'completed' },
    );
    expect(postMock).toHaveBeenCalledWith(
      '/internships/test-cases/case%2Fone/executions',
      expect.objectContaining({ itecStatus: 'failed' }),
    );
    expect(patchMock).toHaveBeenCalledWith(
      '/internships/test-executions/execution%2Fone',
      expect.objectContaining({ iteuStatus: 'in_progress', iteuEvidenceSummary: 'Captura segura' }),
    );
    expect(postMock).toHaveBeenCalledWith(
      '/internships/audit-plans/plan%2Fone/daily-summaries',
      expect.objectContaining({ idscMinutesWorked: 120 }),
    );
    expect(putMock).toHaveBeenCalledWith(
      '/internships/audit-plans/plan%2Fone/final-summary',
      { ifsuConclusions: 'Conclusiones', ifsuSubmit: true },
    );
  });

  it('keeps report filters encoded and reuses the internal feedback root', async () => {
    getMock.mockResolvedValue([]);
    postMock.mockResolvedValue({});

    await InternalFeedback.list({ state: 'ready_for_retest', module: 'Salas y recursos', q: 'doble clic', mine: true });
    await InternalFeedback.create({
      ifcTitle: 'No impide el conflicto',
      ifcDescription: 'Dos reservas quedan confirmadas.',
      ifcCategoryId: '11111111-1111-4111-8111-111111111111',
      ifcProposedSeverityId: '22222222-2222-4222-8222-222222222222',
      ifcReportType: 'error',
      ifcModuleName: 'Salas y recursos',
      ifcEnvironment: 'staging',
      ifcPlatform: 'web',
      ifcLanguage: 'es',
      ifcAccountRole: 'Intern',
    });

    expect(getMock).toHaveBeenCalledWith(
      '/feedback/internal?state=ready_for_retest&module=Salas+y+recursos&q=doble+clic&mine=true',
    );
    expect(postMock).toHaveBeenCalledWith(
      '/feedback/internal',
      expect.objectContaining({ ifcReportType: 'error', ifcEnvironment: 'staging' }),
    );
  });

  it('adds comments, HTTPS evidence links, and retests without replacing report history', async () => {
    postMock.mockResolvedValue({});

    await InternalFeedback.comment('report/id', 'Adjunto la información solicitada.', 'information_response');
    await InternalFeedback.linkEvidence('report/id', 'https://evidence.example.test/video/1', 'Video corto');
    await InternalFeedback.retest('report/id', {
      ifrcExecutionId: 'execution-id',
      ifrcResult: 'passed',
      ifrcNotes: 'El conflicto ya se rechaza.',
      ifrcEvidenceSummary: 'Captura y estado persistido.',
    });

    expect(postMock).toHaveBeenNthCalledWith(
      1,
      '/feedback/internal/report%2Fid/comments',
      { ifccKind: 'information_response', ifccBody: 'Adjunto la información solicitada.' },
    );
    expect(postMock).toHaveBeenNthCalledWith(
      2,
      '/feedback/internal/report%2Fid/evidence-links',
      { ifelUrl: 'https://evidence.example.test/video/1', ifelCaption: 'Video corto', ifelKind: 'video_link' },
    );
    expect(postMock).toHaveBeenNthCalledWith(
      3,
      '/feedback/internal/report%2Fid/retests',
      expect.objectContaining({ ifrcExecutionId: 'execution-id', ifrcResult: 'passed' }),
    );
  });

  it('uses multipart evidence and an authorized private download route', async () => {
    postFormMock.mockResolvedValue({});
    const file = new File(['safe-image'], 'captura.png', { type: 'image/png' });
    await InternalFeedback.uploadEvidence('report/id', file, 'Vista del error');

    const form = postFormMock.mock.calls[0]?.[1] as FormData;
    expect(postFormMock).toHaveBeenCalledWith('/feedback/internal/report%2Fid/evidence', expect.any(FormData));
    expect(form.get('attachment')).toBe(file);
    expect(form.get('caption')).toBe('Vista del error');

    const fetchMock = jest.fn<typeof fetch>().mockResolvedValue({
      ok: true,
      blob: async () => new Blob(['evidence'], { type: 'image/png' }),
    } as Response);
    (globalThis as { fetch: typeof fetch }).fetch = fetchMock;
    await InternalFeedback.downloadEvidence('report/id', 'evidence/id');

    expect(fetchMock).toHaveBeenCalledWith(
      'https://api.audit.test/feedback/internal/report%2Fid/evidence/evidence%2Fid/file',
      { credentials: 'include', headers: { Authorization: 'Bearer audit-token' } },
    );
  });
});
