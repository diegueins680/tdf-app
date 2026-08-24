import { buildAuthorizationHeader } from './authHeader';
import { resolveApiBase } from '../config/apiBase';
import { get, patch, post, postEmpty, postForm } from './client';
import type {
  InternalFeedbackCommentDTO,
  InternalFeedbackDTO,
  InternalFeedbackEvidenceDTO,
  InternalFeedbackRetestDTO,
  InternalReportState,
  InternalFeedbackSummaryDTO,
  InternalReportType,
  LegacyFeedbackDTO,
} from './types';

export interface InternalFeedbackCreate {
  ifcTitle: string;
  ifcDescription: string;
  ifcCategoryId: string;
  ifcProposedSeverityId: string;
  ifcReportType: InternalReportType;
  ifcModuleName: string;
  ifcFeatureName?: string | null;
  ifcEnvironment: string;
  ifcUrlOrScreen?: string | null;
  ifcPlatform: string;
  ifcDevice?: string | null;
  ifcBrowser?: string | null;
  ifcLanguage: string;
  ifcAccountRole: string;
  ifcReproductionSteps?: string | null;
  ifcExpectedResult?: string | null;
  ifcActualResult?: string | null;
  ifcFrequency?: string | null;
  ifcTestCaseId?: string | null;
  ifcTestExecutionId?: string | null;
  ifcInternshipProjectId?: string | null;
  ifcInternshipTaskId?: string | null;
  ifcBlocking?: boolean;
  ifcVideoLinks?: string | null;
}

export interface InternalFeedbackUpdate {
  ifuTitle?: string;
  ifuCategoryId?: string;
  ifuProposedSeverityId?: string;
  ifuReportType?: InternalReportType;
  ifuModuleName?: string;
  ifuFeatureName?: string | null;
  ifuEnvironment?: string;
  ifuUrlOrScreen?: string | null;
  ifuPlatform?: string;
  ifuDevice?: string | null;
  ifuBrowser?: string | null;
  ifuLanguage?: string;
  ifuAccountRole?: string;
  ifuFrequency?: string | null;
  ifuBlocking?: boolean;
  ifuState?: InternalReportState;
  ifuAuthoritativeSeverityId?: string | null;
  ifuPriority?: string | null;
  ifuAssignedTo?: number | null;
  ifuDuplicateOf?: string | null;
  ifuResolution?: string | null;
  ifuRetestResult?: string | null;
  ifuClosureReason?: string | null;
  ifuGithubIssueUrl?: string | null;
  ifuDescription?: string;
  ifuReproductionSteps?: string | null;
  ifuExpectedResult?: string | null;
  ifuActualResult?: string | null;
  ifuVideoLinks?: string | null;
}

const queryString = (filters: Record<string, string | boolean | undefined>) => {
  const query = new URLSearchParams();
  Object.entries(filters).forEach(([key, value]) => {
    if (value !== undefined && value !== '') query.set(key, String(value));
  });
  const encoded = query.toString();
  return encoded ? `?${encoded}` : '';
};

export const InternalFeedback = {
  list: (filters: { state?: string; module?: string; q?: string; mine?: boolean } = {}) =>
    get<InternalFeedbackSummaryDTO[]>(`/feedback/internal${queryString(filters)}`),
  listLegacy: () => get<LegacyFeedbackDTO[]>('/feedback/internal/legacy'),
  get: (reportId: string) =>
    get<InternalFeedbackDTO>(`/feedback/internal/${encodeURIComponent(reportId)}`),
  create: (payload: InternalFeedbackCreate) =>
    post<InternalFeedbackDTO>('/feedback/internal', payload),
  update: (reportId: string, payload: InternalFeedbackUpdate) =>
    patch<InternalFeedbackDTO>(`/feedback/internal/${encodeURIComponent(reportId)}`, payload),
  submit: (reportId: string) =>
    postEmpty<InternalFeedbackDTO>(`/feedback/internal/${encodeURIComponent(reportId)}/submit`),
  comment: (reportId: string, body: string, kind = 'comment') =>
    post<InternalFeedbackCommentDTO>(
      `/feedback/internal/${encodeURIComponent(reportId)}/comments`,
      { ifccKind: kind, ifccBody: body },
    ),
  uploadEvidence: (reportId: string, attachment: File, caption?: string) => {
    const form = new FormData();
    form.append('attachment', attachment);
    if (caption?.trim()) form.append('caption', caption.trim());
    return postForm<InternalFeedbackEvidenceDTO>(
      `/feedback/internal/${encodeURIComponent(reportId)}/evidence`,
      form,
    );
  },
  linkEvidence: (reportId: string, url: string, caption?: string, kind = 'video_link') =>
    post<InternalFeedbackEvidenceDTO>(
      `/feedback/internal/${encodeURIComponent(reportId)}/evidence-links`,
      { ifelUrl: url, ifelCaption: caption, ifelKind: kind },
    ),
  retest: (
    reportId: string,
    payload: { ifrcExecutionId?: string; ifrcResult: string; ifrcNotes?: string; ifrcEvidenceSummary?: string },
  ) => post<InternalFeedbackRetestDTO>(
    `/feedback/internal/${encodeURIComponent(reportId)}/retests`,
    payload,
  ),
  exportCsv: (filters: { state?: string; module?: string } = {}) =>
    get<string>(`/feedback/internal/export.csv${queryString(filters)}`),
  exportJson: (filters: { state?: string; module?: string } = {}) =>
    get<InternalFeedbackSummaryDTO[]>(`/feedback/internal/export.json${queryString(filters)}`),
  downloadEvidence: async (reportId: string, evidenceId: string): Promise<Blob> => {
    const auth = buildAuthorizationHeader();
    const response = await fetch(
      `${resolveApiBase()}/feedback/internal/${encodeURIComponent(reportId)}/evidence/${encodeURIComponent(evidenceId)}/file`,
      { credentials: 'include', headers: auth ? { Authorization: auth } : undefined },
    );
    if (!response.ok) throw new Error('No se pudo descargar la evidencia.');
    return response.blob();
  },
};
