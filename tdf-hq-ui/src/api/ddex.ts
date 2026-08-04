import { get, post, patch } from './client';

// Document types
export interface DdexDocumentDTO {
  ddexDocumentId: number;
  ddexDocumentFileName: string;
  ddexDocumentSha256: string;
  ddexDocumentFamily: string;
  ddexDocumentVersion: string;
  ddexDocumentStatus: string;
  ddexDocumentMessageId: string | null;
  ddexDocumentSenderId: string | null;
  ddexDocumentRecipientId: string | null;
  ddexDocumentCreatedAt: string;
}

export interface DdexUploadRequest {
  uploadFileName: string;
  uploadContentType: string;
  uploadContentBase64: string;
}

export interface DdexDownloadResponse {
  downloadFileName: string;
  downloadContentType: string;
  downloadContentBase64: string;
}

// Validation types
export interface ValidationRunDTO {
  validationRunId: number;
  validationRunDocumentId: number;
  validationRunStatus: string;
  validationRunStartedAt: string;
  validationRunFinishedAt: string | null;
}

export interface ValidationReportDTO {
  reportRunId: number;
  reportIssues: ValidationIssueDTO[];
  reportIsValid: boolean;
}

export interface ValidationIssueDTO {
  issueSeverity: string;
  issueLayer: string;
  issueCode: string;
  issueMessage: string;
  issueLine: number | null;
  issueColumn: number | null;
}

// Preview types
export interface DdexPreviewDTO {
  previewMessageId: string;
  previewSender: string;
  previewReleaseCount: number;
  previewResourceCount: number;
  previewWarnings: string[];
}

// Import plan types
export interface ImportPlanDTO {
  importPlanId: number;
  importPlanDocumentId: number;
  importPlanStatus: string;
  importPlanConflicts: ImportConflictDTO[];
  importPlanChanges: string[];
}

export interface ImportConflictDTO {
  conflictId: number;
  conflictEntityType: string;
  conflictIdentifier: string;
  conflictDescription: string;
}

export interface ImportPlanResolution {
  resolutionPlanId: number;
  resolutionConflicts: ConflictResolution[];
}

export interface ConflictResolution {
  resolutionConflictId: number;
  resolutionAction: 'UseExisting' | 'CreateNew' | 'Ignore';
  resolutionTargetId: number | null;
}

export interface ImportRunDTO {
  importRunId: number;
  importRunPlanId: number;
  importRunStatus: string;
  importRunEntitiesCreated: number;
  importRunEntitiesUpdated: number;
}

// Export types
export interface DdexExportRequest {
  exportReleaseId: number;
  exportPartnerId: number;
  exportProfile: string;
}

export interface DdexExportDTO {
  ddexExportId: number;
  ddexExportReleaseId: number;
  ddexExportPartnerId: number;
  ddexExportStatus: string;
  ddexExportXmlChecksum: string;
  ddexExportCreatedAt: string;
}

// Partner types
export interface DdexPartnerDTO {
  ddexPartnerId: number;
  ddexPartnerName: string;
  ddexPartnerDpid: string | null;
  ddexPartnerAllowedVersions: string[];
}

export interface DdexPartnerCreateRequest {
  partnerName: string;
  partnerDpid: string | null;
  partnerAllowedVersions: string[];
}

// Catalog types
export interface CatalogReleaseDTO {
  catalogReleaseDtoId: number;
  catalogReleaseDtoTitle: string;
  catalogReleaseDtoType: string;
  catalogReleaseDtoUpc: string | null;
  catalogReleaseDtoReleaseDate: string | null;
}

// Document status helpers
export const DdexDocumentStatus = {
  Received: 'received',
  Quarantined: 'quarantined',
  Queued: 'queued',
  Validating: 'validating',
  Invalid: 'invalid',
  Valid: 'valid',
  MappingRequired: 'mapping_required',
  ReadyToImport: 'ready_to_import',
  Importing: 'importing',
  Imported: 'imported',
  ImportFailed: 'import_failed',
  Superseded: 'superseded',
} as const;

export type DdexDocumentStatusType = typeof DdexDocumentStatus[keyof typeof DdexDocumentStatus];

export const getStatusColor = (status: string): 'default' | 'primary' | 'secondary' | 'error' | 'info' | 'success' | 'warning' => {
  switch (status) {
    case DdexDocumentStatus.Received:
    case DdexDocumentStatus.Queued:
      return 'info';
    case DdexDocumentStatus.Quarantined:
    case DdexDocumentStatus.Validating:
      return 'warning';
    case DdexDocumentStatus.Invalid:
    case DdexDocumentStatus.ImportFailed:
      return 'error';
    case DdexDocumentStatus.Valid:
    case DdexDocumentStatus.ReadyToImport:
      return 'success';
    case DdexDocumentStatus.Imported:
      return 'success';
    case DdexDocumentStatus.MappingRequired:
      return 'secondary';
    default:
      return 'default';
  }
};

// API client
export const DDEX = {
  // Documents
  listDocuments: (status?: string, partner?: string) => {
    const params = new URLSearchParams();
    if (status) params.append('status', status);
    if (partner) params.append('partner', partner);
    const query = params.toString();
    return get<DdexDocumentDTO[]>(`/ddex/documents${query ? `?${query}` : ''}`);
  },

  getDocument: (id: number) =>
    get<DdexDocumentDTO>(`/ddex/documents/${id}`),

  uploadDocument: (payload: DdexUploadRequest) =>
    post<DdexDocumentDTO>('/ddex/documents', payload),

  downloadRaw: (id: number) =>
    get<DdexDownloadResponse>(`/ddex/documents/${id}/raw`),

  // Validation
  validateDocument: (id: number) =>
    post<ValidationRunDTO>(`/ddex/documents/${id}/validation-runs`, {}),

  getValidationReport: (id: number) =>
    get<ValidationReportDTO>(`/ddex/documents/${id}/validation-runs/latest`),

  // Preview
  getPreview: (id: number) =>
    get<DdexPreviewDTO>(`/ddex/documents/${id}/preview`),

  // Import plans
  createImportPlan: (id: number) =>
    post<ImportPlanDTO>(`/ddex/documents/${id}/import-plans`, {}),

  resolveImportPlan: (id: number, resolution: ImportPlanResolution) =>
    patch<ImportPlanDTO>(`/ddex/import-plans/${id}`, resolution),

  commitImportPlan: (id: number) =>
    post<ImportRunDTO>(`/ddex/import-plans/${id}/commit`, {}),

  // Exports
  createExport: (payload: DdexExportRequest) =>
    post<DdexExportDTO>('/ddex/exports', payload),

  downloadExport: (id: number) =>
    get<DdexDownloadResponse>(`/ddex/exports/${id}/download`),

  // Partners
  listPartners: () =>
    get<DdexPartnerDTO[]>('/ddex/partners'),

  createPartner: (payload: DdexPartnerCreateRequest) =>
    post<DdexPartnerDTO>('/ddex/partners', payload),

  // Catalog
  listCatalogReleases: (ddexDocumentId?: number) => {
    const query = ddexDocumentId ? `?ddex_document_id=${ddexDocumentId}` : '';
    return get<CatalogReleaseDTO[]>(`/ddex/catalog/releases${query}`);
  },
};
