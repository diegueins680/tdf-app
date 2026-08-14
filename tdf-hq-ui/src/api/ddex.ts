import { get, post } from './client';
import type { components } from './generated/types';

export type DdexDocumentDTO = components['schemas']['DdexDocument'];
export type ValidationReportDTO = components['schemas']['DdexValidationReport'];
export type DdexPartnerDTO = components['schemas']['DdexPartner'];
export type DdexPartnerCreateRequest = components['schemas']['DdexPartnerCreateRequest'];
export type DdexReferenceSnapshotDTO = components['schemas']['DdexReferenceSnapshot'];

// API client
export const DDEX = {
  getReferences: (locale?: string) => {
    const query = locale ? `?locale=${encodeURIComponent(locale)}` : '';
    return get<DdexReferenceSnapshotDTO>(`/ddex/references${query}`);
  },

  // Documents
  listDocuments: (workflowStateId?: string) => {
    const params = new URLSearchParams();
    if (workflowStateId) params.append('workflowStateId', workflowStateId);
    const query = params.toString();
    return get<DdexDocumentDTO[]>(`/ddex/documents${query ? `?${query}` : ''}`);
  },

  getDocument: (id: number) =>
    get<DdexDocumentDTO>(`/ddex/documents/${id}`),

  getValidationReport: (id: number) =>
    get<ValidationReportDTO>(`/ddex/documents/${id}/validation-runs/latest`),

  // Partners
  listPartners: () =>
    get<DdexPartnerDTO[]>('/ddex/partners'),

  createPartner: (payload: DdexPartnerCreateRequest) =>
    post<DdexPartnerDTO>('/ddex/partners', payload),
};
