import type { components } from './generated/types';
import { get, patch, post, postEmpty, postText } from './client';

export type CatalogDefinition = components['schemas']['CatalogDefinition'];
export type CatalogItem = components['schemas']['CatalogItem'];
export type CatalogDefault = components['schemas']['CatalogDefault'];
export type CatalogPage = components['schemas']['CatalogPage'];
export type CatalogBatch = components['schemas']['CatalogBatch'];
export type ContentType = components['schemas']['ContentType'];
export type WorkflowState = components['schemas']['WorkflowState'];
export type WorkflowStates = components['schemas']['WorkflowStates'];
export type AuthoredContent = components['schemas']['AuthoredContent'];
export type CatalogDraft = components['schemas']['CatalogDraft'];
export type CatalogRevision = components['schemas']['CatalogRevision'];
export type CatalogReview = components['schemas']['CatalogReview'];
export type CatalogActivation = components['schemas']['CatalogActivation'];
export type CatalogReorder = components['schemas']['CatalogReorder'];
export type CatalogMerge = components['schemas']['CatalogMerge'];
export type CatalogUsage = components['schemas']['CatalogUsage'];
export type CatalogImportResult = components['schemas']['CatalogImportResult'];

export interface CatalogPageQuery {
  locale?: string;
  q?: string;
  page?: number;
  pageSize?: number;
  includeInactive?: boolean;
}

const appendOptional = (params: URLSearchParams, key: string, value: string | number | boolean | undefined) => {
  if (value !== undefined) params.set(key, String(value));
};

const querySuffix = (params: URLSearchParams) => {
  const encoded = params.toString();
  return encoded ? `?${encoded}` : '';
};

const catalogPath = (catalogCode: string) => `/catalog/${encodeURIComponent(catalogCode)}`;

export const Catalogs = {
  listPublicBatch: (catalogCodes: string[], query: CatalogPageQuery = {}) => {
    const params = new URLSearchParams();
    [...new Set(catalogCodes.map((code) => code.trim()).filter(Boolean))]
      .forEach((code) => params.append('code', code));
    appendOptional(params, 'locale', query.locale);
    appendOptional(params, 'q', query.q);
    appendOptional(params, 'page', query.page);
    appendOptional(params, 'pageSize', query.pageSize);
    return get<CatalogBatch>(`/catalogs/batch${querySuffix(params)}`);
  },

  listDefinitions: (locale?: string) => {
    const params = new URLSearchParams();
    appendOptional(params, 'locale', locale);
    return get<CatalogDefinition[]>(`/catalog/definitions${querySuffix(params)}`);
  },

  listContentTypes: (locale?: string) => {
    const params = new URLSearchParams();
    appendOptional(params, 'locale', locale);
    return get<ContentType[]>(`/catalog/content-types${querySuffix(params)}`);
  },

  listWorkflowStates: (workflowCode?: string, locale?: string) => {
    const params = new URLSearchParams();
    appendOptional(params, 'workflowCode', workflowCode);
    appendOptional(params, 'locale', locale);
    return get<WorkflowState[]>(`/catalog/workflow-states${querySuffix(params)}`);
  },

  getPublicWorkflowStates: (workflowCode: string, locale?: string) => {
    const params = new URLSearchParams();
    appendOptional(params, 'locale', locale);
    return get<WorkflowStates>(
      `/catalogs/workflows/${encodeURIComponent(workflowCode)}/states${querySuffix(params)}`,
    );
  },

  listAuthoredContents: (locale?: string) => {
    const params = new URLSearchParams();
    appendOptional(params, 'locale', locale);
    return get<AuthoredContent[]>(`/catalog/authored-contents${querySuffix(params)}`);
  },

  listItems: (catalogCode: string, query: CatalogPageQuery = {}) => {
    const params = new URLSearchParams();
    appendOptional(params, 'locale', query.locale);
    appendOptional(params, 'q', query.q);
    appendOptional(params, 'page', query.page);
    appendOptional(params, 'pageSize', query.pageSize);
    appendOptional(params, 'includeInactive', query.includeInactive);
    return get<CatalogPage>(`${catalogPath(catalogCode)}/items${querySuffix(params)}`);
  },

  getItem: (catalogCode: string, itemId: string, locale?: string) => {
    const params = new URLSearchParams();
    appendOptional(params, 'locale', locale);
    return get<CatalogItem>(
      `${catalogPath(catalogCode)}/items/${encodeURIComponent(itemId)}${querySuffix(params)}`,
    );
  },

  listRevisions: (catalogCode: string, page = 1, pageSize = 50) => {
    const params = new URLSearchParams({ page: String(page), pageSize: String(pageSize) });
    return get<CatalogRevision[]>(`${catalogPath(catalogCode)}/revisions?${params}`);
  },

  createRevision: (catalogCode: string, draft: CatalogDraft) =>
    post<CatalogRevision>(`${catalogPath(catalogCode)}/revisions`, draft),

  submitRevision: (revisionId: string) =>
    postEmpty<CatalogRevision>(`/catalog/revisions/${encodeURIComponent(revisionId)}/submit`),

  approveRevision: (revisionId: string, review: CatalogReview) =>
    post<CatalogRevision>(`/catalog/revisions/${encodeURIComponent(revisionId)}/approve`, review),

  rejectRevision: (revisionId: string, review: CatalogReview) =>
    post<CatalogRevision>(`/catalog/revisions/${encodeURIComponent(revisionId)}/reject`, review),

  setActivation: (catalogCode: string, itemId: string, activation: CatalogActivation) =>
    patch<CatalogItem>(
      `${catalogPath(catalogCode)}/items/${encodeURIComponent(itemId)}/activation`,
      activation,
    ),

  reorder: (catalogCode: string, ordering: CatalogReorder) =>
    post<void>(`${catalogPath(catalogCode)}/reorder`, ordering),

  merge: (catalogCode: string, merge: CatalogMerge) =>
    post<CatalogRevision>(`${catalogPath(catalogCode)}/merge`, merge),

  getUsage: (catalogCode: string, from?: string, to?: string) => {
    const params = new URLSearchParams();
    appendOptional(params, 'from', from);
    appendOptional(params, 'to', to);
    return get<CatalogUsage[]>(`${catalogPath(catalogCode)}/usage${querySuffix(params)}`);
  },

  exportCsv: (catalogCode: string) => get<string>(`${catalogPath(catalogCode)}/export.csv`),

  importCsv: (catalogCode: string, csv: string, dryRun = true) =>
    postText<CatalogImportResult>(
      `${catalogPath(catalogCode)}/import.csv?dryRun=${String(dryRun)}`,
      csv,
    ),
};
