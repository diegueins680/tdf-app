import { del, get, post, put } from './client';
import { IncrementalSha256 } from '../utils/incrementalSha256';
import { parseMusicMultipartCompletion } from './musicStorageResponse';

export type MusicAssetRole = 'master_audio' | 'cover_original' | 'rights_evidence';
export type MusicReleaseKind = 'single' | 'ep' | 'album';
export type MusicReleaseState =
  | 'draft' | 'uploading' | 'processing' | 'validation_failed' | 'ready_for_review'
  | 'in_review' | 'changes_requested' | 'approved' | 'scheduled' | 'published'
  | 'suspended' | 'cancelled' | 'replacement_pending' | 'takedown_scheduled' | 'withdrawn';

export interface MusicReleaseSummary {
  id: string;
  artistPartyId: number;
  slug: string;
  kind: MusicReleaseKind;
  publishedVersionId: string | null;
  withdrawnAt: string | null;
  versions: { id: string; number: number; state: MusicReleaseState; title: string; displayArtist: string; updatedAt: string }[];
}

export interface MusicValidationError { fieldPath: string; code: string; message: string }

export interface MusicReleaseVersion {
  id: string;
  releaseId: string;
  versionNumber: number;
  state: MusicReleaseState;
  title: string;
  subtitle: string | null;
  versionTitle: string | null;
  displayArtist: string;
  titleLanguage: string;
  titleScript: string | null;
  primaryGenreId: string | null;
  secondaryGenreId: string | null;
  primaryGenre: string | null;
  secondaryGenre: string | null;
  explicitContent: 'not_explicit' | 'explicit' | 'cleaned' | 'unknown';
  originalReleaseDate: string | null;
  releaseAtUtc: string | null;
  releaseTimezone: string | null;
  embargoUntilUtc: string | null;
  labelName: string | null;
  catalogNumber: string | null;
  recordingCopyrightText: string | null;
  workCopyrightText: string | null;
  validation: { metadata: boolean; assets: boolean; rights: boolean; access: boolean; errors: MusicValidationError[] };
  tracks: {
    trackId: string; recordingId: string; discNumber: number; trackNumber: number;
    displayArtist: string; isPrimaryResource: boolean; previewStartMs: number | null;
    previewDurationMs: number | null; title: string; subtitle: string | null;
    versionTitle: string | null; titleLanguage: string; titleScript: string | null;
    durationMs: number | null; explicitContent: 'not_explicit' | 'explicit' | 'cleaned' | 'unknown';
  }[];
  parties: unknown[];
  credits: unknown[];
  identifiers: unknown[];
  rights: unknown[];
  availability: unknown[];
  terms: unknown[];
  assets: {
    id: string; recordingId: string | null; parentAssetId: string | null; role: string;
    originalFilename: string | null; mediaType: string; byteSize: number; sha256: string;
    processingState: string; technicalMetadata: Record<string, unknown>; createdAt: string; readyAt: string | null;
  }[];
  comments: {
    id: string; parentCommentId: string | null; fieldPath: string | null; body: string;
    visibility: string; resolutionState: string; createdBy: number; createdAt: string;
    resolvedBy: number | null; resolvedAt: string | null;
  }[];
  updatedAt: string;
}

export interface MusicReleaseCreateInput {
  artistPartyId: number;
  canonicalSlug: string;
  releaseKind: MusicReleaseKind;
  title: string;
  displayArtist: string;
  titleLanguage: string;
}

export interface MusicReleaseCreated {
  id: string;
  versionId: string;
  state: MusicReleaseState;
  slug: string;
  kind: MusicReleaseKind;
  title: string;
  displayArtist: string;
  updatedAt: string;
}

export interface MusicPublicRelease {
  id: string;
  artistPartyId: number;
  slug: string;
  kind: MusicReleaseKind;
  versionId: string;
  versionNumber: number;
  title: string;
  subtitle: string | null;
  versionTitle: string | null;
  displayArtist: string;
  explicitContent: string;
  originalReleaseDate: string | null;
  releaseAtUtc: string | null;
  labelName: string | null;
  catalogNumber: string | null;
  publishedAt: string;
  tracks: {
    trackId: string; recordingId: string; discNumber: number; trackNumber: number;
    title: string; displayArtist: string; durationMs: number; explicitContent: string;
    sources: { assetId: string; role: 'stream_audio' | 'preview_audio'; mediaType: string; technicalMetadata: Record<string, unknown> }[];
  }[];
  coverAssets: { assetId: string; role: string; mediaType: string; technicalMetadata: Record<string, unknown> }[];
  availability: {
    ruleId: string; trackId: string | null; territoryMode: string; territories: string[]; startsAt: string | null;
    endsAt: string | null; listeningPolicy: 'none' | 'preview' | 'full'; downloadPolicy: string;
    purchasable: boolean; priceMinor: number | null; currency: string | null;
  }[];
}

export interface MusicPublicReleaseSummary {
  id: string;
  artistPartyId: number;
  slug: string;
  kind: MusicReleaseKind;
  versionId: string;
  versionNumber: number;
  title: string;
  displayArtist: string;
  releaseAtUtc: string | null;
  labelName: string | null;
  publishedAt: string;
  coverAssetId: string | null;
}

export interface MusicAssetAccess {
  url: string;
  expiresAt: string;
  mediaType: string;
  byteSize: number;
  sha256: string;
  acceptRanges: boolean;
}

export interface MusicPurchase {
  id: string;
  releaseVersionId: string;
  availabilityRuleId: string;
  state: 'pending' | 'awaiting_payment' | 'paid' | 'cancelled' | 'refunded' | 'chargeback';
  grossMinor: number;
  currency: string;
  checkoutId: string;
  paidAt: string | null;
}

export interface MusicEntitlement {
  id: string;
  releaseVersionId: string;
  assetId: string;
  sourceKind: 'free_grant' | 'purchase' | 'staff_grant';
  status: 'active' | 'revoked' | 'refunded' | 'expired';
  maxDownloads: number | null;
  downloadCount: number;
}

export interface MusicLibraryTrack {
  recordingId: string;
  trackId: string | null;
  title: string;
  durationMs: number | null;
  available: boolean;
  releaseId: string | null;
  releaseVersionId: string | null;
  slug: string | null;
  displayArtist: string | null;
  sources: {
    assetId: string; role: 'stream_audio' | 'preview_audio'; mediaType: string;
    technicalMetadata: Record<string, unknown>;
  }[];
}

export interface MusicFavorite extends MusicLibraryTrack { createdAt: string }

export interface MusicPlaylistItem extends MusicLibraryTrack {
  id: string;
  position: number;
  addedAt: string;
}

export interface MusicPlaylist {
  id: string;
  name: string;
  visibility: 'private' | 'unlisted' | 'public';
  createdAt: string;
  updatedAt: string;
  items: MusicPlaylistItem[];
}

export interface MusicPlaybackHistoryEntry extends MusicLibraryTrack {
  positionMs: number;
  playCount: number;
  lastPlayedAt: string;
}

export interface MusicInfringementReport {
  id: string;
  releaseId: string;
  reasonCode: 'copyright' | 'master_rights' | 'composition_rights' | 'impersonation' | 'metadata' | 'other';
  description: string;
  status: 'received' | 'triage' | 'investigating' | 'actioned' | 'dismissed';
  reporterPartyId?: number;
  assignedTo?: number | null;
  resolutionNotes?: string | null;
  releaseTitle?: string | null;
  publishedVersionId?: string | null;
  createdAt: string;
  updatedAt: string;
  resolvedAt?: string | null;
}

export interface MusicReleaseAnalytics {
  releaseId: string;
  disclaimer: string;
  totals: MusicMetricTotals;
  daily: (MusicMetricTotals & { date: string })[];
  tracks: (MusicMetricTotals & { recordingId: string; title: string })[];
  territories: (MusicMetricTotals & { territoryCode: string })[];
}

export interface MusicMetricTotals {
    playStarts: number; eligiblePlays: number; completions: number; skips: number;
    listenedMs: number; uniqueListeners: number; purchases: number; downloads: number;
}

export interface MusicDdexParty {
  id: string; name: string; dpid: string; role: 'sender' | 'recipient' | 'both';
  verificationAuthority: string; verifiedBy: number; verifiedAt: string; active: boolean; createdAt: string;
}

export interface MusicDdexExport {
  id: string; releaseVersionId: string; operation: 'new_release' | 'update' | 'takedown';
  ernVersion: '4.3.2'; releaseProfile: 'Audio'; releaseProfileVersion: '2.3.1';
  businessProfileVersion: null; avsVersion: '011'; structuralDictionaryVersion: 'DD-ERN-432';
  choreographyVersion: '1.8.1'; senderDpid: string; recipientDpid: string; messageId: string;
  status: 'queued' | 'generating' | 'validation_failed' | 'valid' | 'failed';
  validationReport: Record<string, unknown>; packageSha256: string | null; generatedAt: string | null; createdAt: string;
}

export interface MusicReleaseMetadataDraft {
  expectedUpdatedAt: string;
  title: string;
  subtitle: string | null;
  versionTitle: string | null;
  displayArtist: string;
  titleLanguage: string;
  titleScript: string | null;
  primaryGenreId: string | null;
  secondaryGenreId: string | null;
  explicitContent: 'not_explicit' | 'explicit' | 'cleaned' | 'unknown';
  originalReleaseDate: string | null;
  releaseAtUtc: string | null;
  releaseTimezone: string | null;
  embargoUntilUtc: string | null;
  labelName: string | null;
  catalogNumber: string | null;
  recordingCopyrightText: string | null;
  workCopyrightText: string | null;
}

export interface MusicTrackDraftInput {
  clientRef: string;
  recordingId: string | null;
  title: string;
  subtitle: string | null;
  versionTitle: string | null;
  titleLanguage: string;
  titleScript: string | null;
  explicitContent: 'not_explicit' | 'explicit' | 'cleaned' | 'unknown';
  discNumber: number;
  trackNumber: number;
  displayArtist: string;
  isPrimaryResource: boolean;
  previewStartMs: number | null;
  previewDurationMs: number | null;
}

export interface MusicPartyDraftInput {
  clientRef: string;
  partyId: string | null;
  tdfPartyId: number | null;
  displayName: string;
  legalName: string | null;
  partyKind: 'person' | 'organization' | 'unknown';
  identifiers: { type: 'isni' | 'ipi' | 'dpid' | 'proprietary'; value: string }[];
}

export interface MusicReleaseContentInput {
  expectedUpdatedAt: string;
  tracks: MusicTrackDraftInput[];
  parties: MusicPartyDraftInput[];
  credits: { partyRef: string; trackRef: string | null; role: string; displayOrder: number; notes: string | null }[];
  identifiers: { trackRef: string | null; type: 'isrc' | 'upc' | 'ean' | 'grid' | 'proprietary'; value: string }[];
  rightsDeclarations: {
    trackRef: string | null; scope: 'master' | 'composition'; authorityBasis: string;
    territories: string[]; startsOn: string; endsOn: string | null;
    splits: { partyRef: string; basisPoints: number; territories: string[]; startsOn: string; endsOn: string | null }[];
  }[];
  availability: {
    trackRef: string | null; territoryMode: 'include' | 'exclude'; territories: string[];
    startsAt: string | null; endsAt: string | null; listeningPolicy: 'none' | 'preview' | 'full';
    downloadPolicy: 'none' | 'free' | 'purchase'; purchasable: boolean;
    priceMinor: number | null; currency: string | null; downloadableAssetId: string | null;
  }[];
}

export interface MusicUploadPart {
  partNumber: number;
  byteSize: number;
  etag: string;
  sha256: string;
  uploadedAt: string;
}

export interface MusicUploadSession {
  id: string;
  releaseVersionId: string;
  recordingId: string | null;
  assetRole: MusicAssetRole;
  status: 'initiated' | 'uploading' | 'completed' | 'cancelled' | 'failed' | 'expired';
  expectedSize: number;
  partSizeBytes: number;
  expiresAt: string;
  providerUploadIdBound: boolean;
  createMultipartUrl: string | null;
  parts: MusicUploadPart[];
}

export interface MusicUploadProgress {
  phase: 'hashing' | 'uploading' | 'finalizing';
  completedBytes: number;
  totalBytes: number;
  partNumber?: number;
}

export interface UploadMusicAssetOptions {
  releaseId: string;
  versionId: string;
  recordingId?: string | null;
  assetRole: MusicAssetRole;
  file: File;
  idempotencyKey: string;
  signal?: AbortSignal;
  onProgress?: (progress: MusicUploadProgress) => void;
}

interface SignedPart { url: string; partNumber: number }
interface CompletionInstruction { url: string; method: 'POST'; contentType: string; body: string }
interface CancelInstruction { abortUrl: string | null }

const apiPath = (path: string) => `/music${path}`;

const throwIfAborted = (signal?: AbortSignal) => {
  if (signal?.aborted) throw new DOMException('Upload cancelled', 'AbortError');
};

const hashBlob = async (
  blob: Blob,
  signal?: AbortSignal,
  onBytes?: (completed: number) => void,
): Promise<string> => {
  const digest = new IncrementalSha256();
  const chunkSize = 4 * 1024 * 1024;
  for (let offset = 0; offset < blob.size; offset += chunkSize) {
    throwIfAborted(signal);
    const chunk = new Uint8Array(await blob.slice(offset, offset + chunkSize).arrayBuffer());
    digest.update(chunk);
    onBytes?.(Math.min(offset + chunk.byteLength, blob.size));
  }
  return digest.hex();
};

const parseXmlElement = (body: string, localName: string): string | null => {
  const document = new DOMParser().parseFromString(body, 'application/xml');
  if (document.querySelector('parsererror')) return null;
  const candidates = Array.from(document.getElementsByTagName('*'));
  const value = candidates.find((element) => element.localName === localName)?.textContent?.trim();
  return value && value.length > 0 ? value : null;
};

const requestDirect = async (url: string, init: RequestInit): Promise<Response> => {
  const response = await fetch(url, { ...init, credentials: 'omit' });
  if (!response.ok) throw new Error(`El almacenamiento rechazó la operación (${response.status}).`);
  return response;
};

const uploadPart = (
  url: string,
  body: Blob,
  signal: AbortSignal | undefined,
  onProgress: (loaded: number) => void,
): Promise<string> => new Promise((resolve, reject) => {
  const xhr = new XMLHttpRequest();
  const abort = () => xhr.abort();
  xhr.open('PUT', url);
  xhr.upload.onprogress = (event) => onProgress(event.loaded);
  xhr.onerror = () => reject(new Error('La parte no pudo llegar al almacenamiento.'));
  xhr.onabort = () => reject(new DOMException('Upload cancelled', 'AbortError'));
  xhr.onload = () => {
    signal?.removeEventListener('abort', abort);
    if (xhr.status < 200 || xhr.status >= 300) {
      reject(new Error(`El almacenamiento rechazó la parte (${xhr.status}).`));
      return;
    }
    const etag = xhr.getResponseHeader('ETag')?.trim();
    if (!etag) {
      reject(new Error('El almacenamiento no expuso ETag; revisa ExposeHeaders en CORS.'));
      return;
    }
    resolve(etag);
  };
  signal?.addEventListener('abort', abort, { once: true });
  xhr.send(body);
});

export const uploadMusicAsset = async (options: UploadMusicAssetOptions): Promise<unknown> => {
  const { file, signal, onProgress } = options;
  throwIfAborted(signal);
  const expectedSha256 = await hashBlob(file, signal, (completedBytes) => {
    onProgress?.({ phase: 'hashing', completedBytes, totalBytes: file.size });
  });
  let session = await post<MusicUploadSession>(
    apiPath(`/releases/${options.releaseId}/versions/${options.versionId}/uploads`),
    {
      recordingId: options.recordingId ?? null,
      assetRole: options.assetRole,
      originalFilename: file.name,
      expectedMediaType: file.type || 'application/octet-stream',
      expectedSize: file.size,
      expectedSha256,
    },
    { headers: { 'Idempotency-Key': options.idempotencyKey }, signal },
  );

  try {
    if (!session.providerUploadIdBound) {
      if (!session.createMultipartUrl) throw new Error('La sesión no incluyó una URL para iniciar multipart.');
      const response = await requestDirect(session.createMultipartUrl, { method: 'POST', signal });
      const providerUploadId = parseXmlElement(await response.text(), 'UploadId');
      if (!providerUploadId) throw new Error('El almacenamiento no devolvió un UploadId válido.');
      session = await put<MusicUploadSession>(apiPath(`/uploads/${session.id}/provider`), { providerUploadId }, { signal });
    }

    const recorded = new Map(session.parts.map((part) => [part.partNumber, part]));
    const completedBeforeResume = session.parts.reduce((total, part) => total + part.byteSize, 0);
    let uploadedBytes = completedBeforeResume;
    const partCount = Math.ceil(file.size / session.partSizeBytes);
    for (let partNumber = 1; partNumber <= partCount; partNumber += 1) {
      throwIfAborted(signal);
      if (recorded.has(partNumber)) continue;
      const start = (partNumber - 1) * session.partSizeBytes;
      const blob = file.slice(start, Math.min(start + session.partSizeBytes, file.size));
      const partSha256 = await hashBlob(blob, signal);
      const signed = await get<SignedPart>(apiPath(`/uploads/${session.id}/parts/${partNumber}`), { signal });
      const etag = await uploadPart(signed.url, blob, signal, (loaded) => {
        onProgress?.({ phase: 'uploading', completedBytes: uploadedBytes + loaded, totalBytes: file.size, partNumber });
      });
      await put(apiPath(`/uploads/${session.id}/parts/${partNumber}`), {
        byteSize: blob.size,
        etag,
        sha256: partSha256,
      }, { signal });
      uploadedBytes += blob.size;
      onProgress?.({ phase: 'uploading', completedBytes: uploadedBytes, totalBytes: file.size, partNumber });
    }

    onProgress?.({ phase: 'finalizing', completedBytes: file.size, totalBytes: file.size });
    const completion = await get<CompletionInstruction>(apiPath(`/uploads/${session.id}/completion`), { signal });
    const completedResponse = await requestDirect(completion.url, {
      method: completion.method,
      headers: { 'Content-Type': completion.contentType },
      body: completion.body,
      signal,
    });
    const responseBody = await completedResponse.text();
    const finalEtag = parseMusicMultipartCompletion(responseBody, completedResponse.headers.get('ETag'));
    return post(apiPath(`/uploads/${session.id}/confirm`), { etag: finalEtag }, { signal });
  } catch (error) {
    if (signal?.aborted) {
      try {
        const cancellation = await del<CancelInstruction>(apiPath(`/uploads/${session.id}`));
        if (cancellation.abortUrl) await requestDirect(cancellation.abortUrl, { method: 'DELETE' });
      } catch {
        // The server-side cleanup worker remains responsible for abandoned multipart uploads.
      }
    }
    throw error;
  }
};

export const musicReleases = {
  listMine: (artistPartyId?: number) => get<MusicReleaseSummary[]>(apiPath(`/studio/releases${artistPartyId ? `?artistPartyId=${artistPartyId}` : ''}`)),
  listPublic: (query = '', artistPartyId?: number) => {
    const params = new URLSearchParams();
    if (query.trim()) params.set('q', query.trim());
    if (artistPartyId) params.set('artistPartyId', String(artistPartyId));
    const suffix = params.toString();
    return get<MusicPublicReleaseSummary[]>(apiPath(`/releases${suffix ? `?${suffix}` : ''}`));
  },
  getPublic: (slug: string) => get<MusicPublicRelease>(apiPath(`/releases/${encodeURIComponent(slug)}`)),
  getAssetAccess: (assetId: string) => get<MusicAssetAccess>(apiPath(`/assets/${assetId}/access`)),
  listFavorites: () => get<MusicFavorite[]>(apiPath('/favorites')),
  favorite: (recordingId: string) => put(apiPath('/favorites'), { recordingId }),
  unfavorite: (recordingId: string) => del(apiPath(`/favorites/${recordingId}`)),
  listPlaylists: () => get<MusicPlaylist[]>(apiPath('/playlists')),
  createPlaylist: (name: string, visibility: MusicPlaylist['visibility'] = 'private') => post<MusicPlaylist>(
    apiPath('/playlists'), { name, visibility },
  ),
  updatePlaylist: (playlistId: string, name: string, visibility: MusicPlaylist['visibility']) => put<MusicPlaylist>(
    apiPath(`/playlists/${playlistId}`), { name, visibility },
  ),
  deletePlaylist: (playlistId: string) => del(apiPath(`/playlists/${playlistId}`)),
  addPlaylistItem: (playlistId: string, recordingId: string, position: number) => post<MusicPlaylistItem>(
    apiPath(`/playlists/${playlistId}/items`), { recordingId, position },
  ),
  movePlaylistItem: (playlistId: string, itemId: string, position: number) => put<MusicPlaylistItem>(
    apiPath(`/playlists/${playlistId}/items/${itemId}`), { position },
  ),
  removePlaylistItem: (playlistId: string, itemId: string) => del(apiPath(`/playlists/${playlistId}/items/${itemId}`)),
  playbackHistory: () => get<MusicPlaybackHistoryEntry[]>(apiPath('/history')),
  reportInfringement: (releaseId: string, reasonCode: MusicInfringementReport['reasonCode'], description: string, idempotencyKey: string) => post<MusicInfringementReport>(
    apiPath('/infringement-reports'), { releaseId, reasonCode, description },
    { headers: { 'Idempotency-Key': idempotencyKey } },
  ),
  listInfringementReports: (releaseId?: string) => get<MusicInfringementReport[]>(
    apiPath(`/infringement-reports${releaseId ? `?releaseId=${encodeURIComponent(releaseId)}` : ''}`),
  ),
  actionInfringementReport: (reportId: string, input: {
    status: MusicInfringementReport['status']; notes: string; suspendVersionId: string | null;
  }) => put<MusicInfringementReport>(apiPath(`/infringement-reports/${reportId}`), input),
  freeDownload: (availabilityRuleId: string, requestId: string) => post<MusicAssetAccess>(
    apiPath('/downloads/free'), { availabilityRuleId, requestId },
  ),
  createPurchase: (availabilityRuleId: string, idempotencyKey: string) => post<MusicPurchase>(
    apiPath('/purchases'),
    { availabilityRuleId, territoryCode: 'ZZ', termsVersion: 'music-download-sale-2026-09-12' },
    { headers: { 'Idempotency-Key': idempotencyKey } },
  ),
  createPaypalOrder: (purchaseId: string, idempotencyKey: string) => post<{
    purchaseId: string; paypalOrderId: string; approvalUrl: string | null;
  }>(apiPath(`/purchases/${purchaseId}/paypal/create`), {}, { headers: { 'Idempotency-Key': idempotencyKey } }),
  capturePaypalOrder: (purchaseId: string, paypalOrderId: string, idempotencyKey: string) => post<MusicPurchase>(
    apiPath(`/purchases/${purchaseId}/paypal/capture`), { paypalOrderId },
    { headers: { 'Idempotency-Key': idempotencyKey } },
  ),
  listEntitlements: () => get<MusicEntitlement[]>(apiPath('/entitlements')),
  authorizeDownload: (entitlementId: string, requestId: string) => post<MusicAssetAccess>(
    apiPath(`/entitlements/${entitlementId}/download`), { requestId },
  ),
  create: (input: MusicReleaseCreateInput, idempotencyKey: string) => post<MusicReleaseCreated>(
    apiPath('/releases'), input, { headers: { 'Idempotency-Key': idempotencyKey } },
  ),
  getVersion: (releaseId: string, versionId: string) => get<MusicReleaseVersion>(
    apiPath(`/releases/${releaseId}/versions/${versionId}`),
  ),
  createCorrection: (releaseId: string, versionId: string, idempotencyKey: string) => post<MusicReleaseVersion>(
    apiPath(`/releases/${releaseId}/versions/${versionId}/corrections`), {},
    { headers: { 'Idempotency-Key': idempotencyKey } },
  ),
  saveMetadata: (releaseId: string, versionId: string, input: MusicReleaseMetadataDraft) => put<MusicReleaseVersion>(
    apiPath(`/releases/${releaseId}/versions/${versionId}/draft`), input,
  ),
  saveContent: (releaseId: string, versionId: string, input: MusicReleaseContentInput) => put<MusicReleaseVersion>(
    apiPath(`/releases/${releaseId}/versions/${versionId}/content`), input,
  ),
  acceptTerms: (releaseId: string, versionId: string, termsVersion: string) => post(
    apiPath(`/releases/${releaseId}/versions/${versionId}/terms`),
    { kind: 'publication_authority', version: termsVersion, accepted: true, evidence: { source: 'release_studio' } },
  ),
  validate: (releaseId: string, versionId: string) => post<{ valid: boolean; errors: MusicValidationError[] }>(
    apiPath(`/releases/${releaseId}/versions/${versionId}/validate`), {},
  ),
  transition: (
    releaseId: string,
    versionId: string,
    input: Record<string, unknown>,
    idempotencyKey: string,
  ) => post<MusicReleaseVersion>(
    apiPath(`/releases/${releaseId}/versions/${versionId}/transition`), input,
    { headers: { 'Idempotency-Key': idempotencyKey } },
  ),
  addComment: (releaseId: string, versionId: string, input: {
    parentId: string | null; fieldPath: string | null; body: string; staffOnly: boolean; requestChanges: boolean;
  }) => post(apiPath(`/releases/${releaseId}/versions/${versionId}/comments`), input),
  resolveComment: (releaseId: string, versionId: string, commentId: string) => post(
    apiPath(`/releases/${releaseId}/versions/${versionId}/comments/${commentId}/resolve`), {},
  ),
  analytics: (releaseId: string, from?: string, to?: string) => {
    const params = new URLSearchParams();
    if (from) params.set('from', from);
    if (to) params.set('to', to);
    return get<MusicReleaseAnalytics>(apiPath(`/analytics/releases/${releaseId}${params.size ? `?${params}` : ''}`));
  },
  listDdexParties: () => get<MusicDdexParty[]>(apiPath('/ddex/parties')),
  registerDdexParty: (input: {
    name: string; dpid: string; role: 'sender' | 'recipient' | 'both'; verificationAuthority: string;
    verificationEvidence: Record<string, unknown>;
  }) => post<MusicDdexParty>(apiPath('/ddex/parties'), input),
  listDdexExports: (releaseId: string, versionId: string) => get<MusicDdexExport[]>(
    apiPath(`/releases/${releaseId}/versions/${versionId}/ddex-exports`),
  ),
  createDdexExport: (releaseId: string, versionId: string, input: {
    operation: MusicDdexExport['operation']; senderRegistryId: string; recipientRegistryId: string;
  }, idempotencyKey: string) => post<MusicDdexExport>(
    apiPath(`/releases/${releaseId}/versions/${versionId}/ddex-exports`), input,
    { headers: { 'Idempotency-Key': idempotencyKey } },
  ),
  downloadDdexExport: (exportId: string) => get<MusicAssetAccess>(apiPath(`/ddex/exports/${exportId}/download`)),
  uploadAsset: uploadMusicAsset,
};
