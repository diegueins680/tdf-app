import { get } from '../api/client';

export interface PlayerAssetAuthorization {
  url: string;
  expiresAt: string;
  mediaType: string;
}

export const authorizePlayerAsset = (assetId: string, signal?: AbortSignal): Promise<PlayerAssetAuthorization> =>
  get<PlayerAssetAuthorization>(assetId.startsWith('legacy:')
    ? `/music/legacy/${encodeURIComponent(assetId.slice(7))}/access`
    : `/music/assets/${encodeURIComponent(assetId)}/access`, { signal });
