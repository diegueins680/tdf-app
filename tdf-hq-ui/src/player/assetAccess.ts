import { get } from '../api/client';

export interface PlayerAssetAuthorization {
  url: string;
  expiresAt: string;
  mediaType: string;
}

export const authorizePlayerAsset = (assetId: string): Promise<PlayerAssetAuthorization> =>
  get<PlayerAssetAuthorization>(`/music/assets/${encodeURIComponent(assetId)}/access`);
