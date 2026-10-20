import { get, post, put } from './client';
import type { VideoCampaign, VideoCampaignDocument } from '../features/campaigns/videoCampaign';

export const VideoCampaigns = {
  list: (artistId: number) => get<VideoCampaign[]>(`/artists/video-campaigns?artistId=${artistId}`),
  save: (artistId: number, id: string, expectedRevision: number, document: VideoCampaignDocument) =>
    put<VideoCampaign>(`/artists/video-campaigns/${encodeURIComponent(id)}?artistId=${artistId}`,
      { expectedRevision, document }),
  metrics: (artistId: number, id: string) => get<{ registrations: number; follows: number }>(
    `/artists/video-campaigns/${encodeURIComponent(id)}/metrics?artistId=${artistId}`),
  conversion: (artistId: number, id: string, kind: 'registration' | 'follow', source: string, piece: string) =>
    post(`/artists/video-campaigns/${encodeURIComponent(id)}/conversion?artistId=${artistId}`, { kind, source, piece }),
};
