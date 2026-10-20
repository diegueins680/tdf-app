import { VideoCampaigns } from '../../api/videoCampaigns';

export function videoAttribution(search: string) {
  const params = new URLSearchParams(search);
  if (['utm_campaign', 'utm_source', 'utm_content'].some((key) => params.getAll(key).length !== 1)) return null;
  const campaign = params.get('utm_campaign') ?? '';
  const source = params.get('utm_source') ?? '';
  const piece = params.get('utm_content') ?? '';
  if (!/^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/i.test(campaign)
    || !['instagram', 'tiktok'].includes(source) || !/^[a-zA-Z0-9_-]{1,100}$/.test(piece)) return null;
  return { campaign, source, piece };
}

// Server validates real account/follow creation time and deduplicates by account + campaign.
export async function recordVideoConversion(search: string, artistId: number, kind: 'registration' | 'follow') {
  const attribution = videoAttribution(search);
  if (!attribution) return;
  await VideoCampaigns.conversion(artistId, attribution.campaign, kind, attribution.source, attribution.piece);
}
