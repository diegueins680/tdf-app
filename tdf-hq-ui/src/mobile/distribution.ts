export type MobilePlatform = 'ios' | 'android';
export type DistributionStatus = 'unavailable' | 'coming_soon' | 'internal_testing' | 'closed_testing' | 'open_testing' | 'testflight_internal' | 'testflight_external' | 'store_preorder' | 'public';
export interface DistributionChannel {
  status: DistributionStatus;
  url?: string;
  enrollmentUrl?: string;
  admission?: 'direct' | 'approval_required';
  capacity?: 'available' | 'full' | 'unknown';
  verifiedAt: string;
  validUntil: string;
  version?: string;
  build?: string;
}
export interface MobileDistribution { ios: DistributionChannel; android: DistributionChannel }
export const MOBILE_CANONICAL_URL = 'https://www.tdfrecords.net/app';
const statuses = new Set<DistributionStatus>(['unavailable', 'coming_soon', 'internal_testing', 'closed_testing', 'open_testing', 'testflight_internal', 'testflight_external', 'store_preorder', 'public']);
const accessible = new Set<DistributionStatus>(['closed_testing', 'open_testing', 'testflight_external', 'store_preorder', 'public']);
export function validateDistribution(value: unknown): MobileDistribution {
  if (!value || typeof value !== 'object') throw new Error('Invalid mobile distribution');
  for (const platform of ['ios', 'android'] as const) {
    const c = (value as MobileDistribution)[platform];
    if (!c || !statuses.has(c.status) || !Number.isFinite(Date.parse(c.verifiedAt)) || !Number.isFinite(Date.parse(c.validUntil)) || Date.parse(c.validUntil) <= Date.parse(c.verifiedAt)) throw new Error('Invalid distribution state');
    if (platform === 'ios' && ['internal_testing', 'closed_testing', 'open_testing'].includes(c.status)) throw new Error('Wrong iOS channel');
    if (platform === 'android' && c.status.startsWith('testflight')) throw new Error('Wrong Android channel');
    if (accessible.has(c.status) !== Boolean(c.url)) throw new Error('Channel URL contradicts status');
    if (c.capacity && !['available', 'full', 'unknown'].includes(c.capacity)) throw new Error('Invalid capacity');
    if (c.url) {
      const u = new URL(c.url);
      if (u.protocol !== 'https:' || u.username || u.password || u.port || u.hash) throw new Error('Unsafe distribution URL');
      const valid = platform === 'ios'
        ? c.status === 'testflight_external' ? u.hostname === 'testflight.apple.com' && /^\/join\/[a-zA-Z0-9]+$/.test(u.pathname) && !u.search
          : u.hostname === 'apps.apple.com' && u.pathname.endsWith('/id6779786470')
        : u.hostname === 'play.google.com' && (c.status === 'public' || c.status === 'store_preorder'
          ? u.pathname === '/store/apps/details' && u.searchParams.get('id') === 'com.tdf.records'
          : u.pathname === '/apps/testing/com.tdf.records');
      if (!valid) throw new Error('Unexpected distribution destination');
    }
    if (c.status === 'closed_testing' && !['direct', 'approval_required'].includes(c.admission ?? '')) throw new Error('Closed testing requires admission policy');
    if (c.enrollmentUrl) {
      const u = new URL(c.enrollmentUrl);
      if (c.status !== 'closed_testing' || u.protocol !== 'https:' || u.hostname !== 'groups.google.com' || !/^\/g\/[\w-]+$/.test(u.pathname) || u.search || u.hash || u.username || u.password || u.port) throw new Error('Unsafe enrollment URL');
    }
  }
  return value as MobileDistribution;
}
export function availableChannel(channel: DistributionChannel, now = Date.now()): boolean {
  return accessible.has(channel.status) && Boolean(channel.url) && Date.parse(channel.verifiedAt) <= now && now < Date.parse(channel.validUntil) && (channel.status === 'public' || channel.status === 'store_preorder' || channel.capacity === 'available');
}
export function detectPlatform(userAgent = '', touchPoints = 0): MobilePlatform | 'desktop' {
  if (/android/i.test(userAgent)) return 'android';
  if (/iPad|iPhone|iPod/i.test(userAgent) || (userAgent.includes('Macintosh') && touchPoints > 1)) return 'ios';
  return 'desktop';
}
export function channelLabel(channel: DistributionChannel, platform: MobilePlatform): string {
  if (channel.status === 'public') return platform === 'ios' ? 'app.storeIos' : 'app.storeAndroid';
  if (channel.status === 'store_preorder') return 'app.preorder';
  return platform === 'ios' ? 'app.testflight' : 'app.testAndroid';
}
