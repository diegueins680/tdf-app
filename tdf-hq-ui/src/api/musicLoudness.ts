// MEDIA-MUSIC-LOUDNESS-001 (formal/system/music-loudness-check.md). Types and
// the strict response check; the request itself is musicReleases.getLoudness.
import type { components } from './generated/types';

export type MusicLoudnessCheck = components['schemas']['MusicLoudnessCheck'];
export type MusicLoudnessTrack = components['schemas']['MusicLoudnessTrack'];

const STATUSES: readonly string[] = ['pass', 'warn', 'failed', 'pending', 'not_measured'];
const FLAGS: readonly string[] = ['too_loud', 'too_quiet', 'true_peak_over'];

const record = (item: unknown): item is Record<string, unknown> =>
  typeof item === 'object' && item !== null && !Array.isArray(item);
const integer = (item: unknown): item is number => typeof item === 'number' && Number.isSafeInteger(item);
const uuid = (item: unknown): item is string => typeof item === 'string' &&
  /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(item);

function validTrack(item: unknown): boolean {
  if (!record(item) || !uuid(item['recordingId'])) return false;
  const { masterAssetId, status, checked, flags, integratedLufsTenths, truePeakDbtpTenths, failureReason } = item;
  if (!(masterAssetId === null || uuid(masterAssetId))) return false;
  if (typeof status !== 'string' || !STATUSES.includes(status) || typeof checked !== 'boolean') return false;
  if (!Array.isArray(flags) || !flags.every((flag) => typeof flag === 'string' && FLAGS.includes(flag))) return false;
  if (!(failureReason === null || typeof failureReason === 'string')) return false;
  const decided = status === 'pass' || status === 'warn';
  // The server reports values and "checked" only together with a decision on
  // this exact master; anything else is not a result the editor may show.
  if (checked !== decided) return false;
  if (decided !== (integer(integratedLufsTenths) && integer(truePeakDbtpTenths))) return false;
  if (!decided && (integratedLufsTenths !== null || truePeakDbtpTenths !== null)) return false;
  if ((status === 'warn') !== (flags.length > 0)) return false;
  return decided ? masterAssetId !== null && failureReason === null : true;
}

export function parseMusicLoudnessCheck(value: unknown): MusicLoudnessCheck {
  const limits = record(value) ? value['limits'] : null;
  const tracks = record(value) ? value['tracks'] : null;
  const valid = record(limits) && integer(limits['integratedLufsTenths']) &&
    integer(limits['toleranceTenths']) && limits['toleranceTenths'] >= 0 &&
    integer(limits['truePeakCeilingDbtpTenths']) &&
    Array.isArray(tracks) && tracks.length <= 1000 && tracks.every(validTrack);
  if (!valid) throw new Error('Invalid music loudness response.');
  return value as MusicLoudnessCheck;
}
