import { readFileSync } from 'node:fs';
import { availableChannel, channelLabel, detectPlatform, validateDistribution, type DistributionChannel, type MobileDistribution } from './distribution';
const verifiedAt = '2026-10-04T00:00:00Z';
const validUntil = '2026-10-11T00:00:00Z';
const now = Date.parse('2026-10-05T00:00:00Z');
const empty: DistributionChannel = { status: 'unavailable', verifiedAt, validUntil };
const ios: DistributionChannel = { status: 'testflight_external', url: 'https://testflight.apple.com/join/7k3VE2JJ', capacity: 'available', verifiedAt, validUntil };
const android: DistributionChannel = { status: 'open_testing', url: 'https://play.google.com/apps/testing/com.tdf.records', capacity: 'available', verifiedAt, validUntil };
describe('mobile distribution contract', () => {
  it('validates the deployed source of truth', () => { expect(() => validateDistribution(JSON.parse(readFileSync(new URL('../../public/mobile-distribution.json', 'file://' + process.cwd() + '/src/mobile/'), 'utf8')))).not.toThrow(); });
  it.each([['iPhone', 0, 'ios'], ['iPad', 0, 'ios'], ['Macintosh', 5, 'ios'], ['Android', 0, 'android'], ['Windows NT', 0, 'desktop'], ['', 0, 'desktop']])('detects %s without hiding alternatives', (ua, touch, expected) => expect(detectPlatform(String(ua), Number(touch))).toBe(expected));
  it('accepts verified beta links and distinguishes store copy', () => {
    expect(validateDistribution({ ios, android })).toEqual({ ios, android });
    expect(availableChannel(ios, now)).toBe(true);
    expect(channelLabel(ios, 'ios')).toBe('app.testflight');
    expect(channelLabel({ ...ios, status: 'public' }, 'ios')).toBe('app.storeIos');
  });
  it.each(['unavailable', 'coming_soon', 'testflight_internal'] as const)('never exposes %s as community admission', status => {
    expect(availableChannel({ ...empty, status }, now)).toBe(false);
    expect(() => validateDistribution({ ios: { ...ios, status }, android })).toThrow();
  });
  it('fails closed for expired, future, full and unconfirmed channels', () => {
    expect(availableChannel(ios, Date.parse(validUntil))).toBe(false);
    expect(availableChannel(ios, Date.parse(verifiedAt) - 1)).toBe(false);
    expect(availableChannel({ ...ios, capacity: 'full' }, now)).toBe(false);
    expect(availableChannel({ ...ios, capacity: 'unknown' }, now)).toBe(false);
  });
  it.each(['', 'javascript:alert(1)', 'https://testflight.apple.com.evil.test/join/abc', 'https://evil.test', 'https://user:secret@testflight.apple.com/join/abc', 'https://testflight.apple.com/join/abc?token=x', 'https://expo.dev/build/test'])('rejects unsafe or inconsistent iOS URL %s', url => expect(() => validateDistribution({ ios: { ...ios, url }, android })).toThrow());
  it('requires explicit closed-test admission and validates package identity', () => {
    expect(() => validateDistribution({ ios, android: { ...android, status: 'closed_testing' } })).toThrow();
    expect(() => validateDistribution({ ios, android: { ...android, url: 'https://play.google.com/apps/testing/com.other' } })).toThrow();
    expect(() => validateDistribution({ ios, android: { ...android, status: 'testflight_external' } })).toThrow();
  });
  it('accepts public destinations only for the correct product', () => {
    const config: MobileDistribution = { ios: { ...ios, status: 'public', url: 'https://apps.apple.com/app/id6779786470' }, android: { ...android, status: 'public', url: 'https://play.google.com/store/apps/details?id=com.tdf.records' } };
    expect(validateDistribution(config)).toEqual(config);
    expect(() => validateDistribution({ ...config, ios: { ...config.ios, url: 'https://apps.apple.com/app/id6754828747' } })).toThrow();
  });
});
