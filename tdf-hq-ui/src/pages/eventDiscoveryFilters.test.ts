import {
  EVENT_PAGE_SIZE, eventArtistIds, eventForYouOrder, eventHasNextPage, eventPage, eventPageLimit, isPastEvent,
} from './eventDiscoveryFilters';
import { isEventAffinityOrderEnabled } from '../config/eventAffinityRollout';

it('normalizes canonical artist identities without confusing malformed or overflowing IDs', () => {
  expect(eventArtistIds('7, 8,7,0,-1,abc,9223372036854775808')).toEqual(['7', '8']);
  const ids = eventArtistIds('7, 8,7');
  expect(eventArtistIds(ids.join(','))).toEqual(ids);
  expect(eventArtistIds(Array.from({ length: 51 }, (_, i) => String(i + 1)).join(','))).toHaveLength(50);
});

it('bounds offset navigation to the existing API contract', () => {
  expect(eventPage(null)).toBe(1);
  expect(eventPage('501')).toBe(501);
  for (const raw of ['0', '-1', '502', '2.5', 'NaN']) expect(eventPage(raw)).toBe(1);
});

it('treats a known ongoing event as current across timezone representations', () => {
  const now = Date.parse('2026-11-04T20:00:00Z');
  expect(isPastEvent({ eventStart: '2026-11-04T14:00:00-05:00', eventEnd: '2026-11-04T16:00:00-05:00' }, now)).toBe(false);
  expect(isPastEvent({ eventStart: '2026-11-04T14:00:00-05:00' }, now)).toBe(true);
  expect(isPastEvent({ eventStart: '2026-11-04T21:00:00Z' }, now)).toBe(false);
});

it('asks for exactly one page when the server permutes it, and keeps the look-ahead row otherwise', () => {
  expect(eventPageLimit(false)).toBe(EVENT_PAGE_SIZE + 1);
  expect(eventPageLimit(true)).toBe(EVENT_PAGE_SIZE);
  expect(eventHasNextPage(false, EVENT_PAGE_SIZE)).toBe(false);
  expect(eventHasNextPage(false, EVENT_PAGE_SIZE + 1)).toBe(true);
  expect(eventHasNextPage(true, EVENT_PAGE_SIZE - 1)).toBe(false);
  expect(eventHasNextPage(true, EVENT_PAGE_SIZE)).toBe(true);
});

it('orders for the listener only with the build flag, a session and the exact parameter', () => {
  expect(eventForYouOrder('for_you', true, true)).toBe(true);
  expect(eventForYouOrder('for_you', false, true)).toBe(false);
  expect(eventForYouOrder('for_you', true, false)).toBe(false);
  expect(eventForYouOrder(null, true, true)).toBe(false);
  expect(eventForYouOrder('FOR_YOU', true, true)).toBe(false);
  expect(isEventAffinityOrderEnabled(undefined)).toBe(false);
  expect(isEventAffinityOrderEnabled('1')).toBe(false);
  expect(isEventAffinityOrderEnabled('true')).toBe(true);
});
