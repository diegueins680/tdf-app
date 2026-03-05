import type { ChatThreadDTO } from '../api/types';
import { jest } from '@jest/globals';
import {
  CHAT_READ_STATE_EVENT,
  CHAT_READ_STATE_STORAGE_KEY,
  isThreadUnread,
  loadChatReadMap,
  markThreadSeen,
  subscribeToChatReadState,
} from './chatReadState';

const VALID_TIMESTAMP = '2026-03-05T14:00:00.000Z';

const buildThread = (overrides: Partial<ChatThreadDTO> = {}): ChatThreadDTO => ({
  ctThreadId: 10,
  ctOtherPartyId: 20,
  ctOtherDisplayName: 'Test User',
  ctLastMessage: 'hello',
  ctLastMessageAt: VALID_TIMESTAMP,
  ctUpdatedAt: VALID_TIMESTAMP,
  ...overrides,
});

describe('chatReadState', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('stores last seen timestamps for valid positive safe integer thread ids', () => {
    markThreadSeen(42, VALID_TIMESTAMP);

    expect(loadChatReadMap()).toEqual({ '42': VALID_TIMESTAMP });
  });

  it('ignores invalid thread ids when marking read state', () => {
    const invalidIds = [0, -1, Number.NaN, Number.POSITIVE_INFINITY, 1.25];

    invalidIds.forEach((threadId) => {
      markThreadSeen(threadId, VALID_TIMESTAMP);
    });

    expect(window.localStorage.getItem(CHAT_READ_STATE_STORAGE_KEY)).toBeNull();
    expect(loadChatReadMap()).toEqual({});
  });

  it('filters malformed persisted keys and timestamps from storage', () => {
    window.localStorage.setItem(
      CHAT_READ_STATE_STORAGE_KEY,
      JSON.stringify({
        '11': VALID_TIMESTAMP,
        '9007199254740992': VALID_TIMESTAMP,
        'nan': VALID_TIMESTAMP,
        '12': 'not-a-date',
      }),
    );

    expect(loadChatReadMap()).toEqual({ '11': VALID_TIMESTAMP });
  });

  it('treats threads with invalid ids as read-safe (not unread)', () => {
    const thread = buildThread({ ctThreadId: Number.NaN });

    expect(isThreadUnread(thread, {})).toBe(false);
  });

  it('notifies subscribers for custom chat read-state events', () => {
    const onChange = jest.fn();
    const unsubscribe = subscribeToChatReadState(onChange);

    window.dispatchEvent(new Event(CHAT_READ_STATE_EVENT));

    expect(onChange).toHaveBeenCalledTimes(1);
    unsubscribe();
  });

  it('notifies subscribers for storage updates on chat read-state key', () => {
    const onChange = jest.fn();
    const unsubscribe = subscribeToChatReadState(onChange);

    window.dispatchEvent(new StorageEvent('storage', { key: CHAT_READ_STATE_STORAGE_KEY }));

    expect(onChange).toHaveBeenCalledTimes(1);
    unsubscribe();
  });

  it('notifies subscribers for storage clear events (null storage key)', () => {
    const onChange = jest.fn();
    const unsubscribe = subscribeToChatReadState(onChange);

    window.dispatchEvent(new StorageEvent('storage', { key: null }));

    expect(onChange).toHaveBeenCalledTimes(1);
    unsubscribe();
  });

  it('ignores storage events for unrelated keys', () => {
    const onChange = jest.fn();
    const unsubscribe = subscribeToChatReadState(onChange);

    window.dispatchEvent(new StorageEvent('storage', { key: 'another-key' }));

    expect(onChange).not.toHaveBeenCalled();
    unsubscribe();
  });
});
