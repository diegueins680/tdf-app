import type { ChatThreadDTO } from '../api/types';

export const CHAT_READ_STATE_STORAGE_KEY = 'tdf-chat-last-seen-v1';
export const CHAT_READ_STATE_EVENT = 'tdf-chat-read-state';

export type ChatReadMap = Record<string, string>;

const parseDateMs = (value: string | null | undefined): number | null => {
  if (!value) return null;
  const parsed = new Date(value);
  const ms = parsed.getTime();
  return Number.isNaN(ms) ? null : ms;
};

const readStorage = (): ChatReadMap => {
  if (typeof window === 'undefined') return {};
  try {
    const raw = window.localStorage.getItem(CHAT_READ_STATE_STORAGE_KEY);
    if (!raw) return {};
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object') return {};
    const next: ChatReadMap = {};
    Object.entries(parsed as Record<string, unknown>).forEach(([key, value]) => {
      if (typeof value !== 'string') return;
      if (!key || !/^\d+$/.test(key)) return;
      if (parseDateMs(value) === null) return;
      next[key] = value;
    });
    return next;
  } catch {
    return {};
  }
};

const writeStorage = (map: ChatReadMap) => {
  if (typeof window === 'undefined') return;
  try {
    window.localStorage.setItem(CHAT_READ_STATE_STORAGE_KEY, JSON.stringify(map));
  } catch {
    // ignore storage write issues
  }
};

export const loadChatReadMap = (): ChatReadMap => readStorage();

export const getThreadLastSeenAt = (map: ChatReadMap, threadId: number): string | null => {
  return map[String(threadId)] ?? null;
};

export const markThreadSeen = (threadId: number, lastSeenAt: string) => {
  if (typeof window === 'undefined') return;
  if (threadId <= 0) return;
  if (parseDateMs(lastSeenAt) === null) return;
  const map = readStorage();
  const key = String(threadId);
  const prevMs = parseDateMs(map[key]);
  const nextMs = parseDateMs(lastSeenAt);
  if (nextMs === null) return;
  if (prevMs !== null && nextMs <= prevMs) return;
  map[key] = lastSeenAt;
  writeStorage(map);
  window.dispatchEvent(new Event(CHAT_READ_STATE_EVENT));
};

export const isThreadUnread = (thread: ChatThreadDTO, map: ChatReadMap): boolean => {
  const lastMessageAtMs = parseDateMs(thread.ctLastMessageAt);
  if (lastMessageAtMs === null) return false;
  const lastSeenAt = getThreadLastSeenAt(map, thread.ctThreadId);
  const lastSeenAtMs = parseDateMs(lastSeenAt);
  if (lastSeenAtMs === null) return true;
  return lastMessageAtMs > lastSeenAtMs;
};

export const countUnreadThreads = (threads: ChatThreadDTO[], map: ChatReadMap): number => {
  return threads.reduce((acc, thread) => (isThreadUnread(thread, map) ? acc + 1 : acc), 0);
};

export const subscribeToChatReadState = (onChange: () => void): (() => void) => {
  if (typeof window === 'undefined') return () => undefined;
  const handle = () => onChange();
  const handleStorage = (event: StorageEvent) => {
    if (event.key === CHAT_READ_STATE_STORAGE_KEY) onChange();
  };
  window.addEventListener(CHAT_READ_STATE_EVENT, handle);
  window.addEventListener('storage', handleStorage);
  return () => {
    window.removeEventListener(CHAT_READ_STATE_EVENT, handle);
    window.removeEventListener('storage', handleStorage);
  };
};
