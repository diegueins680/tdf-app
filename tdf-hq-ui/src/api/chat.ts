import { get, post } from './client';
import type { ChatMessageDTO, ChatThreadDTO } from './types';

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

const normalizeOptionalPositiveInteger = (value: number | undefined): number | undefined => {
  if (typeof value !== 'number') return undefined;
  return Number.isSafeInteger(value) && value > 0 ? value : undefined;
};

function buildQuery(params: Record<string, string | number | undefined | null>) {
  const qs = new URLSearchParams();
  Object.entries(params).forEach(([key, value]) => {
    if (value === undefined || value === null) return;
    qs.set(key, String(value));
  });
  const raw = qs.toString();
  return raw ? `?${raw}` : '';
}

export const ChatAPI = {
  listThreads: () => get<ChatThreadDTO[]>('/chat/threads'),
  getOrCreateDmThread: (otherPartyId: number) =>
    post<ChatThreadDTO>(`/chat/threads/dm/${requirePositiveInteger(otherPartyId, 'otherPartyId')}`, {}),
  listMessages: (threadId: number, opts?: { limit?: number; beforeId?: number; afterId?: number }) =>
    get<ChatMessageDTO[]>(
      `/chat/threads/${requirePositiveInteger(threadId, 'threadId')}/messages${buildQuery({
        limit: normalizeOptionalPositiveInteger(opts?.limit),
        beforeId: normalizeOptionalPositiveInteger(opts?.beforeId),
        afterId: normalizeOptionalPositiveInteger(opts?.afterId),
      })}`,
    ),
  sendMessage: (threadId: number, body: string) => {
    const trimmedBody = body.trim();
    if (!trimmedBody) {
      throw new Error('El mensaje no puede estar vacío.');
    }
    return post<ChatMessageDTO>(`/chat/threads/${requirePositiveInteger(threadId, 'threadId')}/messages`, {
      csmBody: trimmedBody,
    });
  },
};
