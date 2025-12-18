import { get, post } from './client';
import type { ChatMessageDTO, ChatThreadDTO } from './types';

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
  getOrCreateDmThread: (otherPartyId: number) => post<ChatThreadDTO>(`/chat/threads/dm/${otherPartyId}`, {}),
  listMessages: (threadId: number, opts?: { limit?: number; beforeId?: number; afterId?: number }) =>
    get<ChatMessageDTO[]>(
      `/chat/threads/${threadId}/messages${buildQuery({
        limit: opts?.limit,
        beforeId: opts?.beforeId,
        afterId: opts?.afterId,
      })}`,
    ),
  sendMessage: (threadId: number, body: string) =>
    post<ChatMessageDTO>(`/chat/threads/${threadId}/messages`, { csmBody: body }),
};

