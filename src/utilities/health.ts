import { get } from '../api/client';

export const Health = {
  fetch: () => get<{ status: string; db: string }>('/health'),
};
