import type { components } from './generated/types';
import { env } from '../utils/env';

const API_BASE = env.read('VITE_API_BASE') ?? '';

export type SessionResponseDTO = components['schemas']['SessionResponse'];

const readErrorText = async (response: Response): Promise<string> => {
  const text = await response.text().catch(() => '');
  const trimmed = text.trim();
  return trimmed === '' ? response.statusText.trim() : trimmed;
};

const sessionUrl = (path: string) => `${API_BASE}${path}`;

export async function loadSessionSnapshot(): Promise<SessionResponseDTO | null> {
  const response = await fetch(sessionUrl('/session'), {
    credentials: 'include',
  });

  if (response.status === 401 || response.status === 403) {
    return null;
  }

  if (!response.ok) {
    throw new Error(await readErrorText(response));
  }

  return response.json() as Promise<SessionResponseDTO>;
}

export async function logoutSessionRequest(): Promise<void> {
  const response = await fetch(sessionUrl('/session/logout'), {
    method: 'POST',
    credentials: 'include',
  });

  if (response.status === 401 || response.status === 403) {
    return;
  }

  if (!response.ok) {
    throw new Error(await readErrorText(response));
  }
}
