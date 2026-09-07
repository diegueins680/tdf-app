import type { components } from './generated/types';
import { resolveApiBase } from '../config/apiBase';

const API_BASE = resolveApiBase();

export type SessionResponseDTO = components['schemas']['SessionResponse'];
export type OnboardingIntent = components['schemas']['OnboardingIntent'];
export type OnboardingFirstValue = NonNullable<components['schemas']['OnboardingCompletionRequest']['firstValue']>;
export type OnboardingProgressDTO = components['schemas']['OnboardingProgress'];
export type OnboardingCompletionResultDTO = components['schemas']['OnboardingCompletionResult'];

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

export async function completeOnboardingProgress(
  firstValue?: OnboardingFirstValue,
): Promise<OnboardingCompletionResultDTO> {
  const response = await fetch(sessionUrl('/session/onboarding/complete'), {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(firstValue ? { firstValue } : {}),
  });

  if (!response.ok) {
    throw new Error(await readErrorText(response));
  }

  return response.json() as Promise<OnboardingCompletionResultDTO>;
}

export async function loadOnboardingProgress(): Promise<OnboardingProgressDTO> {
  const response = await fetch(sessionUrl('/session/onboarding'), {
    credentials: 'include',
  });

  if (!response.ok) {
    throw new Error(await readErrorText(response));
  }

  return response.json() as Promise<OnboardingProgressDTO>;
}

export async function persistOnboardingIntent(
  onboardingIntent: OnboardingIntent,
  apiToken?: string,
): Promise<OnboardingProgressDTO> {
  const response = await fetch(sessionUrl('/session/onboarding/intent'), {
    method: 'PUT',
    credentials: 'include',
    headers: {
      'Content-Type': 'application/json',
      ...(apiToken ? { Authorization: `Bearer ${apiToken}` } : {}),
    },
    body: JSON.stringify({ onboardingIntent }),
  });

  if (!response.ok) {
    throw new Error(await readErrorText(response));
  }

  return response.json() as Promise<OnboardingProgressDTO>;
}
