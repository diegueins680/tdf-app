import { hasLogoutIntent } from '../session/logoutIntent';
import { fetchWithDeadline, RequestTimeoutError } from './requestDeadline';
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

// Accepts an options object so it also works directly as a react-query
// queryFn (whose context carries an AbortSignal).
export async function loadSessionSnapshot(options: { signal?: AbortSignal } = {}): Promise<SessionResponseDTO | null> {
  if (hasLogoutIntent()) return null;
  const response = await fetch(sessionUrl('/session'), {
    credentials: 'include',
    ...(options.signal ? { signal: options.signal } : {}),
  });

  if (response.status === 401 || response.status === 403) {
    return null;
  }

  if (!response.ok) {
    throw new Error(await readErrorText(response));
  }

  return response.json() as Promise<SessionResponseDTO>;
}

export async function logoutSessionRequest(apiToken?: string): Promise<void> {
  const controller = new AbortController();
  const deadline = setTimeout(() => controller.abort(new RequestTimeoutError()), 30_000);
  try {
    if (!apiToken) {
      // An empty logout POST also returns 204. Verify an available cookie target
      // before treating that response as revocation. Never adopt this snapshot.
      const hasCookieTarget = await fetchWithDeadline(sessionUrl('/session'), {
        credentials: 'include', cache: 'no-store', signal: controller.signal,
      }, async (response) => {
        if (!response.ok) throw new Error('Server sign-out is unconfirmed');
        const snapshot = await response.json() as SessionResponseDTO | null;
        return snapshot !== null && Number.isSafeInteger(snapshot.partyId) && snapshot.partyId > 0;
      });
      if (!hasCookieTarget) throw new Error('Server sign-out is unconfirmed');
    }
    await fetchWithDeadline(sessionUrl('/session/logout'), {
      method: 'POST',
      credentials: 'include',
      signal: controller.signal,
      ...(apiToken ? { headers: { Authorization: `Bearer ${apiToken}` } } : {}),
    }, (response) => response.ok
      ? Promise.resolve()
      : Promise.reject(new Error('Server sign-out is unconfirmed')));
  } catch (error) {
    if (controller.signal.aborted) throw controller.signal.reason;
    throw error;
  } finally {
    clearTimeout(deadline);
  }
}

export async function completeOnboardingProgress(
  firstValue?: OnboardingFirstValue,
  apiToken?: string,
): Promise<OnboardingCompletionResultDTO> {
  const response = await fetch(sessionUrl('/session/onboarding/complete'), {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/json', ...(apiToken ? { Authorization: `Bearer ${apiToken}` } : {}) },
    body: JSON.stringify(firstValue ? { firstValue } : {}),
  });

  if (!response.ok) {
    throw new Error(await readErrorText(response));
  }

  return response.json() as Promise<OnboardingCompletionResultDTO>;
}

export async function reconcileOnboardingProgress(
  apiToken?: string,
): Promise<OnboardingCompletionResultDTO> {
  const response = await fetch(sessionUrl('/session/onboarding/reconcile'), {
    method: 'POST',
    credentials: 'include',
    ...(apiToken ? { headers: { Authorization: `Bearer ${apiToken}` } } : {}),
  });

  if (!response.ok) {
    throw new Error(await readErrorText(response));
  }

  return response.json() as Promise<OnboardingCompletionResultDTO>;
}

export async function loadOnboardingProgress(apiToken?: string): Promise<OnboardingProgressDTO> {
  const response = await fetch(sessionUrl('/session/onboarding'), {
    credentials: 'include',
    ...(apiToken ? { headers: { Authorization: `Bearer ${apiToken}` } } : {}),
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

export async function redeemArtistInvitation(
  artistInvitation: string,
  apiToken?: string | null,
): Promise<SessionResponseDTO> {
  const response = await fetch(sessionUrl('/session/artist-invitation'), {
    method: 'POST',
    credentials: 'include',
    headers: {
      'Content-Type': 'application/json',
      ...(apiToken ? { Authorization: `Bearer ${apiToken}` } : {}),
    },
    body: JSON.stringify({ artistInvitation }),
  });

  if (!response.ok) {
    throw new Error(await readErrorText(response));
  }

  return response.json() as Promise<SessionResponseDTO>;
}
