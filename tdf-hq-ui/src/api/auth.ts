import { getLogoutRevision, ownsLogoutRevision, waitForLogoutRequest } from '../session/logoutIntent';
import { AuthRequestError } from '../utils/authErrorMessage';
import { fetchWithDeadline, RequestTimeoutError } from './requestDeadline';
import type { components } from './generated/types';
import { extractErrorDetails } from './errorMessage';
import { resolveApiBase } from '../config/apiBase';
import type { OnboardingIntent } from './session';
import { sanitizeRedirectPath } from '../utils/loginRouting';

const API_BASE = resolveApiBase();
const SERVICE_STARTING_MESSAGE = 'El servicio está arrancando. Intenta de nuevo en unos segundos.';
const AUTH_NETWORK_ERROR_MESSAGE = 'No se pudo conectar con el servicio. Revisa tu conexión e inténtalo de nuevo.';
const AUTH_TIMEOUT_ERROR_MESSAGE = 'La solicitud tardó demasiado. Revisa tu conexión e inténtalo de nuevo.';
const RETRYABLE_UNAVAILABLE_STATUS = 503;
const LOGIN_RETRY_DELAYS_MS = [1000, 2000, 5000];
const MAX_RETRY_DELAY_MS = 15_000;
const STARTING_ERROR_TOKENS = new Set(['starting', 'service unavailable']);

type LoginRequestDTO = components['schemas']['LoginRequest'];
type GoogleLoginRequestDTO = components['schemas']['GoogleLoginRequest'];
export type LoginResponseDTO = components['schemas']['LoginResponse'];

const wait = (ms: number) => new Promise<void>((resolve) => {
  setTimeout(resolve, ms);
});

const readRetryDelayMs = (res: Response, fallbackMs: number): number => {
  const retryAfter = res.headers.get('retry-after')?.trim();
  if (!retryAfter) return fallbackMs;

  const seconds = Number(retryAfter);
  if (Number.isFinite(seconds) && seconds >= 0) {
    return Math.min(Math.round(seconds * 1000), MAX_RETRY_DELAY_MS);
  }

  const retryAt = Date.parse(retryAfter);
  if (!Number.isNaN(retryAt)) {
    return Math.min(Math.max(retryAt - Date.now(), 0), MAX_RETRY_DELAY_MS);
  }

  return fallbackMs;
};

const isStartupFallbackCandidate = (message: string): boolean => {
  const normalized = message.trim().toLowerCase();
  return normalized === '' || STARTING_ERROR_TOKENS.has(normalized);
};

const readErrorMessage = async (res: Response, fallback: string): Promise<string> => {
  const contentType = res.headers.get('content-type') ?? '';
  const text = await res.text();
  const extracted = extractErrorDetails(text, contentType);
  if (res.status === RETRYABLE_UNAVAILABLE_STATUS && isStartupFallbackCandidate(extracted)) {
    return SERVICE_STARTING_MESSAGE;
  }
  return extracted.trim() === '' ? fallback : extracted;
};

const parseJson = <T>(res: Response) => res.json() as Promise<T>;

const readConfiguredRetryDelayMs = (retryDelaysMs: readonly number[], attempt: number): number =>
  retryDelaysMs[attempt] ?? retryDelaysMs[retryDelaysMs.length - 1] ?? 0;

const authFetch = async <T>(
  input: RequestInfo | URL,
  init: RequestInit,
  readResponse: (response: Response) => Promise<T>,
): Promise<T> => {
  try {
    return await fetchWithDeadline(input, init, readResponse);
  } catch (err) {
    if (init.signal?.aborted) throw err;
    const timeout = err instanceof RequestTimeoutError;
    const wrapped = new AuthRequestError(timeout ? 'timeout' : 'network', timeout ? AUTH_TIMEOUT_ERROR_MESSAGE : AUTH_NETWORK_ERROR_MESSAGE);
    (wrapped as Error & { cause?: unknown }).cause = err;
    throw wrapped;
  }
};

type AuthResult<T> = { ok: true; data: T } | { ok: false; response: Response; message: string };

const readAuthResult = async <T>(res: Response, fallback: string): Promise<AuthResult<T>> =>
  res.ok
    ? { ok: true, data: await parseJson<T>(res) }
    : { ok: false, response: res, message: await readErrorMessage(res, fallback) };

async function postAuthJson<T>(
  path: string,
  body: unknown,
  fallback: string,
  options?: { retryDelaysMs?: readonly number[]; credentialsError?: boolean },
): Promise<T> {
  const authRevision = getLogoutRevision();
  const pendingLogout = waitForLogoutRequest();
  if (pendingLogout) await pendingLogout;
  const retryDelaysMs = options?.retryDelaysMs ?? [];
  const maxAttempts = retryDelaysMs.length + 1;

  let attempt = 0;
  while (attempt < maxAttempts) {
    if (!ownsLogoutRevision(authRevision)) throw new Error('El acceso cambió. Inicia sesión de nuevo.');
    let result: AuthResult<T>;
    let responseReceived = false;
    try {
      result = await authFetch(`${API_BASE}${path}`, {
        method: 'POST',
        credentials: 'include',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
      }, (res) => {
        responseReceived = true;
        return readAuthResult<T>(res, fallback);
      });
    } catch (error) {
      // A body failure can follow an effective login/account creation. Do not replay it.
      if (!responseReceived && attempt < retryDelaysMs.length) {
        const fallbackDelayMs = readConfiguredRetryDelayMs(retryDelaysMs, attempt);
        attempt += 1;
        await wait(fallbackDelayMs);
        continue;
      }
      throw error;
    }

    if (result.ok) return result.data;
    const res = result.response;

    if (res.status === RETRYABLE_UNAVAILABLE_STATUS && attempt < retryDelaysMs.length) {
      const fallbackDelayMs = readConfiguredRetryDelayMs(retryDelaysMs, attempt);
      attempt += 1;
      await wait(readRetryDelayMs(res, fallbackDelayMs));
      continue;
    }

    const message = result.message;
    const code = res.status === 401 && options?.credentialsError ? 'credentials' : message === SERVICE_STARTING_MESSAGE ? 'starting' : 'response';
    throw new AuthRequestError(code, message);
  }

  throw new Error(fallback);
}

export async function loginRequest(payload: LoginRequestDTO): Promise<LoginResponseDTO> {
  return postAuthJson<LoginResponseDTO>('/login', payload, 'Credenciales inválidas', {
    retryDelaysMs: LOGIN_RETRY_DELAYS_MS,
    credentialsError: true,
  });
}

export async function googleLoginRequest(payload: GoogleLoginRequestDTO): Promise<LoginResponseDTO> {
  return postAuthJson<LoginResponseDTO>('/login/google', payload, 'No pudimos iniciar sesión con Google.', {
    retryDelaysMs: LOGIN_RETRY_DELAYS_MS,
    credentialsError: true,
  });
}

export async function requestPasswordReset(email: string, redirect?: string | null, locale?: string): Promise<void> {
  const destination = sanitizeRedirectPath(redirect);
  const params = new URLSearchParams();
  if (destination) params.set('redirect', destination);
  if (locale) params.set('locale', locale.toLowerCase().startsWith('es') ? 'es' : 'en');
  const query = params.size ? `?${params.toString()}` : '';
  const message = await authFetch(`${API_BASE}/v1/password-reset${query}`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ email }),
  }, async (res) => res.ok ? null : readErrorMessage(res, 'No pudimos iniciar el reset. Verifica el correo.'));
  if (message !== null) throw new Error(message);
}

export interface PasswordResetConfirmPayload {
  token: string;
  newPassword: string;
}

export async function confirmPasswordReset(
  payload: PasswordResetConfirmPayload,
): Promise<LoginResponseDTO> {
  return postAuthJson<LoginResponseDTO>(
    '/v1/password-reset/confirm', payload, 'No pudimos restablecer la contraseña.',
  );
}

export interface SignupPayload {
  firstName: string;
  lastName: string;
  email: string;
  phone?: string;
  password: string;
  marketingOptIn?: boolean;
  termsAccepted: true;
  termsVersion: string;
  fanArtistIds?: number[];
  claimArtistId?: number;
  onboardingIntent?: OnboardingIntent;
}

export async function signupRequest(payload: SignupPayload): Promise<LoginResponseDTO> {
  return postAuthJson<LoginResponseDTO>(
    '/signup', payload, 'No pudimos crear la cuenta. Revisa los campos.',
  );
}
