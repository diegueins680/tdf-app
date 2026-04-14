import type { components } from './generated/types';
import type { SignupRole } from '../constants/roles';
import { extractErrorDetails } from './errorMessage';
import { env } from '../utils/env';

const API_BASE = env.read('VITE_API_BASE') ?? '';
const SERVICE_STARTING_MESSAGE = 'El servicio está arrancando. Intenta de nuevo en unos segundos.';
const RETRYABLE_UNAVAILABLE_STATUS = 503;
const LOGIN_RETRY_DELAYS_MS = [1000, 2000];
const STARTING_ERROR_TOKENS = new Set(['starting', 'service unavailable']);

type LoginRequestDTO = components['schemas']['LoginRequest'];
type GoogleLoginRequestDTO = components['schemas']['GoogleLoginRequest'];
export type LoginResponseDTO = components['schemas']['LoginResponse'];

const wait = (ms: number) => new Promise<void>((resolve) => {
  setTimeout(resolve, ms);
});

const isStartupFallbackCandidate = (message: string): boolean => {
  const normalized = message.trim().toLowerCase();
  return normalized === '' || STARTING_ERROR_TOKENS.has(normalized);
};

const readErrorMessage = async (res: Response, fallback: string): Promise<string> => {
  const contentType = res.headers.get('content-type') ?? '';
  const text = await res.text().catch(() => '');
  const extracted = extractErrorDetails(text, contentType);
  if (res.status === RETRYABLE_UNAVAILABLE_STATUS && isStartupFallbackCandidate(extracted)) {
    return SERVICE_STARTING_MESSAGE;
  }
  return extracted.trim() === '' ? fallback : extracted;
};

const parseJson = <T>(res: Response) => res.json() as Promise<T>;

async function postAuthJson<T>(
  path: string,
  body: unknown,
  fallback: string,
  options?: { retryDelaysMs?: readonly number[] },
): Promise<T> {
  const retryDelaysMs = options?.retryDelaysMs ?? [];

  for (let attempt = 0; ; attempt += 1) {
    const res = await fetch(`${API_BASE}${path}`, {
      method: 'POST',
      credentials: 'include',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    });

    if (res.ok) {
      return parseJson<T>(res);
    }

    if (res.status === RETRYABLE_UNAVAILABLE_STATUS && attempt < retryDelaysMs.length) {
      await wait(retryDelaysMs[attempt] ?? retryDelaysMs[retryDelaysMs.length - 1] ?? 0);
      continue;
    }

    throw new Error(await readErrorMessage(res, fallback));
  }
}

export async function loginRequest(payload: LoginRequestDTO): Promise<LoginResponseDTO> {
  return postAuthJson<LoginResponseDTO>('/login', payload, 'Credenciales inválidas', {
    retryDelaysMs: LOGIN_RETRY_DELAYS_MS,
  });
}

export async function googleLoginRequest(payload: GoogleLoginRequestDTO): Promise<LoginResponseDTO> {
  return postAuthJson<LoginResponseDTO>('/login/google', payload, 'No pudimos iniciar sesión con Google.', {
    retryDelaysMs: LOGIN_RETRY_DELAYS_MS,
  });
}

export async function requestPasswordReset(email: string): Promise<void> {
  const res = await fetch(`${API_BASE}/v1/password-reset`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ email }),
  });
  if (!res.ok) {
    throw new Error(await readErrorMessage(res, 'No pudimos iniciar el reset. Verifica el correo.'));
  }
}

export interface PasswordResetConfirmPayload {
  token: string;
  newPassword: string;
}

export async function confirmPasswordReset(
  payload: PasswordResetConfirmPayload,
): Promise<LoginResponseDTO> {
  const res = await fetch(`${API_BASE}/v1/password-reset/confirm`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });
  if (!res.ok) {
    throw new Error(await readErrorMessage(res, 'No pudimos restablecer la contraseña.'));
  }
  return parseJson<LoginResponseDTO>(res);
}

export interface SignupPayload {
  firstName: string;
  lastName: string;
  email: string;
  phone?: string;
  password: string;
  internshipStartAt?: string;
  internshipEndAt?: string;
  internshipRequiredHours?: number;
  internshipSkills?: string;
  internshipAreas?: string;
  roles?: SignupRole[];
  fanArtistIds?: number[];
  claimArtistId?: number;
}

export async function signupRequest(payload: SignupPayload): Promise<LoginResponseDTO> {
  const res = await fetch(`${API_BASE}/signup`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });
  if (!res.ok) {
    throw new Error(await readErrorMessage(res, 'No pudimos crear la cuenta. Revisa los campos.'));
  }
  return parseJson<LoginResponseDTO>(res);
}
