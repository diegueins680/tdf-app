import type { components } from './generated/types';
import type { SignupRole } from '../constants/roles';

const API_BASE = import.meta.env.VITE_API_BASE ?? '';

type LoginRequestDTO = components['schemas']['LoginRequest'];
type GoogleLoginRequestDTO = components['schemas']['GoogleLoginRequest'];
export type LoginResponseDTO = components['schemas']['LoginResponse'];

export async function loginRequest(payload: LoginRequestDTO): Promise<LoginResponseDTO> {
  const res = await fetch(`${API_BASE}/login`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    const trimmed = text.trim();
    throw new Error(trimmed === '' ? 'Credenciales inválidas' : trimmed);
  }
  return res.json() as Promise<LoginResponseDTO>;
}

export async function googleLoginRequest(payload: GoogleLoginRequestDTO): Promise<LoginResponseDTO> {
  const res = await fetch(`${API_BASE}/login/google`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    const trimmed = text.trim();
    throw new Error(trimmed === '' ? 'No pudimos iniciar sesión con Google.' : trimmed);
  }
  return res.json() as Promise<LoginResponseDTO>;
}

export async function requestPasswordReset(email: string): Promise<void> {
  const res = await fetch(`${API_BASE}/v1/password-reset`, {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ email }),
  });
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    const trimmed = text.trim();
    throw new Error(trimmed === '' ? 'No pudimos iniciar el reset. Verifica el correo.' : trimmed);
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
    const text = await res.text().catch(() => '');
    const trimmed = text.trim();
    throw new Error(trimmed === '' ? 'No pudimos restablecer la contraseña.' : trimmed);
  }
  return res.json() as Promise<LoginResponseDTO>;
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
    const text = await res.text().catch(() => '');
    const trimmed = text.trim();
    throw new Error(trimmed === '' ? 'No pudimos crear la cuenta. Revisa los campos.' : trimmed);
  }
  return res.json() as Promise<LoginResponseDTO>;
}
