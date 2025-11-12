const API_BASE = import.meta.env.VITE_API_BASE ?? '';

export interface LoginResponseDTO {
  token: string;
  partyId: number;
  roles: string[];
  modules: string[];
}

export async function loginRequest(payload: { username: string; password: string }): Promise<LoginResponseDTO> {
  const res = await fetch(`${API_BASE}/login`, {
    method: 'POST',
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

export async function requestPasswordReset(email: string): Promise<void> {
  const res = await fetch(`${API_BASE}/v1/password-reset`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ email }),
  });
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    const trimmed = text.trim();
    throw new Error(trimmed === '' ? 'No pudimos iniciar el reset. Verifica el correo.' : trimmed);
  }
}

export interface SignupPayload {
  firstName: string;
  lastName: string;
  email: string;
  phone?: string;
  password: string;
}

export async function signupRequest(payload: SignupPayload): Promise<LoginResponseDTO> {
  const res = await fetch(`${API_BASE}/signup`, {
    method: 'POST',
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
