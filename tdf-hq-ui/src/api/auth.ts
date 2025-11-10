const API_BASE = import.meta.env.VITE_API_BASE || '';

export type LoginResponseDTO = {
  token: string;
  partyId: number;
  roles: string[];
  modules: string[];
};

export async function loginRequest(payload: { username: string; password: string }): Promise<LoginResponseDTO> {
  const res = await fetch(`${API_BASE}/login`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    throw new Error(text || 'Credenciales inválidas');
  }
  return res.json() as Promise<LoginResponseDTO>;
}
