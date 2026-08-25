import type { AuthUser } from './AuthProvider';

export function parseStoredAuthUser(raw: string): AuthUser | null {
  try {
    return JSON.parse(raw) as AuthUser;
  } catch {
    return null;
  }
}
