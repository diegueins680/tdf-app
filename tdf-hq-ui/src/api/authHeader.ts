import { getStoredSessionToken } from '../session/SessionContext';

const BEARER_PREFIX = /^bearer\b/i;

export const buildAuthorizationHeader = (): string | undefined => {
  const token = getStoredSessionToken()?.trim();
  if (!token) return undefined;

  if (BEARER_PREFIX.test(token)) {
    const rawCredentials = token.replace(BEARER_PREFIX, '').trim();
    if (!rawCredentials) return undefined;
    return `Bearer ${rawCredentials}`;
  }

  return `Bearer ${token}`;
};
