export const AUTH_SESSION_EXPIRED_EVENT = 'tdf-auth-session-expired';

let authSessionEpoch = 0;
export const getAuthSessionEpoch = (): number => authSessionEpoch;
export const advanceAuthSessionEpoch = (): void => { authSessionEpoch += 1; };

const SESSION_AUTH_FAILURE_MESSAGES = [
  'missing or invalid auth token',
  'invalid or inactive token',
  'invalid or inactive session token',
  'conflicting auth credentials found',
];

export const isSessionAuthFailureMessage = (message: string): boolean => {
  const normalized = message.trim().toLowerCase();
  return SESSION_AUTH_FAILURE_MESSAGES.some((entry) => normalized.includes(entry));
};

export const notifyAuthSessionExpired = (requestEpoch = authSessionEpoch): void => {
  if (requestEpoch !== authSessionEpoch) return;
  if (typeof window === 'undefined') return;
  window.dispatchEvent(new Event(AUTH_SESSION_EXPIRED_EVENT));
};
