import type { TFunction } from 'i18next';

type AuthErrorCode = 'network' | 'timeout' | 'credentials' | 'starting' | 'response';

// Preserve the existing API message for non-UI consumers; views translate stable codes.
export class AuthRequestError extends Error {
  constructor(readonly code: AuthErrorCode, message: string) {
    super(message);
    this.name = 'AuthRequestError';
  }
}

export function authErrorMessage(error: unknown, t: TFunction, fallback: string): string {
  if (error instanceof AuthRequestError) {
    switch (error.code) {
      case 'network': return t('authEntry.networkError');
      case 'timeout': return t('authEntry.timeoutError');
      case 'credentials': return t('authEntry.invalidCredentials');
      case 'starting': return t('authEntry.servicePreparing');
    }
  }
  return error instanceof Error && error.message.trim() ? error.message : t(fallback);
}
