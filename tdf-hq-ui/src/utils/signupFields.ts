import { isValidAuthPassword } from './passwordPolicy';

// Mirrors the server's practical requirement (local@domain.tld); the server
// remains the authority and normalizes/validates again.
const EMAIL_PATTERN = /^[^\s@]+@[^\s@]+\.[^\s@]{2,}$/;
const MAX_DISPLAY_NAME_CHARS = 80;

export interface SignupFieldErrors {
  email?: string;
  password?: string;
}

type Translate = (key: string) => string;

/**
 * Signup asks only for email and password. Pass `password: null` to validate
 * the email alone (e.g. on blur).
 */
export function validateSignupFields(email: string, password: string | null, t: Translate): SignupFieldErrors {
  const errors: SignupFieldErrors = {};
  const trimmedEmail = email.trim();
  if (!trimmedEmail || !EMAIL_PATTERN.test(trimmedEmail)) errors.email = t('authEntry.emailInvalid');
  if (password !== null) {
    if (Array.from(password.trim()).length < 8) errors.password = t('authEntry.passwordTooShort');
    else if (!isValidAuthPassword(password)) errors.password = t('authEntry.passwordInvalid');
  }
  return errors;
}

/**
 * Initial display name for an email/password account: the email local part,
 * the same fallback Google accounts receive when they have no profile name.
 * Users can replace it from their profile later (progressive profiling).
 */
export function deriveSignupDisplayName(email: string): string {
  const local = email.trim().split('@')[0] ?? '';
  const cleaned = local
    .split('+')[0]!
    .replace(/[._-]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim()
    .slice(0, MAX_DISPLAY_NAME_CHARS);
  if (!/[\p{L}\p{N}]/u.test(cleaned)) return 'Fan TDF';
  return cleaned.charAt(0).toLocaleUpperCase('es') + cleaned.slice(1);
}
