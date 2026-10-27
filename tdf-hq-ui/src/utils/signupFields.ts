import { isValidAuthPassword } from './passwordPolicy';

// Mirrors the server's practical requirement (local@domain.tld); the server
// remains the authority and normalizes/validates again.
const EMAIL_PATTERN = /^[^\s@]+@[^\s@]+\.[^\s@]{2,}$/;

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
 * Keep the legacy argument for callers, but never use private contact data
 * as an automatic public identity. People choose their public name later.
 */
export function deriveSignupDisplayName(_email: string): string {
  return 'Cuenta TDF';
}
