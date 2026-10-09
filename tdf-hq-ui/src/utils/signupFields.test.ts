import { deriveSignupDisplayName, validateSignupFields } from './signupFields';

const t = (key: string) => key;

describe('validateSignupFields', () => {
  it('accepts an email and an 8+ character password and nothing else', () => {
    expect(validateSignupFields('llamaestepez@gmail.com', 'secreta12', t)).toEqual({});
  });

  it.each(['', 'juan', 'juan@gmail', 'juan @gmail.com', '@gmail.com'])('rejects email %j', (email) => {
    expect(validateSignupFields(email, 'secreta12', t).email).toBe('authEntry.emailInvalid');
  });

  it('reports short and unsafe passwords separately', () => {
    expect(validateSignupFields('a@b.co', 'corta', t).password).toBe('authEntry.passwordTooShort');
    expect(validateSignupFields('a@b.co', 'secreta​12', t).password).toBe('authEntry.passwordInvalid');
  });

  it('validates the email alone when no password is given (blur)', () => {
    expect(validateSignupFields('a@b.co', null, t)).toEqual({});
  });
});

describe('deriveSignupDisplayName', () => {
  it.each([
    ['llamaestepez@gmail.com', 'Llamaestepez'],
    ['maria.jose_perez@example.com', 'Maria jose perez'],
    ['ana+tdf@example.com', 'Ana'],
    ['...@example.com', 'Fan TDF'],
    [`${'x'.repeat(120)}@example.com`, `X${'x'.repeat(79)}`],
  ])('derives a server-valid display name from %s', (email, expected) => {
    expect(deriveSignupDisplayName(email)).toBe(expected);
  });
});
