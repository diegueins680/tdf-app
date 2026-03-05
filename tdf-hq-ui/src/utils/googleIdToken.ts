export interface GoogleIdTokenClaims {
  email?: string;
  name?: string;
}

const toBase64 = (value: string): string => value.replace(/-/g, '+').replace(/_/g, '/');

const binaryToUtf8 = (value: string): string => {
  const bytes = Uint8Array.from(value, (char) => char.charCodeAt(0));
  if (typeof TextDecoder !== 'undefined') {
    return new TextDecoder().decode(bytes);
  }
  const percentEncoded = Array.from(bytes, (byte) => `%${byte.toString(16).padStart(2, '0')}`).join('');
  return decodeURIComponent(percentEncoded);
};

const decodeBase64UrlUtf8 = (value: string): string | null => {
  const normalized = toBase64(value);
  const padded = normalized.padEnd(Math.ceil(normalized.length / 4) * 4, '=');
  try {
    const binary = atob(padded);
    return binaryToUtf8(binary);
  } catch {
    return null;
  }
};

const normalizeClaim = (value: unknown): string | undefined => {
  if (typeof value !== 'string') return undefined;
  const trimmed = value.trim();
  return trimmed === '' ? undefined : trimmed;
};

export const parseGoogleIdToken = (token: string): GoogleIdTokenClaims | null => {
  const [, payload] = token.split('.');
  if (!payload) return null;

  const decodedPayload = decodeBase64UrlUtf8(payload);
  if (!decodedPayload) return null;

  try {
    const parsed = JSON.parse(decodedPayload) as Record<string, unknown>;
    if (!parsed || typeof parsed !== 'object') return null;
    return {
      email: normalizeClaim(parsed['email']),
      name: normalizeClaim(parsed['name']),
    };
  } catch {
    return null;
  }
};
