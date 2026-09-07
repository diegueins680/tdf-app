export const AUTH_PASSWORD_REQUIREMENTS_ES =
  'Usa al menos 8 caracteres y como máximo 72 bytes UTF-8, sin caracteres de control ni formato oculto.';

const containsUnsafePasswordCharacter = (value: string): boolean =>
  /[\p{Cc}\p{Cf}\p{Zl}\p{Zp}]/u.test(value);

export function isValidAuthPassword(value: string): boolean {
  const normalized = value.trim();
  const utf8Bytes = new TextEncoder().encode(normalized).length;
  return Array.from(normalized).length >= 8
    && utf8Bytes <= 72
    && !containsUnsafePasswordCharacter(normalized);
}
