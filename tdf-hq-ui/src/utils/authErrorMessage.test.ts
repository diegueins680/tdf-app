import i18n from '../i18n';
import { AuthRequestError, authErrorMessage } from './authErrorMessage';

describe('auth failure presentation', () => {
  it.each(['es', 'en'])('translates stable transport/status codes in %s', async locale => {
    await i18n.changeLanguage(locale);
    const expected = locale === 'en'
      ? ['Could not connect', 'took too long', 'Incorrect username', 'service is starting']
      : ['No se pudo conectar', 'tardó demasiado', 'Credenciales inválidas', 'Estamos activando el servicio'];
    for (const [index, code] of (['network', 'timeout', 'credentials', 'starting'] as const).entries()) {
      expect(authErrorMessage(new AuthRequestError(code, 'raw API sentinel'), i18n.t, 'authEntry.loginError')).toContain(expected[index]);
    }
    expect(authErrorMessage(undefined, i18n.t, 'authEntry.loginError')).toBe(i18n.t('authEntry.loginError'));
  });
  it('preserves actionable provider details outside the known transport codes', () => {
    expect(authErrorMessage(new AuthRequestError('response', 'Reviewed provider detail'), i18n.t, 'authEntry.loginError')).toBe('Reviewed provider detail');
  });
});
