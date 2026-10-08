import { shouldHideRadioForRoute, shouldRenderRadioWidget } from './radioRouteVisibility';

describe('shouldHideRadioForRoute', () => {
  it.each(['/musica', '/musica/single-propio', '/musica/biblioteca'])(
    'keeps the legacy radio unmounted on canonical audio route %s, even with #radio', (path) => {
      expect(shouldRenderRadioWidget(path, '', true, false)).toBe(false);
      expect(shouldRenderRadioWidget(path, '#radio', true, false)).toBe(false);
    },
  );
  it('does not hide unrelated route prefixes', () => {
    expect(shouldRenderRadioWidget('/musical-tools', '', true, false)).toBe(true);
  });
  it('keeps the radio widget off the dense course registrations admin page', () => {
    expect(shouldHideRadioForRoute('/configuracion/inscripciones-curso')).toBe(true);
    expect(shouldHideRadioForRoute('/configuracion/inscripciones-curso?status=paid')).toBe(true);
  });

  it('keeps the radio available on lighter admin routes and explicit radio routes', () => {
    expect(shouldHideRadioForRoute('/configuracion/estado')).toBe(false);
    expect(shouldHideRadioForRoute('/inicio')).toBe(true);
    expect(shouldHideRadioForRoute('/inicio', '#radio')).toBe(false);
  });

  it('keeps acquisition pages clear unless the visitor explicitly opens the radio', () => {
    expect(shouldHideRadioForRoute('/tdf')).toBe(true);
    expect(shouldHideRadioForRoute('/tdf/artistas')).toBe(true);
    expect(shouldHideRadioForRoute('/tdf', '#radio')).toBe(false);
  });

  it('does not mount the protected radio client before an authenticated session is ready', () => {
    expect(shouldRenderRadioWidget('/domo-del-pululahua', '', false, false)).toBe(false);
    expect(shouldRenderRadioWidget('/domo-del-pululahua', '', true, true)).toBe(false);
    expect(shouldRenderRadioWidget('/domo-del-pululahua', '', true, false)).toBe(true);
    expect(shouldRenderRadioWidget('/login', '', true, false)).toBe(false);
  });
});
