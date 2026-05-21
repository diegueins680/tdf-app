import { shouldHideRadioForRoute } from './radioRouteVisibility';

describe('shouldHideRadioForRoute', () => {
  it('keeps the radio widget off the dense course registrations admin page', () => {
    expect(shouldHideRadioForRoute('/configuracion/inscripciones-curso')).toBe(true);
    expect(shouldHideRadioForRoute('/configuracion/inscripciones-curso?status=paid')).toBe(true);
  });

  it('keeps the radio available on lighter admin routes and explicit radio routes', () => {
    expect(shouldHideRadioForRoute('/configuracion/estado')).toBe(false);
    expect(shouldHideRadioForRoute('/inicio')).toBe(true);
    expect(shouldHideRadioForRoute('/inicio', '#radio')).toBe(false);
  });
});
