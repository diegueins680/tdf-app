import '@testing-library/jest-dom';
import { act, cleanup, render, screen } from '@testing-library/react';
import { MemoryRouter, useNavigate, type NavigateFunction } from 'react-router-dom';

import SecurityPage from './SecurityPage';

afterEach(cleanup);

describe('SecurityPage deep links', () => {
  it('provides menu destinations for public reputation rights', () => {
    render(<MemoryRouter><SecurityPage /></MemoryRouter>);
    expect(screen.getByRole('menuitem', { name: 'Privacidad de reputación' })).toHaveAttribute('href', '/privacidad');
    expect(screen.getByRole('menuitem', { name: 'Apelaciones de reputación' })).toHaveAttribute('href', '/apelaciones');
    expect(screen.getByRole('menuitem', { name: 'Cómo se calcula la reputación' })).toHaveAttribute('href', '/reputacion/como-se-calcula');
  });

  it('expands the policy named by an in-app hash change, not only on first load', () => {
    let navigate: NavigateFunction | undefined;
    function CaptureNavigate() {
      navigate = useNavigate();
      return null;
    }
    render(
      <MemoryRouter initialEntries={['/seguridad']}>
        <CaptureNavigate />
        <SecurityPage />
      </MemoryRouter>,
    );
    const privacy = () => screen.getByRole('button', { name: /Política de Privacidad/ });
    const terms = () => screen.getByRole('button', { name: /Términos del Servicio/ });
    expect(privacy()).toHaveAttribute('aria-expanded', 'false');
    expect(terms()).toHaveAttribute('aria-expanded', 'false');

    act(() => navigate!('/seguridad#privacidad'));
    expect(privacy()).toHaveAttribute('aria-expanded', 'true');
    expect(terms()).toHaveAttribute('aria-expanded', 'false');

    act(() => navigate!('/seguridad#terminos'));
    expect(terms()).toHaveAttribute('aria-expanded', 'true');
  });
});
