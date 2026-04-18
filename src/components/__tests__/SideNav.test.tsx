import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { MemoryRouter } from 'react-router-dom';
import { vi } from 'vitest';
import SideNav from '../SideNav';

vi.mock('../../auth/AuthProvider', () => ({
  useAuth: () => ({
    user: {
      roles: ['admin'],
    },
  }),
}));

describe('SideNav submenu toggles', () => {
  it('keeps module submenus hidden until expanded', async () => {
    const user = userEvent.setup();
    render(
      <MemoryRouter>
        <SideNav collapsed={false} onToggle={() => {}} />
      </MemoryRouter>
    );

    expect(screen.queryByText('Calendario')).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /Mostrar Estudio/i }));
    expect(screen.getByText('Calendario')).toBeVisible();

    await user.click(screen.getByRole('button', { name: /Ocultar Estudio/i }));
    expect(screen.queryByText('Calendario')).not.toBeInTheDocument();
  });

  it('keeps only one module expanded so admin navigation stays focused', async () => {
    const user = userEvent.setup();
    render(
      <MemoryRouter>
        <SideNav collapsed={false} onToggle={() => {}} />
      </MemoryRouter>
    );

    await user.click(screen.getByRole('button', { name: /Mostrar Estudio/i }));
    expect(screen.getByText('Calendario')).toBeVisible();

    await user.click(screen.getByRole('button', { name: /Mostrar CRM/i }));

    expect(screen.getByText('Contactos')).toBeVisible();
    expect(screen.queryByText('Calendario')).not.toBeInTheDocument();
    expect(screen.getByRole('button', { name: /Mostrar Estudio/i })).toHaveAttribute('aria-expanded', 'false');
  });

  it('resets expanded submenus when the navigation collapses', async () => {
    const user = userEvent.setup();
    const { rerender } = render(
      <MemoryRouter>
        <SideNav collapsed={false} onToggle={() => {}} />
      </MemoryRouter>
    );

    await user.click(screen.getByRole('button', { name: /Mostrar CRM/i }));
    expect(screen.getByText('Contactos')).toBeVisible();

    rerender(
      <MemoryRouter>
        <SideNav collapsed onToggle={() => {}} />
      </MemoryRouter>
    );

    expect(screen.queryByText('Contactos')).not.toBeInTheDocument();

    rerender(
      <MemoryRouter>
        <SideNav collapsed={false} onToggle={() => {}} />
      </MemoryRouter>
    );

    await waitFor(() => {
      expect(screen.getByRole('button', { name: /Mostrar CRM/i })).toHaveAttribute('aria-expanded', 'false');
    });
    expect(screen.queryByText('Contactos')).not.toBeInTheDocument();
  });
});
