import { jest } from '@jest/globals';
import { render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

const session = {
  username: 'admin',
  displayName: 'Admin',
  roles: ['admin'],
  featureFlags: [] as string[],
  modules: ['admin', 'crm', 'scheduling'],
};
let activeSession: typeof session | null = session;

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: activeSession }),
  getStoredSessionToken: () => null,
}));
jest.unstable_mockModule('../hooks/useChatUnreadCount', () => ({
  useChatUnreadCount: () => ({ unreadCount: 0 }),
}));
jest.unstable_mockModule('../hooks/useNavigationPreferences', () => ({
  useNavigationPreferences: () => ({ query: { data: [] }, update: { mutate: jest.fn(), mutateAsync: jest.fn() }, visit: { mutate: jest.fn() } }),
}));

const { default: MenuFamilyTabs } = await import('./MenuFamilyTabs');

const renderAt = (path: string) => render(<MemoryRouter initialEntries={[path]}><MenuFamilyTabs /></MemoryRouter>);

describe('MenuFamilyTabs', () => {
  afterEach(() => { activeSession = session; session.roles = ['admin']; session.modules = ['admin', 'crm', 'scheduling']; });

  it('links the pages that share the Contactos entry and marks the open one', () => {
    renderAt('/crm/leads');
    const tabs = screen.getAllByRole('tab');
    expect(screen.getByRole('tablist', { name: 'Contactos' })).not.toBeNull();
    expect(tabs.map((tab) => tab.textContent)).toEqual(['Contactos', 'Empresas', 'Leads']);
    expect(tabs.map((tab) => tab.getAttribute('href'))).toEqual(['/crm/contactos', '/crm/empresas', '/crm/leads']);
    expect(tabs[2]?.getAttribute('aria-selected')).toBe('true');
  });

  it('offers the session list beside the calendar to the studio team only', () => {
    const view = renderAt('/estudio/calendario');
    expect(screen.getAllByRole('tab').map((tab) => tab.textContent)).toEqual(['Calendario', 'Lista de sesiones']);
    view.unmount();
    session.roles = ['Artist']; session.modules = ['scheduling'];
    renderAt('/estudio/calendario');
    expect(screen.queryByRole('tablist')).toBeNull();
  });

  it('renders nothing outside a page family or without a session', () => {
    const view = renderAt('/inicio');
    expect(screen.queryByRole('tablist')).toBeNull();
    view.unmount();
    activeSession = null;
    renderAt('/crm/leads');
    expect(screen.queryByRole('tablist')).toBeNull();
  });
});
