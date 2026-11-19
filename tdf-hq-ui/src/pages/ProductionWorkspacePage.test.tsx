import { jest } from '@jest/globals';
import { act, type ReactNode } from 'react';
import { createRoot } from 'react-dom/client';
import { fireEvent, within, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { Action, Project } from '../api/productions';

const project: Project = { id: 'ep', title: 'EP de prueba', role: 'producer', format: 'EP',
  delivery: '2027-05-31', revision: 0, songs: [], tasks: [], people: [], proposals: [] };
const list = jest.fn<() => Promise<Project[]>>();
const command = jest.fn<(p: Project, body: Action) => Promise<unknown>>();
jest.unstable_mockModule('../api/productions', () => ({ listProductions: list,
  productionAction: command, createProduction: jest.fn(), deliveryRisks: () => [],
  workingDate: () => '2027-05-01', nextPerson: () => null }));
jest.unstable_mockModule('../api/label', () => ({ Label: { listTracks: jest.fn() } }));
jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: { partyId: 1, roles: ['engineer'] } }),
}));
jest.unstable_mockModule('../components/PageShell', () => ({
  default: ({ children }: { children: ReactNode }) => <main>{children}</main>,
}));
jest.unstable_mockModule('../features/production/WorkspaceViews', () => ({
  Calendar: () => null, TaskCard: () => null, Timeline: () => null,
}));
const { default: ProductionWorkspacePage } = await import('./ProductionWorkspacePage');
const deferred = <T,>() => {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>(done => { resolve = done; });
  return { promise, resolve };
};

it('keeps the next invitation disabled until the confirmed write and its fresh projection finish', async () => {
  (globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  const write = deferred<unknown>(), refresh = deferred<Project[]>();
  command.mockReset().mockImplementationOnce(() => write.promise).mockResolvedValue({ id: 'second' });
  list.mockReset().mockImplementationOnce(() => refresh.promise).mockResolvedValue([{ ...project, revision: 2 }]);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, staleTime: Infinity, gcTime: 0 } } });
  client.setQueryData(['productions', 1], [project]);
  const container = document.createElement('div'); document.body.append(container);
  const root = createRoot(container), ui = within(container);
  try {
    await act(async () => { root.render(<QueryClientProvider client={client}><ProductionWorkspacePage /></QueryClientProvider>); });
    fireEvent.click(ui.getByText('Invitar al equipo', { exact: true }));
    const username = ui.getByRole('textbox', { name: /Usuario de la cuenta TDF/ });
    const invite = ui.getByRole('button', { name: 'Dar acceso al proyecto' });
    await act(async () => {
      fireEvent.change(username, { target: { value: 'ingeniera' } });
      invite.click();
    });
    expect(command).toHaveBeenCalledTimes(1);
    expect(invite.matches(':disabled')).toBe(true);
    expect(username.matches(':disabled')).toBe(true);
    expect(ui.getByRole('status').textContent).toContain('Guardando');
    await act(async () => { invite.click(); });
    expect(command).toHaveBeenCalledTimes(1);
    await act(async () => { write.resolve({ id: 'first' }); });
    expect(list).toHaveBeenCalledTimes(1);
    expect(invite.matches(':disabled')).toBe(true);
    await act(async () => { refresh.resolve([{ ...project, revision: 1 }]); });
    await waitFor(() => expect(invite.matches(':disabled')).toBe(false));
    await act(async () => {
      fireEvent.change(username, { target: { value: 'artista' } });
      invite.click();
    });
    expect(command).toHaveBeenCalledTimes(2);
    expect(command.mock.calls[1]?.[0].revision).toBe(1);
    expect(command.mock.calls[1]?.[1]).toMatchObject({ op: 'invite', username: 'artista' });
  } finally {
    await act(async () => { root.unmount(); }); client.clear(); container.remove();
  }
});
