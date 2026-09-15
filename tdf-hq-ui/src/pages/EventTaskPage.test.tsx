import { jest } from '@jest/globals';
import { act, StrictMode } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import type { SessionUser } from '../session/SessionContext';
import es from '../i18n/locales/es';
import en from '../i18n/locales/en';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
const postMock = jest.fn();
let session: SessionUser | null;
let loading = false;
let english = false;
jest.unstable_mockModule('../api/client', () => ({ get: getMock, post: postMock }));
jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session, loading }), getActiveSession: () => session,
}));
jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ t: (key: string, options: Record<string, string | number> = {}) => {
    const strings = english ? en.eventTask : es.eventTask;
    const text = strings[key.replace('eventTask.', '') as keyof typeof strings] ?? key;
    return text.replace(/\{\{(\w+)\}\}/g, (_, name: string) => String(options[name] ?? ''));
  } }),
}));
const { default: EventTaskPage } = await import('./EventTaskPage');
const fixture = {
  eventId: 80, activityId: 8000, status: 'planned', version: 1,
  policy: { requiresAccountability: true, dependenciesGateCompletion: true, version: 2 },
  raci: [{ partyId: 11, role: 'accountable' }, { partyId: 12, role: 'responsible' }],
  accountabilityNeedsAttention: false,
};
function deferred() {
  let resolve!: (value: unknown) => void;
  let reject!: (reason: Error) => void;
  const promise = new Promise<unknown>((yes, no) => { resolve = yes; reject = no; });
  return { promise, resolve, reject };
}
const tree = (eventId = '80', activityId: string | null = '8000') =>
  <MemoryRouter><EventTaskPage eventId={eventId} activityId={activityId} /></MemoryRouter>;

beforeEach(() => {
  session = { username: 'a', displayName: 'A', partyId: 1, roles: [], apiToken: 'synthetic-a' };
  loading = false; english = false;
  getMock.mockReset().mockResolvedValue(fixture); postMock.mockReset();
});
afterEach(cleanup);

test('reads only the exact task with pinned bearer and abort signal; renders semantic RACI', async () => {
  const view = render(tree());
  expect(await screen.findByRole('table', { name: 'Asignaciones RACI' })).toBeTruthy();
  expect(screen.getByText('Persona #11')).toBeTruthy();
  expect(screen.getByText('A — Rinde cuentas')).toBeTruthy();
  expect(screen.getByText('R — Ejecuta')).toBeTruthy();
  expect(getMock).toHaveBeenCalledTimes(1);
  expect(getMock).toHaveBeenCalledWith('/event-operations/events/80/tasks/8000', {
    cache: 'no-store', headers: { Authorization: 'Bearer synthetic-a' }, signal: expect.any(AbortSignal),
  });
  expect(postMock).not.toHaveBeenCalled();
  await expectNoSeriousAccessibilityViolations(view.container);
});

test.each(['', '0', '-1', '01', '1e2', '1.0', ' 80', '9007199254740992', null])(
  'invalid task capture %s sends no request', async value => {
    render(tree('80', value));
    expect(screen.getByRole('alert').textContent).toContain('El enlace de la tarea no es válido');
    expect(getMock).not.toHaveBeenCalled();
  },
);
test('invalid event capture sends no request', () => {
  render(tree('1e2')); expect(getMock).not.toHaveBeenCalled();
});

test('hydration and logged-out states do not read or retain task data', async () => {
  loading = true;
  const view = render(tree()); expect(getMock).not.toHaveBeenCalled();
  loading = false; session = null; view.rerender(tree());
  expect(screen.getByRole('alert').textContent).toContain('Inicia sesión');
  expect(getMock).not.toHaveBeenCalled();
});

test('empty policy and assignments are honest, not a readiness or completion promise', async () => {
  getMock.mockResolvedValue({ ...fixture, policy: undefined, raci: [] });
  render(tree());
  expect(await screen.findByText('No hay asignaciones RACI registradas.')).toBeTruthy();
  expect(screen.getByText('No hay una política de responsabilidad registrada.')).toBeTruthy();
  expect(screen.getByText(/Esta consulta no confirma/)).toBeTruthy();
});

test('canonical attention is visible and English translation is available', async () => {
  english = true;
  getMock.mockResolvedValue({ ...fixture, raci: [], accountabilityNeedsAttention: true });
  render(tree());
  expect(await screen.findByText('Accountability needs attention.')).toBeTruthy();
  expect(screen.getByRole('button', { name: 'Refresh' })).toBeTruthy();
});

test.each([undefined, { ...fixture, activityId: 9000 }, { ...fixture, secret: 'private' }])(
  'rejects malformed successful data without displaying diagnostics', async invalid => {
    getMock.mockResolvedValue(invalid); render(tree());
    expect(await screen.findByRole('alert')).toBeTruthy();
    expect(screen.queryByRole('table')).toBeNull();
    expect(screen.queryByText(/private/)).toBeNull();
  },
);

test('refresh removes prior data while pending and on denial; retry recovers', async () => {
  const pending = deferred();
  render(tree()); await screen.findByRole('table');
  getMock.mockReturnValueOnce(pending.promise);
  fireEvent.click(screen.getByRole('button', { name: 'Actualizar' }));
  expect(screen.queryByRole('table')).toBeNull();
  await act(async () => pending.reject(new Error('private permission diagnostic')));
  expect(await screen.findByRole('alert')).toBeTruthy();
  expect(screen.queryByText(/private/)).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  expect(await screen.findByRole('table')).toBeTruthy();
  expect(getMock).toHaveBeenCalledTimes(3);
});

test('navigation hides previous success before the next response and ignores a late old target', async () => {
  const old = deferred(); const next = deferred();
  getMock.mockReturnValueOnce(old.promise).mockReturnValueOnce(next.promise);
  const view = render(tree());
  view.rerender(tree('80', '9000'));
  await act(async () => old.resolve(fixture));
  expect(screen.queryByRole('table')).toBeNull();
  await act(async () => next.resolve({ ...fixture, activityId: 9000, raci: [], accountabilityNeedsAttention: true }));
  expect(await screen.findByText('La responsabilidad requiere atención.')).toBeTruthy();
  expect(screen.queryByText('Persona #11')).toBeNull();
});

test('same-party credential rotation clears displayed data and uses the new bearer', async () => {
  const next = deferred(); const view = render(tree()); await screen.findByRole('table');
  getMock.mockReturnValueOnce(next.promise);
  session = { ...session!, apiToken: 'synthetic-rotated' }; view.rerender(tree());
  expect(screen.queryByRole('table')).toBeNull();
  expect(getMock.mock.calls[1]?.[1]?.headers).toEqual({ Authorization: 'Bearer synthetic-rotated' });
  await act(async () => next.resolve(fixture)); await screen.findByRole('table');
});

test('late account failure cannot replace the new account view', async () => {
  const old = deferred(); getMock.mockReturnValueOnce(old.promise);
  const view = render(tree());
  session = { ...session!, partyId: 2, apiToken: 'synthetic-b' }; view.rerender(tree());
  await screen.findByRole('table');
  await act(async () => old.reject(new Error('old private failure')));
  expect(screen.getByRole('table')).toBeTruthy(); expect(screen.queryByRole('alert')).toBeNull();
});

test('logout removes the receipt; an unmounted request is aborted and ignored', async () => {
  const pending = deferred(); const view = render(tree()); await screen.findByRole('table');
  session = null; view.rerender(tree()); expect(screen.queryByRole('table')).toBeNull();
  session = { username: 'b', displayName: 'B', roles: [], partyId: 2 };
  getMock.mockReturnValueOnce(pending.promise); view.rerender(tree());
  const signal = getMock.mock.calls.at(-1)?.[1]?.signal;
  view.unmount(); expect(signal?.aborted).toBe(true);
  await act(async () => pending.resolve(fixture));
  expect(screen.queryByRole('table')).toBeNull();
});

test('session invalidation before React rerenders discards the arriving response', async () => {
  const pending = deferred(); getMock.mockReturnValueOnce(pending.promise);
  render(tree()); session = null;
  await act(async () => pending.resolve(fixture));
  await waitFor(() => expect(screen.queryByRole('table')).toBeNull());
});

test('StrictMode cleanup aborts the first read and cannot overwrite the current receipt', async () => {
  const first = deferred(); getMock.mockReturnValueOnce(first.promise);
  render(<StrictMode>{tree()}</StrictMode>);
  await screen.findByRole('table');
  expect(getMock).toHaveBeenCalledTimes(2);
  expect(getMock.mock.calls[0]?.[1]?.signal?.aborted).toBe(true);
  await act(async () => first.resolve({ ...fixture, raci: [], accountabilityNeedsAttention: true }));
  expect(screen.getByRole('table')).toBeTruthy();
  expect(screen.queryByText('La responsabilidad requiere atención.')).toBeNull();
});
