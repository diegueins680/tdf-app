import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { act, StrictMode } from 'react';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import type { SessionUser } from '../session/SessionContext';
import es from '../i18n/locales/es';
import en from '../i18n/locales/en';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown, init?: RequestInit) => Promise<unknown>>();
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

const editorContext = {
  eventId: 80, activityId: 8000, aggregateRevision: '4', canManage: true, operationReady: true,
  replaceableAssignments: fixture.raci, eligiblePartyIds: [11, 12, 13],
};
function enableEditor(value: unknown = editorContext) {
  getMock.mockImplementation(async path => path.includes('/raci/context') ? value : fixture);
}
async function openEditor() {
  fireEvent.click(await screen.findByRole('button', { name: 'Preparar reasignación' }));
  return screen.findByRole('combobox', { name: 'Asignación que se reemplaza' });
}
async function reviewChange() {
  fireEvent.change(await openEditor(), { target: { value: '12:responsible' } });
  fireEvent.change(screen.getByRole('combobox', { name: 'Nueva persona responsable del rol' }), { target: { value: '13' } });
  fireEvent.change(screen.getByRole('textbox', { name: 'Motivo de la reasignación' }), { target: { value: 'Cambio de turno de prueba' } });
  fireEvent.click(screen.getByRole('button', { name: 'Revisar cambio' }));
  return screen.findByRole('dialog', { name: 'Confirmar reasignación RACI' });
}
function accepted(replayed = false) {
  const key = postMock.mock.calls.at(-1)![2]!.headers as Record<string, string>;
  return { eventId: 80, activityId: 8000, commandId: key['Idempotency-Key'], role: 'responsible',
    fromPartyId: 12, toPartyId: 13, aggregateRevision: '6', replayed };
}

test('editor reads only on explicit opening, filters the source, and requires reason and separate confirmation', async () => {
  enableEditor(); const view = render(tree());
  await screen.findByRole('table'); expect(getMock).toHaveBeenCalledTimes(1);
  const source = await openEditor();
  expect(getMock).toHaveBeenCalledTimes(2);
  expect(getMock.mock.calls[1]?.[0]).toBe('/event-operations/events/80/tasks/8000/raci/context?afterPartyId=0');
  fireEvent.change(source, { target: { value: '12:responsible' } });
  const recipient = screen.getByRole('combobox', { name: 'Nueva persona responsable del rol' });
  expect(within(recipient).queryByRole('option', { name: 'Persona #12' })).toBeNull();
  fireEvent.change(recipient, { target: { value: '13' } });
  expect(screen.getByRole('button', { name: 'Revisar cambio' })).toBeDisabled();
  fireEvent.change(screen.getByRole('textbox'), { target: { value: '   ' } });
  expect(screen.getByRole('button', { name: 'Revisar cambio' })).toBeDisabled();
  fireEvent.change(screen.getByRole('textbox'), { target: { value: 'Cambio de prueba' } });
  await expectNoSeriousAccessibilityViolations(view.container);
  fireEvent.click(screen.getByRole('button', { name: 'Revisar cambio' }));
  const dialog = await screen.findByRole('dialog');
  expect(within(dialog).getByText('Cambio de prueba')).toBeTruthy();
  expect(within(dialog).getByText('Revisión conjunta: 4')).toBeTruthy();
  expect(postMock).not.toHaveBeenCalled();
  await expectNoSeriousAccessibilityViolations(dialog);
  fireEvent.click(within(dialog).getByRole('button', { name: 'Volver sin enviar' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  expect(postMock).not.toHaveBeenCalled();
});

test.each([false, true])('non-ready context with manager=%s has no editing controls', async canManage => {
  enableEditor({ ...editorContext, canManage, operationReady: false, replaceableAssignments: [], eligiblePartyIds: [] });
  render(tree()); fireEvent.click(await screen.findByRole('button', { name: 'Preparar reasignación' }));
  await screen.findByText('No hay una reasignación disponible con tus permisos y el estado actual.');
  expect(screen.queryByRole('combobox')).toBeNull(); expect(postMock).not.toHaveBeenCalled();
});

test('context failure can retry but never invents options or exposes diagnostics', async () => {
  enableEditor(); render(tree()); await screen.findByRole('table');
  getMock.mockRejectedValueOnce(new Error('private diagnostic'));
  fireEvent.click(screen.getByRole('button', { name: 'Preparar reasignación' }));
  await screen.findByText('No pudimos cargar las opciones. Vuelve a consultarlas.');
  expect(screen.queryByText('private diagnostic')).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Consultar opciones de nuevo' }));
  await screen.findByRole('combobox', { name: 'Asignación que se reemplaza' });
  expect(postMock).not.toHaveBeenCalled();
});

test('new page replaces prior options and resets selection without mixing revisions', async () => {
  enableEditor({ ...editorContext, eligiblePartyIds: Array.from({ length: 100 }, (_, n) => n + 1), nextAfterPartyId: 100 });
  render(tree()); fireEvent.change(await openEditor(), { target: { value: '12:responsible' } });
  fireEvent.change(screen.getByRole('combobox', { name: 'Nueva persona responsable del rol' }), { target: { value: '13' } });
  getMock.mockResolvedValueOnce({ ...editorContext, aggregateRevision: '5', eligiblePartyIds: [101] });
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente página de personas' }));
  await screen.findByText('Revisión conjunta: 5');
  const source = screen.getByRole('combobox', { name: 'Asignación que se reemplaza' });
  expect(source).toHaveValue('');
  fireEvent.change(source, { target: { value: '12:responsible' } });
  const recipient = screen.getByRole('combobox', { name: 'Nueva persona responsable del rol' });
  expect(within(recipient).queryByRole('option', { name: 'Persona #13' })).toBeNull();
  expect(within(recipient).getByRole('option', { name: 'Persona #101' })).toBeTruthy();
  expect(getMock.mock.calls.at(-1)?.[0]).toContain('afterPartyId=100');
  expect(postMock).not.toHaveBeenCalled();
});

test('confirmation is single-flight and only a validated receipt shows success; refresh then reloads canonical task', async () => {
  enableEditor(); const pending = deferred(); postMock.mockReturnValue(pending.promise);
  render(tree()); await screen.findByRole('table');
  const refresh = screen.getByRole('button', { name: 'Actualizar' });
  const dialog = await reviewChange();
  const confirm = within(dialog).getByRole('button', { name: 'Confirmar y enviar' });
  fireEvent.click(confirm); fireEvent.click(confirm);
  expect(postMock).toHaveBeenCalledTimes(1);
  expect(refresh).toBeDisabled();
  expect(postMock.mock.calls[0]?.[0]).toBe('/event-operations/events/80/tasks/8000/raci/reassign');
  expect(postMock.mock.calls[0]?.[1]).toEqual({ expectedRevision: '4', role: 'responsible',
    fromPartyId: 12, toPartyId: 13, reason: 'Cambio de turno de prueba', correlationId: expect.stringContaining('raci-web:') });
  expect(postMock.mock.calls[0]![2]!.headers).toMatchObject({ Authorization: 'Bearer synthetic-a' });
  expect(screen.queryByText('Reasignación confirmada por el servidor.')).toBeNull();
  await act(async () => pending.resolve(accepted()));
  await screen.findByText('Reasignación confirmada por el servidor.');
  expect(refresh).not.toBeDisabled();
  expect(getMock).toHaveBeenCalledTimes(2);
  fireEvent.click(within(dialog).getByRole('button', { name: 'Volver a consultar la tarea' }));
  await screen.findByRole('table'); expect(getMock).toHaveBeenCalledTimes(3);
});

test('uncertain outcome keeps exact body and key on explicit retry, including after replay conflict', async () => {
  enableEditor(); postMock.mockRejectedValueOnce(new Error('lost response'));
  render(tree()); const dialog = await reviewChange();
  fireEvent.click(within(dialog).getByRole('button', { name: 'Confirmar y enviar' }));
  await screen.findByText(/No sabemos si se aplicó/);
  const original = postMock.mock.calls[0]!;
  expect(within(dialog).queryByRole('button', { name: 'Volver sin enviar' })).toBeNull();
  const unload = new Event('beforeunload', { cancelable: true });
  window.dispatchEvent(unload); expect(unload.defaultPrevented).toBe(true);
  postMock.mockRejectedValueOnce(Object.assign(new Error('conflict'), { status: 409 }));
  fireEvent.click(within(dialog).getByRole('button', { name: 'Reintentar la misma solicitud' }));
  await screen.findByText(/No sabemos si se aplicó/);
  expect(postMock.mock.calls[1]?.[1]).toEqual(original?.[1]);
  expect(postMock.mock.calls[1]![2]!.headers).toEqual(original[2]!.headers);
  expect(screen.queryByText(/La tarea cambió/)).toBeNull();
  postMock.mockImplementation(async () => accepted(true));
  fireEvent.click(within(dialog).getByRole('button', { name: 'Reintentar la misma solicitud' }));
  await screen.findByText('Reasignación confirmada por el servidor.');
  expect(postMock).toHaveBeenCalledTimes(3); expect(getMock).toHaveBeenCalledTimes(2);
});

test('initial conflict requires a fresh review and never retries or rebases automatically', async () => {
  enableEditor(); postMock.mockRejectedValue(Object.assign(new Error('private conflict details'), { status: 409 }));
  render(tree()); const dialog = await reviewChange();
  fireEvent.click(within(dialog).getByRole('button', { name: 'Confirmar y enviar' }));
  await screen.findByText(/La tarea cambió/);
  expect(screen.queryByText('private conflict details')).toBeNull();
  expect(screen.queryByRole('button', { name: 'Reintentar la misma solicitud' })).toBeNull();
  expect(postMock).toHaveBeenCalledTimes(1); expect(getMock).toHaveBeenCalledTimes(2);
});

test('malformed success remains uncertain rather than optimistic success', async () => {
  enableEditor(); postMock.mockResolvedValue({ success: true });
  render(tree()); const dialog = await reviewChange();
  fireEvent.click(within(dialog).getByRole('button', { name: 'Confirmar y enviar' }));
  await screen.findByText(/No sabemos si se aplicó/);
  expect(screen.queryByText('Reasignación confirmada por el servidor.')).toBeNull();
});

test('rotation removes the old review and cannot send with stale authority', async () => {
  enableEditor(); const view = render(tree()); const dialog = await reviewChange();
  const oldConfirm = within(dialog).getByRole('button', { name: 'Confirmar y enviar' });
  session = { ...session!, apiToken: 'rotated' }; view.rerender(tree());
  fireEvent.click(oldConfirm);
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  expect(postMock).not.toHaveBeenCalled();
});

test('session invalidation before rerender prevents confirmation dispatch', async () => {
  enableEditor(); render(tree()); const dialog = await reviewChange();
  session = null;
  fireEvent.click(within(dialog).getByRole('button', { name: 'Confirmar y enviar' }));
  expect(postMock).not.toHaveBeenCalled();
});

test('late context after navigation cannot populate the next task editor', async () => {
  enableEditor(); const pending = deferred(); const view = render(tree()); await screen.findByRole('table');
  getMock.mockReturnValueOnce(pending.promise);
  fireEvent.click(screen.getByRole('button', { name: 'Preparar reasignación' }));
  const signal = getMock.mock.calls.at(-1)?.[1]?.signal;
  getMock.mockResolvedValue({ ...fixture, activityId: 9000 }); view.rerender(tree('80', '9000'));
  expect(signal?.aborted).toBe(true);
  await act(async () => pending.resolve(editorContext));
  expect(screen.queryByRole('combobox')).toBeNull();
});

test('late accepted command after logout is ignored and no task is updated optimistically', async () => {
  enableEditor(); const pending = deferred(); postMock.mockReturnValue(pending.promise);
  const view = render(tree()); const dialog = await reviewChange();
  fireEvent.click(within(dialog).getByRole('button', { name: 'Confirmar y enviar' }));
  const signal = postMock.mock.calls[0]![2]!.signal;
  session = null; view.rerender(tree()); expect(signal?.aborted).toBe(true);
  await act(async () => pending.resolve(accepted()));
  expect(screen.queryByText('Reasignación confirmada por el servidor.')).toBeNull();
  expect(screen.queryByRole('table')).toBeNull();
});

test('editor instructions and form labels have an English fallback', async () => {
  english = true; enableEditor(); render(tree());
  fireEvent.click(await screen.findByRole('button', { name: 'Prepare reassignment' }));
  await screen.findByRole('combobox', { name: 'Assignment to replace' });
  expect(screen.getByRole('textbox', { name: 'Reason for reassignment' })).toBeTruthy();
});

test('missing secure UUID support cannot create a review or send a weak-key command', async () => {
  enableEditor(); render(tree());
  const uuid = jest.spyOn(crypto, 'randomUUID').mockImplementation(() => { throw new Error('unavailable'); });
  try {
    fireEvent.change(await openEditor(), { target: { value: '12:responsible' } });
    fireEvent.change(screen.getByRole('combobox', { name: 'Nueva persona responsable del rol' }), { target: { value: '13' } });
    fireEvent.change(screen.getByRole('textbox'), { target: { value: 'Cambio de prueba' } });
    fireEvent.click(screen.getByRole('button', { name: 'Revisar cambio' }));
    await screen.findByText(/No pudimos generar una clave segura/);
    expect(screen.queryByRole('dialog')).toBeNull(); expect(postMock).not.toHaveBeenCalled();
  } finally { uuid.mockRestore(); }
});

test('double opening is one context read and already assigned same-role recipients are excluded', async () => {
  enableEditor({ ...editorContext, replaceableAssignments: [...fixture.raci, { partyId: 13, role: 'responsible' }] });
  render(tree()); const open = await screen.findByRole('button', { name: 'Preparar reasignación' });
  fireEvent.click(open); fireEvent.click(open);
  fireEvent.change(await screen.findByRole('combobox', { name: 'Asignación que se reemplaza' }),
    { target: { value: '12:responsible' } });
  const recipient = screen.getByRole('combobox', { name: 'Nueva persona responsable del rol' });
  expect(within(recipient).queryByRole('option', { name: 'Persona #13' })).toBeNull();
  expect(within(recipient).getByRole('option', { name: 'Persona #11' })).toBeTruthy();
  expect(getMock).toHaveBeenCalledTimes(2); expect(postMock).not.toHaveBeenCalled();
});
