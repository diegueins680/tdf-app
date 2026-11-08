import { jest } from '@jest/globals';
const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown, init?: RequestInit) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get: getMock, post: postMock, postEmpty: jest.fn() }));
const api = await import('./eventCrm');
const snapshot = {
  event: { id: 10, title: 'Ensayo privado', startTime: '2026-12-05T16:00:00Z', venue: 'Sede de ensayo', organizerId: '1', timezone: 'America/Guayaquil' },
  role: 'sales', leads: [], members: [], invitations: [], orders: [], manualPayments: [], sales: [], activities: [], transport: [], transportRequired: false, inventory: [], waitlist: [], combinationSalesEnabled: false, audit: [],
};
beforeEach(() => { getMock.mockReset(); postMock.mockReset(); });
describe('private event CRM transport and CSV boundaries', () => {
  it('requests uncached scoped filters and rejects a mismatched event response', async () => {
    getMock.mockResolvedValue(snapshot);
    await expect(api.getEventCrm('10', { stage: 'contacted', responsibleId: '4', overdue: true })).resolves.toEqual(snapshot);
    expect(getMock).toHaveBeenCalledWith('/event-crm/events/10?stage=contacted&responsibleId=4&overdue=true', { cache: 'no-store' });
    await expect(api.getEventCrm('11')).rejects.toThrow('otro evento');
  });
  it('exports only a fresh server-authorized snapshot', async () => {
    getMock.mockResolvedValue(snapshot); await api.exportEventCrm('10');
    expect(getMock).toHaveBeenCalledWith('/event-crm/events/10/export', { cache: 'no-store' });
    getMock.mockRejectedValue(new Error('403')); await expect(api.exportEventCrm('10')).rejects.toThrow('403');
  });
  it('posts secret tokens in the body, never in the request URL', async () => {
    postMock.mockResolvedValue({ eventId: 10, membershipId: '33333333-3333-4333-8333-333333333333', role: 'sales' });
    await api.acceptTeamInvitation('synthetic-secret');
    expect(postMock).toHaveBeenCalledWith('/event-crm/invitations', { token: 'synthetic-secret', accept: true });
  });
  it('keeps retry identity separate from the payload', async () => {
    const key = '33333333-3333-4333-8333-333333333333'; const body = { operation: 'lead.update', notes: 'follow-up' };
    postMock.mockResolvedValue({ saved: true }); await api.crmCommand('10', body, key);
    expect(postMock).toHaveBeenCalledWith('/event-crm/events/10/commands', body, { headers: { 'Idempotency-Key': key } });
  });
  it('reads quoted names, multiline notes, escaped quotes, BOM and CRLF', () => {
    expect(api.parseCrmCsv('\uFEFFNombre,Notas\r\n"Persona, ejemplo","Dijo ""hola""\ny volvió"\r\n')).toEqual([['Nombre', 'Notas'], ['Persona, ejemplo', 'Dijo "hola"\ny volvió']]);
  });
  it.each(['Nombre\n"sin cerrar', 'Nombre\n"cerrado"texto', 'Nombre\nmal"formado'])('rejects malformed CSV %s', value => expect(() => api.parseCrmCsv(value)).toThrow());
  it('bounds rows, columns and file size before any request', () => {
    expect(() => api.parseCrmCsv('n\n' + 'x\n'.repeat(501))).toThrow('500');
    expect(() => api.parseCrmCsv('n\n' + 'x,'.repeat(41))).toThrow('40');
    expect(() => api.parseCrmCsv('x'.repeat(1_000_001))).toThrow('1 MB');
    expect(postMock).not.toHaveBeenCalled();
  });
  it('maps Spanish stages as claims, preserving payment reconciliation for the server', () => {
    expect(api.mapCrmRows([['Nombre', 'Estado', 'Entradas'], ['Prueba', 'Pagado', '2']], { name: 0, stage: 1, quantity: 2 })).toEqual([{ name: 'Prueba', stage: 'paid', quantity: 2, source: 'lista manual' }]);
    expect(() => api.mapCrmRows([['n', 'q'], ['Prueba', '1.5']], { name: 0, quantity: 1 })).toThrow('cantidades');
  });
  it('does not interpret spreadsheet formulas or markup', () => {
    expect(api.mapCrmRows([['n', 'nota'], ['=HYPERLINK("bad")', '<script>'] ], { name: 0, notes: 1 })[0]?.notes).toBe('<script>');
  });
});
