import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
const transient = jest.fn();
const fetchMock = jest.fn<typeof fetch>();
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: null }), setTransientApiToken: transient }));
jest.unstable_mockModule('../config/apiBase', () => ({ resolveApiBase: () => '' }));
jest.unstable_mockModule('../components/PublicBrandBar', () => ({ default: () => null }));
jest.unstable_mockModule('./LiveSessionIntakePage', () => ({ LiveSessionIntakeForm: ({ accessCode }: { accessCode?: string }) => <input aria-label="Nombre de banda" data-access-code={accessCode ?? ''} /> }));
const { default: Page } = await import('./LiveSessionPublicPage');
const response = (body: unknown) => ({ ok: true, status: 200, json: async () => body }) as Response;
const show = () => render(<MemoryRouter><Page /></MemoryRouter>);
const enter = (code: string) => fireEvent.change(screen.getByLabelText('Código de acceso'), { target: { value: code } });
const validate = () => fireEvent.click(screen.getByRole('button', { name: 'Validar código' }));
beforeEach(() => { jest.clearAllMocks(); global.fetch = fetchMock; });
afterEach(() => cleanup());
it('validates the existing session endpoint even with a same-origin API and enables only a persisted account', async () => {
 fetchMock.mockResolvedValue(response({ partyId: 42 })); show();
 expect(screen.getByLabelText('Nombre de banda')).toBeDisabled();
 enter('synthetic-code'); validate();
 await waitFor(() => expect(screen.getByLabelText('Nombre de banda')).toBeEnabled());
 expect(fetchMock).toHaveBeenCalledWith('/session', expect.objectContaining({ headers: { Authorization: 'Bearer synthetic-code' }, credentials: 'omit', signal: expect.any(AbortSignal) }));
 expect(screen.getByLabelText('Nombre de banda')).toHaveAttribute('data-access-code', 'synthetic-code');
 expect(transient).not.toHaveBeenCalled();
});
it.each([null, {}, { status: 'ok' }, { partyId: -1 }])('rejects a superficial successful response: %j', async (body) => {
 fetchMock.mockResolvedValue(response(body)); show(); enter('synthetic-invalid'); validate();
 await screen.findAllByText(/No pudimos validar el código/);
 expect(screen.getByLabelText('Nombre de banda')).toBeDisabled();
 expect(transient).not.toHaveBeenCalledWith('synthetic-invalid');
});
it('ignores an old successful validation after the code changes', async () => {
 let finish!: (value: Response) => void;
 const old = new Promise<Response>(resolve => { finish = resolve; });
 fetchMock.mockReturnValueOnce(old); show(); enter('synthetic-old'); validate();
 await waitFor(() => expect(fetchMock).toHaveBeenCalledTimes(1));
 enter('synthetic-new');
 await act(async () => { finish(response({ partyId: 42 })); await old; });
 expect(screen.getByLabelText('Nombre de banda')).toBeDisabled();
 expect(transient).not.toHaveBeenCalledWith('synthetic-old');
 expect(transient).not.toHaveBeenCalledWith('synthetic-new');
 fetchMock.mockResolvedValue(response({ partyId: 43 })); validate();
 await waitFor(() => expect(screen.getByLabelText('Nombre de banda')).toBeEnabled());
 expect(screen.getByLabelText('Nombre de banda')).toHaveAttribute('data-access-code', 'synthetic-new');
});
it('retains recoverable form input while a changed code is unverified', async () => {
 fetchMock.mockResolvedValue(response({ partyId: 42 }));show();enter('synthetic-valid');validate();
 await waitFor(()=>expect(screen.getByLabelText('Nombre de banda')).toHaveAttribute('data-access-code', 'synthetic-valid'));
 const band=screen.getByLabelText('Nombre de banda');
 fireEvent.change(band,{target:{value:'Banda sintética'}});enter('');
 expect(band).toBeDisabled();expect(band).toHaveValue('Banda sintética');
 expect(band).toHaveAttribute('data-access-code', '');
 expect(transient).not.toHaveBeenCalled();
});

it('invalidates a previous request even when the user returns to the same code', async () => {
 let finish!: (value: Response) => void;
 const old = new Promise<Response>(resolve => { finish = resolve; });
 fetchMock.mockReturnValueOnce(old); show(); enter('synthetic-same'); validate();
 enter('synthetic-other'); enter('synthetic-same');
 await act(async () => { finish(response({ partyId: 42 })); await old; });
 expect(screen.getByLabelText('Nombre de banda')).toBeDisabled();
 expect(transient).not.toHaveBeenCalledWith('synthetic-same');
});
