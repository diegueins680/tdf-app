import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { useTheme } from '@mui/material/styles';
const transient = jest.fn();
const fetchMock = jest.fn<typeof fetch>();
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: null }), setTransientApiToken: transient }));
jest.unstable_mockModule('../config/apiBase', () => ({ resolveApiBase: () => '' }));
jest.unstable_mockModule('../components/PublicBrandBar', () => ({ default: () => null }));
function MockIntakeForm({ accessCode }: { accessCode?: string }) {
 const theme = useTheme();
 return <input aria-label="Nombre de banda" data-access-code={accessCode ?? ''} data-palette-mode={theme.palette.mode} data-text-secondary={theme.palette.text.secondary} data-action-active={theme.palette.action.active} />;
}
jest.unstable_mockModule('./LiveSessionIntakePage', () => ({ LiveSessionIntakeForm: MockIntakeForm }));
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

const luminance = (hex: string) => {
 const [r, g, b] = [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16) / 255).map((c) => (c <= 0.03928 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4));
 return 0.2126 * r! + 0.7152 * g! + 0.0722 * b!;
};
it('renders the navy shell and embedded form under a dark theme so muted copy and icons stay legible', () => {
 show();
 const form = screen.getByLabelText('Nombre de banda');
 expect(form).toHaveAttribute('data-palette-mode', 'dark');
 // text.secondary on the navy paper (#111a2e) must clear WCAG AA (4.5:1).
 const secondary = form.getAttribute('data-text-secondary') ?? '';
 expect(secondary).toMatch(/^#[0-9a-f]{6}$/i);
 const ratio = (luminance(secondary) + 0.05) / (luminance('#111a2e') + 0.05);
 expect(ratio).toBeGreaterThanOrEqual(4.5);
 // Adornment icons (the eye toggle) use action.active, which must be light on dark.
 expect(form.getAttribute('data-action-active')).toMatch(/255, 255, 255|#fff/i);
 const toggle = screen.getByRole('button', { name: 'Mostrar código' });
 fireEvent.click(toggle);
 expect(screen.getByRole('button', { name: 'Ocultar código' })).toBeInTheDocument();
});
