import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { readFileSync } from 'node:fs';
import yaml from 'yaml';

const claims = jest.fn<() => Promise<Record<string, unknown>[]>>();
const decide = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('../api/directory', () => ({ Directory: {
  adminClaims: claims, adminVerifications: jest.fn(), moderationQueue: jest.fn(),
  setClaimStatus: decide, mergeProfiles: jest.fn(),
} }));
const { default: DirectoryAdminPage } = await import('./DirectoryAdminPage');
const contract = yaml.parse(readFileSync('../docs/music-directory/formal-model.yaml', 'utf8')) as {
  state_machines: { claim: { transitions: Record<string, string[]> } };
};
const labels: Record<string, string> = {
  submitted: 'Enviar a revisión', under_review: 'Tomar revisión', withdrawn: 'Retirar',
  approved: 'Aprobar', rejected: 'Rechazar', more_evidence_requested: 'Solicitar evidencia',
};
let client: QueryClient;
beforeEach(() => {
  claims.mockReset(); decide.mockReset();
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
});
afterEach(() => { cleanup(); client.clear(); });
const show = () => render(<QueryClientProvider client={client}><DirectoryAdminPage /></QueryClientProvider>);
const fixture = (status: string) => ({ id: 'claim-1', profileName: 'Perfil sintético', status, claimType: 'profile' });

it.each(Object.entries(contract.state_machines.claim.transitions))(
  'offers precisely the declared outgoing claim edges from %s', async (status, targets) => {
    claims.mockResolvedValue([fixture(status)]); show();
    const heading = await screen.findByRole('heading', { name: 'Perfil sintético' });
    const card = heading.closest('.MuiCard-root');
    if (!(card instanceof HTMLElement)) throw new Error('Missing review card');
    expect(within(card).queryAllByRole('button').map(button => button.textContent).sort())
      .toEqual(targets.map(target => labels[target]).sort());
  },
);

it('keeps authoritative state during a request and preserves a rejected decision for recovery', async () => {
  claims.mockResolvedValue([fixture('under_review')]);
  let rejectDecision: ((error: Error) => void) | undefined;
  decide.mockImplementation(() => new Promise((_, reject) => { rejectDecision = reject; }));
  show();
  const button = await screen.findByRole('button', { name: 'Aprobar' });
  fireEvent.click(button);
  await waitFor(() => expect((button as HTMLButtonElement).disabled).toBe(true));
  expect(screen.getByText('under_review')).toBeTruthy();
  expect(decide).toHaveBeenCalledWith('claim-1', 'approved', expect.any(String));
  await act(async () => { rejectDecision?.(new Error('La decisión cambió; recarga la revisión.')); });
  expect(await screen.findByText('La decisión cambió; recarga la revisión.')).toBeTruthy();
  expect(screen.getByText('under_review')).toBeTruthy();
  expect(claims).toHaveBeenCalledTimes(1);
});

it('renders the claimant and submitted evidence as text without activating markup', async () => {
  claims.mockResolvedValue([{ ...fixture('under_review'), claimantPartyId: 42,
    evidence: [{ note: '<script>untrusted submitted evidence</script>' }] }]);
  show();
  expect(await screen.findByText('Solicitante (Party): 42')).toBeTruthy();
  expect(screen.getByText(/untrusted submitted evidence/).tagName).toBe('PRE');
  expect(document.querySelector('script')).toBeNull();
});
