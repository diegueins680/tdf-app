import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { PartySelectorOption } from '../api/partySelector';
import type { DriveFileInfo } from '../services/googleDrive';
import '../i18n';

let ownerId = 42;
const create = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const contact: PartySelectorOption = { partyId: 11, displayName: 'Cliente Once', partyType: 'person',
  username: null, avatarUrl: null, secondaryLabel: null, accountStatus: 'active' };
jest.unstable_mockModule('../session/SessionContext', () => ({
  useOptionalSession: () => ({ session: { partyId: ownerId }, loading: false }),
  useSession: () => ({ session: { partyId: ownerId }, loading: false }),
  getActiveSession: () => ({ partyId: ownerId }), getStoredSessionToken: () => null,
}));
jest.unstable_mockModule('../api/payments', () => ({ Payments: { create } }));
jest.unstable_mockModule('../components/SessionInvoiceGeneratorCard', () => ({ default: () => null }));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ currency: 'USD', supportedCurrencies: ['USD'], locale: 'es', timezone: 'America/Guayaquil' }),
}));
jest.unstable_mockModule('../contexts/CurrencyContext', () => ({ useCurrency: () => ({ formatMoney: (value: number) => String(value) }) }));
jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({
  PartySelector: ({ value, onChange }: { value: PartySelectorOption | null; onChange: (value: PartySelectorOption) => void }) => <>
    <button onClick={() => onChange(contact)}>Elegir contacto</button><span>{value?.displayName}</span>
  </>,
}));
jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({
  default: ({ onComplete }: { onComplete: (files: DriveFileInfo[]) => void }) =>
    <button onClick={() => onComplete([{ id: 'owned-file', name: 'comprobante.pdf', webContentLink: 'https://api.tdf.test/storage/files/owned-file' }])}>Adjuntar comprobante</button>,
}));
const { PaymentForm } = await import('./PaymentsPage');
function openForm(onCreated = jest.fn()) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  return { ...render(<QueryClientProvider client={client}><PaymentForm payments={[]} onCreated={onCreated} /></QueryClientProvider>), client, onCreated };
}
beforeEach(() => { sessionStorage.clear(); localStorage.clear(); ownerId = 42; create.mockReset().mockResolvedValue({ payId: 1 }); });

it('restores the chosen contact, edited payment fields and attachment after remount', async () => {
  const first = openForm();
  fireEvent.click(screen.getByRole('button', { name: 'Elegir contacto' }));
  fireEvent.change(screen.getByLabelText(/Monto/), { target: { value: '120.50' } });
  fireEvent.change(screen.getByLabelText('Referencia'), { target: { value: 'referencia-propia' } });
  fireEvent.change(screen.getByLabelText('Periodo'), { target: { value: 'Periodo personalizado' } });
  fireEvent.click(screen.getByRole('button', { name: 'Adjuntar comprobante' }));
  first.unmount(); first.client.clear();
  const next = openForm();
  await screen.findByText('Cliente Once');
  expect((screen.getByLabelText(/Monto/)).value).toBe('120.50');
  expect((screen.getByLabelText('Referencia')).value).toBe('referencia-propia');
  expect((screen.getByLabelText('Periodo')).value).toBe('PERIODO PERSONALIZADO');
  expect((screen.getByLabelText('URL de respaldo (opcional)')).value).toBe('https://api.tdf.test/storage/files/owned-file');
  expect(create).not.toHaveBeenCalled();
  next.unmount(); next.client.clear();
});
it('does not let a completed earlier request erase edits made while saving', async () => {
  let finish: ((result: unknown) => void) | undefined;
  create.mockImplementation(() => new Promise((resolve) => { finish = resolve; }));
  const view = openForm();
  fireEvent.click(screen.getByRole('button', { name: 'Elegir contacto' }));
  fireEvent.change(screen.getByLabelText(/Monto/), { target: { value: '10' } });
  const attachmentUrl = screen.getByLabelText('URL de respaldo (opcional)');
  const submit = screen.getByRole('button', { name: 'Registrar pago' });
  expect(attachmentUrl.compareDocumentPosition(submit) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  fireEvent.click(submit);
  await waitFor(() => expect(create).toHaveBeenCalledTimes(1));
  fireEvent.change(screen.getByLabelText(/Monto/), { target: { value: '20' } });
  await act(async () => { finish?.({ payId: 1 }); });
  expect((screen.getByLabelText(/Monto/)).value).toBe('20');
  expect(view.onCreated).not.toHaveBeenCalled();
  view.unmount(); view.client.clear();
});
