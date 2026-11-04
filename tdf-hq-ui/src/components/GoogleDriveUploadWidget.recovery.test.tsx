import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';

const ensureToken = jest.fn<() => Promise<{ accessToken: string }>>();
const startAuth = jest.fn<() => Promise<void>>();
const upload = jest.fn<(...args: unknown[]) => Promise<{ id: string; name: string; publicUrl: string }>>();
const assetUpload = jest.fn<(...args: unknown[]) => Promise<never>>();
const loadFiles = jest.fn<() => Promise<unknown[]>>();
const removeFile = jest.fn<() => Promise<void>>();
jest.unstable_mockModule('../hooks/useGoogleDriveAuth', () => ({
  useGoogleDriveAuth: () => ({ status: 'idle', error: null, startAuth, ensureToken }),
}));
jest.unstable_mockModule('../api/drive', () => ({ uploadToDrive: upload }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useOptionalSession: () => ({ session: { partyId: 42 }, loading: false }) }));
jest.unstable_mockModule('../api/inventory', () => ({ uploadAssetPhoto: assetUpload }));
jest.unstable_mockModule('../api/contentStorage', () => ({
  availableStorageBytes: () => 5_000_000_000,
  loadStorageAccount: async () => ({ allowanceBytes: 5_000_000_000, usedBytes: 0, reservedBytes: 0, revision: 0, defaultDestination: null, legacyUsageVerified: true }),
  loadStorageFiles: loadFiles, deleteStorageFile: removeFile, downloadStorageFile: async () => undefined, saveStorageDestination: async () => ({}),
}));
const { default: Widget } = await import('./GoogleDriveUploadWidget');

beforeEach(() => {
  sessionStorage.clear();
  ensureToken.mockReset().mockResolvedValue({ accessToken: 'personal-access' });
  startAuth.mockReset().mockResolvedValue(undefined);
  upload.mockReset().mockResolvedValue({ id: 'file-1', name: 'cover.png', publicUrl: 'https://example.test/cover.png' });
  assetUpload.mockReset();
  loadFiles.mockReset().mockResolvedValue([]);
  removeFile.mockReset().mockResolvedValue(undefined);
  URL.createObjectURL = jest.fn(() => 'blob:synthetic-cover');
  URL.revokeObjectURL = jest.fn();
});

async function selectFile(container: HTMLElement) {
  const file = new File(['synthetic cover'], 'cover.png', { type: 'image/png' });
  const input = container.querySelector('input[data-testid=storage-upload-select]');
  if (!input) throw new Error('Missing file input');
  await act(async () => { fireEvent.change(input, { target: { files: [file] } }); });
  return file;
}

it('never falls back to a server upload when the chosen personal authorization fails', async () => {
  ensureToken.mockRejectedValue(new Error('Need consent'));
  const { container } = render(<Widget authMode="user" />);
  await selectFile(container);
  await waitFor(() => expect(ensureToken).toHaveBeenCalledTimes(1));
  expect(upload).not.toHaveBeenCalled();
  expect(assetUpload).not.toHaveBeenCalled();
  expect(screen.getByText('cover.png')).toBeTruthy();
});

it('retains the original File and retry identity after a failed upload', async () => {
  upload.mockRejectedValueOnce(new Error('temporarily unavailable'));
  const complete = jest.fn();
  const { container } = render(<Widget authMode="server" onComplete={complete} />);
  const file = await selectFile(container);
  await waitFor(() => expect(upload).toHaveBeenCalledTimes(1));
  const firstOptions = upload.mock.calls[0]?.[1] as { idempotencyKey?: string };
  expect(firstOptions.idempotencyKey).toMatch(/^[a-f0-9]{64}$/);
  const retry = await screen.findByRole('button', { name: /reintentar.*cover.png/i });
  fireEvent.click(retry);
  await waitFor(() => expect(complete).toHaveBeenCalledTimes(1));
  expect(upload.mock.calls[1]?.[0]).toBe(file);
  expect((upload.mock.calls[1]?.[1] as { idempotencyKey: string }).idempotencyKey).toBe(firstOptions.idempotencyKey);
});

it('keeps a service credential failure away from personal reauthorization instructions', async () => {
  upload.mockRejectedValueOnce(new Error('Refresh token expirado o revocado. Reautoriza Google Drive.'));
  const { container } = render(<Widget authMode="server" />);
  await selectFile(container);
  await waitFor(() => expect(screen.getByText(/almacenamiento de TDF no está disponible/i)).toBeTruthy());
  expect(screen.queryByRole('button', { name: /conectar drive/i })).toBeNull();
  expect(screen.queryByText(/refresh token/i)).toBeNull();
});

it('does not attach a result after the selected upload was cancelled', async () => {
  let finish: ((value: { id: string; name: string; publicUrl: string }) => void) | undefined;
  upload.mockImplementation(() => new Promise((resolve) => { finish = resolve; }));
  const complete = jest.fn();
  const { container } = render(<Widget authMode="server" onComplete={complete} />);
  await selectFile(container);
  await waitFor(() => expect(upload).toHaveBeenCalledTimes(1));
  fireEvent.click(screen.getByRole('button', { name: /Quitar cover.png/ }));
  await act(async () => { finish?.({ id: 'file', name: 'cover.png', publicUrl: 'https://example.test/cover.png' }); });
  expect(complete).not.toHaveBeenCalled();
  expect(screen.queryByText('cover.png')).toBeNull();
});
it('lists owned files and asks explicitly before deleting a file', async () => {
  loadFiles.mockResolvedValueOnce([{ fileId: '12345678-1234-1234-1234-123456789abc', fileName: 'owned.png', byteSize: 50, status: 'completed', visibility: 'public' }]);
  const confirm = jest.spyOn(window, 'confirm').mockReturnValueOnce(false).mockReturnValueOnce(true);
  render(<Widget authMode="server" />);
  fireEvent.click(screen.getByRole('button', { name: 'Administrar almacenamiento TDF' }));
  await screen.findByText('owned.png');
  fireEvent.click(screen.getByRole('button', { name: 'Borrar', exact: true }));
  expect(removeFile).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Borrar', exact: true }));
  await waitFor(() => expect(removeFile).toHaveBeenCalledTimes(1));
  await waitFor(() => expect(screen.queryByText('owned.png')).toBeNull());
  confirm.mockRestore();
});

it('preserves an uncertain request after remount and reselects only its original file', async () => {
  upload.mockRejectedValueOnce(new Error('network unavailable'));
  const first = render(<Widget authMode="server" recoveryScope="artist:7" />);
  const file = await selectFile(first.container);
  await waitFor(() => expect(upload).toHaveBeenCalledTimes(1));
  const key = (upload.mock.calls[0]?.[1] as { idempotencyKey: string }).idempotencyKey;
  first.unmount();
  const restored = render(<Widget authMode="server" recoveryScope="artist:7" />);
  const reselect = await screen.findByRole('button', { name: 'Reseleccionar archivo' });
  fireEvent.click(reselect);
  const input = restored.container.querySelector('input[data-testid=storage-upload-reselect]');
  if (!input) throw new Error('Missing recovery file input');
  await act(async () => { fireEvent.change(input, { target: { files: [file] } }); });
  await waitFor(() => expect(upload).toHaveBeenCalledTimes(2));
  expect((upload.mock.calls[1]?.[1] as { idempotencyKey: string }).idempotencyKey).toBe(key);
});
it('does not attach a late result to a different artist editor under the same account', async () => {
  let finish: ((value: { id: string; name: string; publicUrl: string }) => void) | undefined;
  upload.mockImplementation(() => new Promise((resolve) => { finish = resolve; }));
  const complete = jest.fn();
  const view = render(<Widget authMode="server" recoveryScope="artist:7" onComplete={complete} />);
  await selectFile(view.container);
  await waitFor(() => expect(upload).toHaveBeenCalledTimes(1));
  view.rerender(<Widget authMode="server" recoveryScope="artist:8" onComplete={complete} />);
  await act(async () => { finish?.({ id: 'file', name: 'cover.png', publicUrl: 'https://example.test/cover.png' }); });
  expect(complete).not.toHaveBeenCalled();
});
