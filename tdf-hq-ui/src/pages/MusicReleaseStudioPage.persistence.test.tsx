import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { MusicReleaseVersion, MusicReleaseContentInput } from '../api/musicReleases';

// Full MUI Studio renders, including all rights/access controls. This is a
// component-correctness budget, not an API latency or autosave debounce limit.
jest.setTimeout(15_000);

const version = {
  id: 'version', releaseId: 'release', versionNumber: 1, state: 'draft', title: 'Borrador',
  displayArtist: 'Artista sintético', titleLanguage: 'es', explicitContent: 'unknown',
  tracks: [], parties: [], credits: [], identifiers: [], rights: [], availability: [],
  terms: [], assets: [], comments: [], validation: { errors: [] }, updatedAt: '2026-09-15T00:00:00Z',
} as unknown as MusicReleaseVersion;
const saveMetadata = jest.fn<() => Promise<MusicReleaseVersion>>();
const saveContent = jest.fn<(releaseId: string, versionId: string, input: MusicReleaseContentInput) => Promise<MusicReleaseVersion>>();
const acceptTerms = jest.fn<() => Promise<void>>();
const validate = jest.fn<() => Promise<{ valid: boolean; errors: [] }>>();
const transition = jest.fn<() => Promise<void>>();
const uploadAsset = jest.fn<() => Promise<void>>();
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: {
  partyId: 101, displayName: 'Artista sintético', roles: ['Artist'],
} }) }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listPublicItems: async () => ({ items: [] }) } }));
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: {
  getVersion: async () => version, analytics: async () => null,
  saveMetadata, saveContent, acceptTerms, validate, transition, uploadAsset,
} }));
const { default: Studio } = await import('./MusicReleaseStudioPage');

beforeEach(() => {
  version.availability = [];
  version.assets = [];
  version.tracks = [];
  saveMetadata.mockReset().mockResolvedValue({ ...version, updatedAt: '2026-09-15T00:00:01Z' });
  saveContent.mockReset().mockResolvedValue({ ...version, updatedAt: '2026-09-15T00:00:02Z' });
  acceptTerms.mockReset().mockResolvedValue(undefined);
  validate.mockReset().mockResolvedValue({ valid: true, errors: [] });
  transition.mockReset().mockResolvedValue(undefined);
  uploadAsset.mockReset().mockResolvedValue(undefined);
});

function configurePrice(price: number | string = '9007199254740993', currency = 'USD') {
  const assetId = '00000000-0000-4000-8000-000000000001';
  version.tracks = [{ trackId: '00000000-0000-4000-8000-000000000002',
    recordingId: '00000000-0000-4000-8000-000000000003', discNumber: 1, trackNumber: 1,
    displayArtist: 'Artista sintético', isPrimaryResource: true, previewStartMs: null,
    previewDurationMs: null, title: 'Pista sintética', subtitle: null, versionTitle: null,
    titleLanguage: 'es', titleScript: null, durationMs: 60000, explicitContent: 'not_explicit' }];
  version.availability = [{ territory_mode: 'include', territories: ['Worldwide'],
    starts_at: null, ends_at: null, listening_policy: 'none', download_policy: 'purchase',
    purchasable: true, price_minor: price, currency, downloadable_asset_id: assetId }];
  version.assets = [{ id: assetId, recordingId: null, parentAssetId: 'source', role: 'stream_audio',
    originalFilename: 'synthetic.mp3', mediaType: 'audio/mpeg', byteSize: 4, sha256: 'a'.repeat(64),
    processingState: 'ready', technicalMetadata: {}, createdAt: version.updatedAt, readyAt: version.updatedAt }];
  saveMetadata.mockResolvedValue({ ...version, updatedAt: '2026-09-15T00:00:01Z' });
  saveContent.mockImplementation(async (_release, _version, input) => ({ ...version,
    availability: version.availability.map((value, index) => ({ ...(value as Record<string, unknown>),
      price_minor: input.availability[index]?.priceMinor, currency: input.availability[index]?.currency,
      download_policy: input.availability[index]?.downloadPolicy,
    })), updatedAt: '2026-09-15T00:00:02Z' }));
}

it('keeps a large authored price as exact decimal text through the actual form', async () => {
  configurePrice();
  const close = await openStudio();
  try {
    const field = await screen.findByRole<HTMLInputElement>('textbox', { name: 'Precio (USD)' });
    expect(field.type).toBe('text');
    expect(field.inputMode).toBe('decimal');
    expect(field.value).toBe('90071992547409.93');
    fireEvent.change(field, { target: { value: '90071992547409.94' } });
    fireEvent.keyDown(field, { key: 'ArrowUp' });
    expect(() => field.stepUp()).toThrow();
    expect(field.value).toBe('90071992547409.94');
    fireEvent.change(field, { target: { value: '92233720368547758.07' } });
    expect(field.value).toBe('92233720368547758.07');
    await click('Guardar ahora');
    await waitFor(() => expect(saveContent).toHaveBeenCalledTimes(1));
    expect(saveContent).toHaveBeenCalledWith('release', 'version', expect.objectContaining({
      availability: [expect.objectContaining({ priceMinor: '9223372036854775807' })],
    }));
  } finally { close(); version.availability = []; version.assets = []; version.tracks = []; }
});

async function openStudio() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0, staleTime: Infinity } } });
  client.setQueryData(['music-release-version', 'release', 'version'], version);
  client.setQueryData(['catalog', 'genres', 'music-release-studio'], { items: [] });
  client.setQueryData(['music-release-analytics', 'release', '', ''], null);
  const view = render(<QueryClientProvider client={client}><MemoryRouter initialEntries={['/release/version']}>
    <Routes><Route path="/:releaseId/:versionId" element={<Studio />} /></Routes>
  </MemoryRouter></QueryClientProvider>);
  expect(screen.getByDisplayValue('Borrador')).toBeTruthy();
  return () => { view.unmount(); client.clear(); };
}

function button(name: string) {
  const element = screen.getByText(name, { exact: true }).closest('button');
  if (!element) throw new Error(`Missing button: ${name}`);
  return element;
}

async function click(name: string) {
  await act(async () => { fireEvent.click(button(name)); });
}

function acceptDeclaration() {
  const input = document.querySelector('input[type="checkbox"]');
  if (!input) throw new Error('Missing authority checkbox');
  fireEvent.click(input);
}

it.each(['Aceptar y validar', 'Enviar a revisión'])('does not execute %s after a content save failure', async (action) => {
  saveContent.mockRejectedValue(new Error('Splits incompletos'));
  const close = await openStudio();
  try {
    fireEvent.change(screen.getByDisplayValue('Borrador'), { target: { value: 'Cambio pendiente' } });
    acceptDeclaration();
    await click(action);
    await waitFor(() => expect(saveContent).toHaveBeenCalledTimes(1));
    await screen.findByText('Splits incompletos');
    expect(acceptTerms).not.toHaveBeenCalled();
    expect(validate).not.toHaveBeenCalled();
    expect(transition).not.toHaveBeenCalled();
    expect(screen.getByDisplayValue('Cambio pendiente')).toBeTruthy();
  } finally { close(); }
});

it('locks the draft snapshot while saving and prevents a parallel submission', async () => {
  let finish: (value: MusicReleaseVersion) => void = () => undefined;
  saveContent.mockImplementationOnce(() => new Promise((resolve) => { finish = resolve; }));
  const close = await openStudio();
  try {
    fireEvent.change(screen.getByDisplayValue('Borrador'), { target: { value: 'Cambio pendiente' } });
    await click('Guardar ahora');
    await waitFor(() => expect(saveContent).toHaveBeenCalledTimes(1));
    const title = screen.getByDisplayValue('Cambio pendiente');
    expect(title.matches(':disabled')).toBe(true);
    expect(button('Enviar a revisión').matches(':disabled')).toBe(true);
    await click('Enviar a revisión');
    expect(transition).not.toHaveBeenCalled();
    await act(async () => { finish({ ...version, title: 'Cambio pendiente' }); });
    await waitFor(() => expect(title.matches(':disabled')).toBe(false));
    expect(transition).not.toHaveBeenCalled();
  } finally { close(); }
});

it('validates once, only after both draft transactions succeed', async () => {
  let finish: (value: MusicReleaseVersion) => void = () => undefined;
  saveContent.mockImplementationOnce(() => new Promise((resolve) => { finish = resolve; }));
  const close = await openStudio();
  try {
    fireEvent.change(screen.getByDisplayValue('Borrador'), { target: { value: 'Cambio pendiente' } });
    acceptDeclaration();
    await click('Aceptar y validar');
    await waitFor(() => expect(saveContent).toHaveBeenCalledTimes(1));
    await click('Aceptar y validar');
    expect(acceptTerms).not.toHaveBeenCalled();
    await act(async () => { finish({ ...version, title: 'Cambio pendiente' }); });
    await waitFor(() => expect(validate).toHaveBeenCalledTimes(1));
    expect(acceptTerms).toHaveBeenCalledTimes(1);
    expect(saveMetadata).toHaveBeenCalledTimes(1);
    expect(saveContent).toHaveBeenCalledTimes(1);
  } finally { close(); }
});

it('keeps the successful metadata revision when content fails, so an explicit retry is not stale', async () => {
  saveContent.mockRejectedValueOnce(new Error('Splits incompletos'));
  const close = await openStudio();
  try {
    fireEvent.change(screen.getByDisplayValue('Borrador'), { target: { value: 'Cambio pendiente' } });
    await click('Guardar ahora');
    await screen.findByText('Splits incompletos');
    await click('Guardar ahora');
    await waitFor(() => expect(saveMetadata).toHaveBeenCalledTimes(2));
    expect(saveMetadata.mock.calls[1]).toEqual(['release', 'version', expect.objectContaining({ expectedUpdatedAt: '2026-09-15T00:00:01Z' })]);
  } finally { close(); }
});

it('does not upload a cover if the draft could not be saved', async () => {
  saveMetadata.mockRejectedValue(new Error('Conflicto de versión'));
  const close = await openStudio();
  try {
    fireEvent.change(screen.getByDisplayValue('Borrador'), { target: { value: 'Cambio pendiente' } });
    const input = document.querySelector('input[accept="image/jpeg,image/png,image/tiff"]');
    if (!input) throw new Error('Missing cover input');
    await act(async () => { fireEvent.change(input, { target: { files: [new File(['synthetic'], 'cover.png', { type: 'image/png' })] } }); });
    await screen.findByText('Conflicto de versión');
    expect(uploadAsset).not.toHaveBeenCalled();
  } finally { close(); }
});


it.each(['-1', '4.999', '1e2', '92233720368547758.08'])('keeps invalid authored USD price %s local and blocks review/validation', async value => {
  configurePrice('499');
  const close = await openStudio();
  try {
    const field = screen.getByRole<HTMLInputElement>('textbox', { name: 'Precio (USD)' });
    fireEvent.change(field, { target: { value } });
    expect(field.value).toBe(value);
    expect(field.getAttribute('aria-invalid')).toBe('true');
    for (const action of ['Guardar ahora', 'Aceptar y validar', 'Enviar a revisión']) {
      expect(button(action).matches(':disabled')).toBe(true);
      await click(action);
    }
    expect(saveMetadata).not.toHaveBeenCalled(); expect(saveContent).not.toHaveBeenCalled();
    expect(acceptTerms).not.toHaveBeenCalled(); expect(validate).not.toHaveBeenCalled(); expect(transition).not.toHaveBeenCalled();
  } finally { close(); }
});

it('pauses autosave on an incomplete price and saves corrected decimal-comma input exactly', async () => {
  configurePrice('499');
  const close = await openStudio();
  try {
    const field = screen.getByRole<HTMLInputElement>('textbox', { name: 'Precio (USD)' });
    fireEvent.change(screen.getByDisplayValue('Borrador'), { target: { value: 'Cambio pendiente' } });
    fireEvent.change(field, { target: { value: '4,' } });
    await act(async () => { await new Promise(resolve => setTimeout(resolve, 1800)); });
    expect(field.value).toBe('4,');
    expect(saveMetadata).not.toHaveBeenCalled(); expect(saveContent).not.toHaveBeenCalled();
    fireEvent.change(field, { target: { value: '4,99' } });
    await click('Guardar ahora');
    await waitFor(() => expect(saveContent).toHaveBeenCalledWith('release', 'version', expect.objectContaining({ availability: [expect.objectContaining({ priceMinor: '499', currency: 'USD' })] })));
  } finally { close(); }
});

it('does not reinterpret unsupported currency as a purchasable offer', async () => {
  configurePrice('499', 'JPY');
  const close = await openStudio();
  try {
    expect(screen.getByRole('textbox', { name: 'Moneda ISO' }).getAttribute('aria-invalid')).toBe('true');
    expect(button('Aceptar y validar').matches(':disabled')).toBe(true);
    expect(saveMetadata).not.toHaveBeenCalled(); expect(saveContent).not.toHaveBeenCalled();
  } finally { close(); }
});

it('keeps an unsafe loaded numeric price unavailable until explicitly corrected', async () => {
  configurePrice(9007199254740992);
  const close = await openStudio();
  try {
    const field = screen.getByRole('textbox', { name: 'Precio (USD)' });
    expect(field.getAttribute('aria-invalid')).toBe('true');
    expect(screen.getByText(/El precio guardado no es exacto/)).toBeTruthy();
    expect(saveContent).not.toHaveBeenCalled();
    fireEvent.change(field, { target: { value: '4,99' } });
    await click('Guardar ahora');
    await waitFor(() => expect(saveContent).toHaveBeenCalledWith('release', 'version', expect.objectContaining({ availability: [expect.objectContaining({ priceMinor: '499' })] })));
  } finally { close(); }
});


it.each(['', '0'])('keeps incomplete purchase price %s saveable as a draft, with review still blocked', async raw => {
  configurePrice('499'); const close = await openStudio();
  try {
    fireEvent.change(screen.getByRole('textbox', { name: 'Precio (USD)' }), { target: { value: raw } });
    expect(button('Guardar ahora').matches(':disabled')).toBe(false);
    expect(button('Aceptar y validar').matches(':disabled')).toBe(true);
    expect(button('Enviar a revisión').matches(':disabled')).toBe(true);
    await click('Guardar ahora');
    await waitFor(() => expect(saveContent).toHaveBeenCalledWith('release', 'version', expect.objectContaining({ availability: [expect.objectContaining({ priceMinor: raw === '' ? null : '0' })] })));
    expect(acceptTerms).not.toHaveBeenCalled(); expect(validate).not.toHaveBeenCalled(); expect(transition).not.toHaveBeenCalled();
  } finally { close(); }
});
