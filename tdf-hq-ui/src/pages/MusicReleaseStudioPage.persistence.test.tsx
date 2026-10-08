import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { MusicReleaseVersion } from '../api/musicReleases';

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
const saveContent = jest.fn<() => Promise<MusicReleaseVersion>>();
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
  saveMetadata.mockReset().mockResolvedValue({ ...version, updatedAt: '2026-09-15T00:00:01Z' });
  saveContent.mockReset().mockResolvedValue({ ...version, updatedAt: '2026-09-15T00:00:02Z' });
  acceptTerms.mockReset().mockResolvedValue(undefined);
  validate.mockReset().mockResolvedValue({ valid: true, errors: [] });
  transition.mockReset().mockResolvedValue(undefined);
  uploadAsset.mockReset().mockResolvedValue(undefined);
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
