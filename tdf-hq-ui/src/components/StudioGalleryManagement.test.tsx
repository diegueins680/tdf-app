import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { render, screen, fireEvent, within, waitFor, cleanup } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import { useState } from 'react';
import type { CmsContentDTO, CmsContentIn } from '../api/cms';
import type { StudioGalleryPayload } from '../utils/studioGallery';
import { parseStudioGallery } from '../utils/studioGallery';
import { studioGallerySeed } from '../__tests__/fixtures/studioGallery';

jest.setTimeout(15000);

const contentId = '2368bb35-3ff7-45b3-96f3-425e11812544';
const live: CmsContentDTO = { ccdId: 1, ccdContentId: contentId, ccdSlug: 'studio-gallery', ccdLocale: 'es', ccdStatus: 'published', ccdVersion: 1, ccdTitle: 'Galería del estudio', ccdPayload: studioGallerySeed, ccdCreatedAt: '2026-11-07T00:00:00Z' };
const getPublic = jest.fn<() => Promise<CmsContentDTO>>();
const create = jest.fn<(input: CmsContentIn) => Promise<CmsContentDTO>>();
const publish = jest.fn<() => Promise<CmsContentDTO>>();
const upload = jest.fn<() => Promise<{ id: string; name: string; publicUrl?: string }>>();
jest.unstable_mockModule('../api/cms', () => ({ Cms: {
  getPublic, create, publish, list: async () => [live], remove: jest.fn(),
} }));
jest.unstable_mockModule('../api/drive', () => ({ uploadToDrive: upload }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: {
  listAuthoredContents: async () => [{ id: contentId, code: 'studio-gallery', contentTypeCode: 'studio-photo-gallery', name: 'Galería del estudio', currentSlug: 'studio-gallery', schema: { required: ['schemaVersion', 'photos'], example: { schemaVersion: 1, photos: [] } }, schemaVersion: 1, publicRoute: '/tdf#estudio', active: true }],
  listItems: async () => ({ items: [{ id: 'es', code: 'es', name: 'Español' }] }),
  listWorkflowStates: async () => [{ id: 'draft', code: 'draft', active: true, sortOrder: 1, name: 'Borrador' }, { id: 'published', code: 'published', active: true, sortOrder: 2, name: 'Publicado' }],
} }));
jest.unstable_mockModule('../components/SessionGate', () => ({ SessionGate: ({ children }: { children: React.ReactNode }) => <>{children}</> }));
const { default: StudioGallery } = await import('./StudioGallery');
const { default: StudioGalleryEditor } = await import('./StudioGalleryEditor');
const { default: CmsAdminPage } = await import('../pages/CmsAdminPage');

function queryView(element: React.ReactNode) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return { ...render(<QueryClientProvider client={client}>{element}</QueryClientProvider>), client };
}
function Editor() {
  const [value, setValue] = useState<StudioGalleryPayload>({ schemaVersion: 1, photos: studioGallerySeed.photos.slice(0, 2) });
  return <><StudioGalleryEditor value={value} onChange={setValue} onBusyChange={() => {}} /><output data-testid="draft">{JSON.stringify(value)}</output></>;
}

beforeEach(() => {
  localStorage.clear(); jest.clearAllMocks();
  getPublic.mockResolvedValue(live);
  create.mockImplementation(async input => ({ ...live, ccdId: 2, ccdVersion: 2, ccdStatus: 'draft', ccdPayload: input.cciPayload }));
});
afterEach(cleanup);

it('renders the published order and visibility, including an intentionally empty gallery', async () => {
  getPublic.mockResolvedValueOnce({ ...live, ccdPayload: { schemaVersion: 1, photos: [studioGallerySeed.photos[1], { ...studioGallerySeed.photos[0], visible: false }] } });
  const view = queryView(<StudioGallery />);
  expect(await screen.findByRole('button', { name: 'Ampliar foto: Sala de control · Google Maps' })).toBeInTheDocument();
  expect(screen.getAllByRole('button', { name: /^Ampliar foto:/ })).toHaveLength(1);
  getPublic.mockResolvedValue({ ...live, ccdVersion: 2, ccdPayload: { schemaVersion: 1, photos: [] } });
  await view.client.invalidateQueries({ queryKey: ['cms-public'] });
  expect(await screen.findByText('Todavía no hay fotos publicadas.')).toBeInTheDocument();
  expect(screen.queryByRole('img')).not.toBeInTheDocument();
});

it('does not resurrect removed bundled photos after a failed API read and allows retry', async () => {
  getPublic.mockRejectedValueOnce(new Error('Network unavailable'));
  // The production hook retries once, then displays the error.
  getPublic.mockRejectedValueOnce(new Error('Network unavailable'));
  queryView(<StudioGallery />);
  expect(await screen.findByRole('alert', {}, { timeout: 4000 })).toHaveTextContent('No se pudo cargar');
  expect(screen.queryByRole('img')).not.toBeInTheDocument();
  getPublic.mockResolvedValue(live);
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  expect(await screen.findByRole('button', { name: 'Ampliar foto: Procesadores y preamplificadores' })).toBeInTheDocument();
});

it('rejects an invalid published manifest without rendering unsafe image URLs', async () => {
  getPublic.mockResolvedValue({ ...live, ccdPayload: { schemaVersion: 1, photos: [{ ...studioGallerySeed.photos[0], imageUrl: 'javascript:alert(1)' }] } });
  queryView(<StudioGallery />);
  expect(await screen.findByRole('alert')).toHaveTextContent('La galería no está disponible');
  expect(screen.queryByRole('img')).not.toBeInTheDocument();
});

it('lets admins reorder, edit, hide, add and confirm removal while retaining editable incomplete fields', () => {
  render(<Editor />);
  fireEvent.click(screen.getByRole('button', { name: 'Bajar foto: Procesadores y preamplificadores' }));
  let first = screen.getByRole('region', { name: 'Foto 1: Sala de control · Google Maps' });
  fireEvent.click(within(first).getByRole('button', { name: 'Editar foto: Sala de control · Google Maps' }));
  fireEvent.change(within(first).getByLabelText('Título de la foto'), { target: { value: '' } });
  fireEvent.change(within(screen.getAllByRole('region')[0]!).getByLabelText('Título de la foto'), { target: { value: 'Control actualizado' } });
  first = screen.getByRole('region', { name: 'Foto 1: Control actualizado' });
  fireEvent.click(within(first).getByLabelText('Visible en la galería al publicar'));
  fireEvent.change(screen.getByLabelText('URL pública de la nueva foto'), { target: { value: 'https://example.com/new.webp' } });
  fireEvent.click(screen.getByRole('button', { name: 'Añadir por URL' }));
  const added = screen.getByRole('region', { name: 'Foto 3: Nueva foto' });
  fireEvent.click(within(added).getByRole('button', { name: 'Quitar foto' }));
  expect(screen.getAllByRole('region')).toHaveLength(3);
  fireEvent.click(within(added).getByRole('button', { name: 'Confirmar quitar' }));
  const draft = parseStudioGallery(JSON.parse(screen.getByTestId('draft').textContent!));
  expect(draft.photos).toHaveLength(2);
  expect(draft.photos[0]).toMatchObject({ id: 'maps-control-room', title: 'Control actualizado', visible: false });
});

it('uses public storage uploads and preserves the draft when an upload fails', async () => {
  upload.mockRejectedValueOnce(new Error('Sin espacio disponible'));
  render(<Editor />);
  const file = new File(['image'], 'session.webp', { type: 'image/webp' });
  fireEvent.change(screen.getByLabelText('Subir foto pública', { selector: 'input' }), { target: { files: [file] } });
  expect(await screen.findByText('Sin espacio disponible')).toBeInTheDocument();
  expect(screen.getAllByRole('region')).toHaveLength(2);
  upload.mockResolvedValueOnce({ id: 'uploaded', name: file.name, publicUrl: 'https://example.com/upload.webp' });
  fireEvent.change(screen.getByLabelText('Subir foto pública', { selector: 'input' }), { target: { files: [file] } });
  expect(await screen.findByRole('region', { name: 'Foto 3: session' })).toBeInTheDocument();
  expect(upload).toHaveBeenLastCalledWith(file, expect.objectContaining({ destination: 'tdf', visibility: 'public', signal: expect.any(AbortSignal) }));
});

it('saves visual edits through the versioned CMS API without publishing the author’s draft', async () => {
  queryView(<MemoryRouter initialEntries={['/configuracion/cms?slug=studio-gallery&locale=es']}><CmsAdminPage /></MemoryRouter>);
  const load = await screen.findByRole('button', { name: /Usar versión en vivo|Traer versión en vivo|Cargar versión en vivo/ });
  fireEvent.click(load);
  const first = await screen.findByRole('region', { name: 'Foto 1: Procesadores y preamplificadores' });
  fireEvent.click(within(first).getByLabelText('Visible en la galería al publicar'));
  fireEvent.click(screen.getByRole('button', { name: 'Bajar foto: Procesadores y preamplificadores' }));
  fireEvent.click(await screen.findByRole('button', { name: 'Guardar borrador' }));
  await waitFor(() => expect(create).toHaveBeenCalledTimes(1));
  const input = create.mock.calls[0]![0];
  expect(input).toMatchObject({ cciContentId: contentId, cciLocale: 'es', cciStatus: 'draft' });
  const saved = parseStudioGallery(input.cciPayload);
  expect(saved.photos[0]!.id).toBe('maps-control-room');
  expect(saved.photos[1]).toMatchObject({ id: 'maps-preamps', visible: false });
  expect(publish).not.toHaveBeenCalled();
});
