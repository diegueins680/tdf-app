import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { vi } from 'vitest';
import MetadataManager from '../Metadata/Manager';

const mockApi = vi.fn();

vi.mock('../../api/client', () => ({
  api: (path: string) => mockApi(path),
}));

vi.mock('@mui/x-data-grid', () => ({
  DataGrid: ({ rows, loading }: { rows: unknown[]; loading?: boolean }) => (
    <div data-testid="metadata-grid">
      {loading ? 'Cargando metadatos…' : `Grid rows: ${rows.length}`}
    </div>
  ),
}));

function buildMetadataRow(overrides: Partial<Record<string, unknown>> = {}) {
  return {
    catalog_id: 'CAT-001',
    artist_name: 'Ana Tijoux',
    project_title: 'Vida',
    session_type: 'Mix',
    bpm: 96,
    key: 'Am',
    genre: 'Hip-Hop',
    ...overrides,
  };
}

describe('MetadataManager', () => {
  let createObjectUrlSpy: ReturnType<typeof vi.spyOn>;
  let revokeObjectUrlSpy: ReturnType<typeof vi.spyOn>;
  const clickSpy = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => undefined);

  beforeAll(() => {
    if (!('createObjectURL' in URL)) {
      Object.defineProperty(URL, 'createObjectURL', {
        writable: true,
        value: () => 'blob:metadata-export',
      });
    }
    if (!('revokeObjectURL' in URL)) {
      Object.defineProperty(URL, 'revokeObjectURL', {
        writable: true,
        value: () => undefined,
      });
    }
  });

  beforeEach(() => {
    vi.clearAllMocks();
    createObjectUrlSpy = vi.spyOn(URL, 'createObjectURL');
    revokeObjectUrlSpy = vi.spyOn(URL, 'revokeObjectURL');
    createObjectUrlSpy.mockReturnValue('blob:metadata-export');
    revokeObjectUrlSpy.mockImplementation(() => undefined);
  });

  afterEach(() => {
    createObjectUrlSpy.mockRestore();
    revokeObjectUrlSpy.mockRestore();
  });

  afterAll(() => {
    clickSpy.mockRestore();
  });

  it('hides export clutter until metadata exists and explains the first import step', async () => {
    mockApi.mockResolvedValue([]);

    render(<MetadataManager />);

    expect(await screen.findByText('Metadata Manager')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Todavía no hay metadatos cargados.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Usa Importar metadatos para traer tu primer catálogo\. Cuando exista el primero, aquí podrás revisarlo y exportarlo en CSV o JSON desde un solo lugar\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByText(
          /Empieza con Importar metadatos\. La exportación aparecerá cuando exista el primer registro\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.getByRole('button', { name: /Importar metadatos/i })).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /^Exportar$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/Exportar CSV/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Exportar JSON/i)).not.toBeInTheDocument();
    expect(screen.queryByTestId('metadata-grid')).not.toBeInTheDocument();
  });

  it('replaces the one-row grid with a compact first-record summary until comparison matters', async () => {
    mockApi.mockResolvedValue([buildMetadataRow()]);

    render(<MetadataManager />);

    expect(await screen.findByText('Metadata Manager')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primer registro cargado.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Revísalo aquí; cuando exista el segundo, volverá la tabla para comparar catálogo, tempo y tonalidad\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText(/Catalog ID:\s*CAT-001/i)).toBeInTheDocument();
      expect(screen.getByText(/Artist:\s*Ana Tijoux/i)).toBeInTheDocument();
      expect(screen.getByText(/Project:\s*Vida/i)).toBeInTheDocument();
      expect(screen.getByText(/Type:\s*Mix/i)).toBeInTheDocument();
      expect(screen.getByText(/BPM \/ Key:\s*96 \/ Am/i)).toBeInTheDocument();
      expect(screen.getByText(/Genre:\s*Hip-Hop/i)).toBeInTheDocument();
      expect(
        screen.getByText(
          /Ya puedes exportar el catálogo actual en CSV o JSON\. La tabla aparecerá cuando exista un segundo registro para comparar\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.getByRole('button', { name: /Importar metadatos/i })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /^Exportar$/i })).toBeInTheDocument();
    expect(screen.queryByTestId('metadata-grid')).not.toBeInTheDocument();
  });

  it('uses one export entry point and keeps file formats inside the menu', async () => {
    const user = userEvent.setup();
    mockApi.mockResolvedValue([
      buildMetadataRow(),
      buildMetadataRow({
        catalog_id: 'CAT-002',
        artist_name: 'Nathy Peluso',
        project_title: 'Calambre',
        session_type: 'Master',
        bpm: 120,
        key: 'Dm',
        genre: 'Pop',
      }),
    ]);

    render(<MetadataManager />);

    expect(await screen.findByText('Metadata Manager')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByTestId('metadata-grid')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /^Exportar$/i })).toBeInTheDocument();
      expect(
        screen.getByText(
          /Exporta el catálogo actual en CSV o JSON desde un solo menú para evitar acciones duplicadas\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /Exportar CSV/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Exportar JSON/i })).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Exportar$/i }));

    expect(await screen.findByRole('menuitem', { name: /Exportar CSV/i })).toBeInTheDocument();
    expect(screen.getByRole('menuitem', { name: /Exportar JSON/i })).toBeInTheDocument();

    await user.click(screen.getByRole('menuitem', { name: /Exportar CSV/i }));

    expect(createObjectUrlSpy).toHaveBeenCalledTimes(1);
    expect(clickSpy).toHaveBeenCalledTimes(1);
    expect(revokeObjectUrlSpy).toHaveBeenCalledTimes(1);
    expect(screen.queryByRole('menuitem', { name: /Exportar CSV/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Exportar JSON/i })).not.toBeInTheDocument();
  });
});
