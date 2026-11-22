import { jest } from '@jest/globals';
import { fireEvent, render, screen, waitFor, cleanup } from '@testing-library/react';
import '../i18n';

const finance = await import('../features/finance/davidCelayaReport');
const pdf = jest.fn(finance.buildWorkAccountReportPdfBlob);
jest.unstable_mockModule('../features/finance/davidCelayaReport', () => ({ ...finance, buildWorkAccountReportPdfBlob: pdf }));
const { default: Builder } = await import('./WorkAccountReportBuilderPage');
const storageKey = 'tdf-work-account-report-builder-templates';
let serial = 0;
beforeEach(() => {
  localStorage.clear(); pdf.mockClear(); serial = 0;
  URL.createObjectURL = jest.fn(() => `blob:synthetic-${++serial}`);
  URL.revokeObjectURL = jest.fn();
});
afterEach(cleanup);
const rate = () => screen.getByRole('textbox', { name: 'Tarifa USD/h' });
const saved = () => JSON.parse(localStorage.getItem(storageKey) ?? '[]') as { source: { hourlyRateCents: number } }[];

it('saves decimal USD as exact cents and restores legacy cents without rounding', async () => {
  const view = render(<Builder />);
  fireEvent.change(screen.getByRole('textbox', { name: 'Nombre de la plantilla' }), { target: { value: 'Plantilla sintética' } });
  fireEvent.change(rate(), { target: { value: '0,29' } });
  await waitFor(() => expect(pdf.mock.calls.at(-1)?.[0].source.hourlyRateCents).toBe(29));
  fireEvent.click(screen.getByRole('button', { name: 'Guardar plantilla', exact: true }));
  expect(saved()[0].source.hourlyRateCents).toBe(29);
  view.unmount(); render(<Builder />);
  fireEvent.click(screen.getByRole('button', { name: 'Cargar', exact: true }));
  expect((rate() as HTMLInputElement).value).toBe('0.29');
  fireEvent.change(rate(), { target: { value: '90071992547409.91' } });
  fireEvent.click(screen.getByRole('button', { name: 'Guardar plantilla', exact: true }));
  expect(saved()[0].source.hourlyRateCents).toBe(Number.MAX_SAFE_INTEGER);
  fireEvent.change(rate(), { target: { value: '1' } });
  fireEvent.click(screen.getByRole('button', { name: 'Reaplicar', exact: true }));
  expect((rate() as HTMLInputElement).value).toBe('90071992547409.91');
});

it.each(['25abc', '1e3', 'Infinity', '25.505', '-1', '', '90071992547409.92'])(
  'keeps invalid raw rate %s visible, labels the retained preview and blocks saving/export', async (raw) => {
    render(<Builder />);
    fireEvent.change(rate(), { target: { value: '40,99' } });
    await waitFor(() => expect(pdf.mock.calls.at(-1)?.[0].source.hourlyRateCents).toBe(4099));
    const count = pdf.mock.calls.length;
    fireEvent.change(rate(), { target: { value: raw } });
    expect((rate() as HTMLInputElement).value).toBe(raw);
    expect(rate().getAttribute('aria-invalid')).toBe('true');
    expect(document.getElementById(rate().getAttribute('aria-describedby')!)?.textContent).toBeTruthy();
    expect(screen.getByText(/La vista previa conserva la última tarifa válida/)).toBeTruthy();
    const download = document.querySelector('a[download="reporte-cuenta.pdf"]')!;
    expect(download.getAttribute('href')).toBeNull();
    expect((screen.getByRole('button', { name: 'Guardar plantilla', exact: true })).disabled).toBe(true);
    expect(pdf.mock.calls.length).toBe(count); expect(saved()).toEqual([]);
    fireEvent.change(rate(), { target: { value: '0' } });
    await waitFor(() => expect(pdf.mock.calls.at(-1)?.[0].source.hourlyRateCents).toBe(0));
    expect(rate().getAttribute('aria-invalid')).toBe('false');
  },
);

it('prevents exporting unsafe derived totals while preserving the entered rate and existing calculation policy', () => {
  render(<Builder />);
  fireEvent.click(screen.getByRole('button', { name: 'Agregar bloque', exact: true }));
  fireEvent.change(screen.getByLabelText('Fin'), { target: { value: '11:00:00' } });
  const count = pdf.mock.calls.length;
  fireEvent.change(rate(), { target: { value: '90071992547409.91' } });
  expect(screen.getByText(/El total no conserva centavos exactos/)).toBeTruthy();
  expect((rate() as HTMLInputElement).value).toBe('90071992547409.91');
  expect((screen.getByRole('button', { name: 'Guardar plantilla', exact: true })).disabled).toBe(true);
  expect(pdf.mock.calls.length).toBe(count);
});
