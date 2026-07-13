import {
  buildWorkAccountReport,
  buildWorkAccountReportPdfSource,
  formatCurrency,
  type WorkAccountReportSource,
} from './davidCelayaReport';

describe('work account report builder', () => {
  const source: WorkAccountReportSource = {
    personName: 'Cliente Prueba',
    reportTitle: 'Reporte de cuenta',
    periodLabel: 'julio 2026',
    hourlyRateCents: 3_000,
    notes: 'Generado desde el sistema.',
    workBlocks: [
      {
        id: 'block-a',
        date: '2026-07-12',
        startTime: '10:00:00',
        endTime: '12:20:00',
        startPhotoLabel: 'Inicio',
        endPhotoLabel: 'Fin',
        description: 'Bloque de prueba.',
        discountHours: 1,
        discountReason: 'Descuento de prueba.',
      },
    ],
  };

  it('builds a custom report from system input', () => {
    const report = buildWorkAccountReport(source, { asOfDate: '2026-07-12' });

    expect(report.source.personName).toBe('Cliente Prueba');
    expect(report.totalBillableHours).toBe(1);
    expect(report.totalDiscountHours).toBe(1);
    expect(report.totalBillableCents).toBe(3_000);
    expect(report.workBlocks[0]?.billableLabel).toBe(formatCurrency(3_000));
  });

  it('generates a PDF for a custom report', () => {
    const report = buildWorkAccountReport(source, { asOfDate: '2026-07-12' });
    const pdf = buildWorkAccountReportPdfSource(report);

    expect(pdf).toContain('Cliente Prueba');
    expect(pdf).toContain('Descuento acumulado');
    expect(pdf).toContain('$30,00');
  });
});
