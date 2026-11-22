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
    customFields: [
      { id: 'field-1', label: 'Propósito', value: 'Ensayo y acompañamiento.' },
    ],
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
    expect(report.customFields).toHaveLength(1);
    expect(report.totalBillableHours).toBe(1);
    expect(report.totalDiscountHours).toBe(1);
    expect(report.totalBillableCents).toBe(3_000);
    expect(report.workBlocks[0]?.billableLabel).toBe(formatCurrency(3_000));
  });

  it('generates a PDF for a custom report', () => {
    const report = buildWorkAccountReport(source, { asOfDate: '2026-07-12' });
    const pdf = buildWorkAccountReportPdfSource(report);

    expect(pdf).toContain('Cliente Prueba');
    expect(pdf).toContain('Proposito');
    expect(pdf).toContain('Descuento acumulado');
    expect(pdf).toContain('$30,00');
  });
});


describe('exact report currency formatting', () => {
  it.each([29, 110, 2550, 0, -1, -110])('preserves the existing locale presentation for %s cents', cents => {
    const legacy = new Intl.NumberFormat('es-EC', { style: 'currency', currency: 'USD',
      minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(cents / 100).replace('USD', '$');
    expect(formatCurrency(cents)).toBe(legacy);
  });
  it.each([Number.MAX_SAFE_INTEGER, -Number.MAX_SAFE_INTEGER])('prints the last cent at the safe integer boundary %s', cents => {
    expect(formatCurrency(cents).replace(/[^\d,-]/g, '')).toBe(`${cents < 0 ? '-' : ''}90071992547409,91`);
  });
});
