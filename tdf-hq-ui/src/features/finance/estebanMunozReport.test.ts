import {
  ESTEBAN_MUNOZ_REPORT_SOURCE,
  buildEstebanMunozReportPdfBlob,
  buildEstebanMunozReportPdfSource,
  buildEstebanMunozReport,
  formatMonthLabel,
  listMonthsAfterThrough,
} from './estebanMunozReport';

const buildJuneReport = () => buildEstebanMunozReport({ asOfDate: '2026-06-23' });
const buildJulyReport = () => buildEstebanMunozReport({ asOfDate: '2026-07-10' });

describe('Esteban Muñoz account report', () => {
  it('calculates corrected 200 dollar rent from January through June 2026', () => {
    const report = buildJuneReport();

    expect(report.unpaidRentMonths).toEqual([
      '2026-01',
      '2026-02',
      '2026-03',
      '2026-04',
      '2026-05',
      '2026-06',
    ]);
    expect(report.source.rent.monthlyAmountCents).toBe(20_000);
    expect(report.rentDueCents).toBe(120_000);
  });

  it('advances the rent cutoff from the report date each month', () => {
    const report = buildJulyReport();

    expect(report.source.cutoffDate).toBe('2026-07-10');
    expect(report.source.rent.throughMonth).toBe('2026-07');
    expect(report.unpaidRentMonths).toEqual([
      '2026-01',
      '2026-02',
      '2026-03',
      '2026-04',
      '2026-05',
      '2026-06',
      '2026-07',
    ]);
    expect(report.rentDueCents).toBe(140_000);
  });

  it('calculates two production courses at 16 hours each and 25 dollars per hour', () => {
    const report = buildJulyReport();

    expect(report.courseRows).toHaveLength(2);
    expect(report.courseRows.map((course) => course.sessionDates)).toEqual([
      ['2026-02-28', '2026-03-07', '2026-03-14', '2026-03-21'],
      ['2026-06-06', '2026-06-13', '2026-06-20', '2026-06-27'],
    ]);
    expect(report.courseRows.map((course) => course.subtotalCents)).toEqual([40_000, 40_000]);
    expect(report.coursePayableCents).toBe(80_000);
  });

  it('includes Paula Roman mastering work with Esteban receiving 40 percent', () => {
    const report = buildJulyReport();

    expect(report.promotionShareRow).toMatchObject({
      payerName: 'Paula Roman',
      concept: 'Realización de mastering',
      totalPaidCents: 20_000,
      estebanSharePercent: 40,
      units: 5,
      estebanShareCents: 8_000,
    });
    expect(report.accountPositions.find((item) => item.id === 'mastering-work')).toMatchObject({
      direction: 'tdf_owes_esteban',
      amountCents: 8_000,
      status: 'Por pagar',
    });
  });

  it('includes the previous 500 dollar payable TDF owed to Esteban', () => {
    const report = buildJulyReport();

    expect(report.previousPayableCents).toBe(50_000);
    expect(report.payableToEstebanCents).toBe(138_000);
    expect(report.accountPositions.find((item) => item.id === 'previous-tdf-payable')).toMatchObject({
      account: 'Saldo anterior a favor',
      direction: 'tdf_owes_esteban',
      amountCents: 50_000,
      status: 'Por pagar',
    });
  });

  it('shows the compensated net as an amount owed from Esteban to TDF', () => {
    const report = buildJulyReport();

    expect(report.netAfterOffsetCents).toBe(2_000);
    expect(report.netDirection).toBe('esteban_owes_tdf');
    expect(report.accountPositions.find((item) => item.id === 'net')).toMatchObject({
      amountCents: 2_000,
      status: 'Saldo neto por cobrar',
    });
  });

  it('keeps the receipt policy explicit', () => {
    expect(ESTEBAN_MUNOZ_REPORT_SOURCE.lastReceipt.originalDateLabel).toBe('04-01-2026');
    expect(formatMonthLabel(ESTEBAN_MUNOZ_REPORT_SOURCE.rent.lastPaidMonth)).toBe('diciembre 2025');
    expect(listMonthsAfterThrough('2026-06', '2026-01')).toEqual([]);
  });

  it('generates a downloadable PDF document with report totals', () => {
    const report = buildJulyReport();
    const pdf = buildEstebanMunozReportPdfBlob(report);
    const source = buildEstebanMunozReportPdfSource(report);

    expect(pdf.type).toBe('application/pdf');
    expect(source.startsWith('%PDF-1.4')).toBe(true);
    expect(source).toContain('Reporte Esteban Munoz');
    expect(source).toContain('Resumen ejecutivo');
    expect(source).toContain('Participacion por realizacion de mastering');
    expect(source).toContain('Paula Roman');
    expect(source).toContain('$200.00');
    expect(source).toContain('$1,400.00');
    expect(source).toContain('$80.00');
    expect(source).toContain('Saldo anterior');
    expect(source).toContain('$500.00');
    expect(source).toContain('Esteban debe a TDF: $20.00');
  });
});
