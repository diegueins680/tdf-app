import {
  ESTEBAN_MUNOZ_REPORT_SOURCE,
  buildEstebanMunozReportPdfBlob,
  buildEstebanMunozReportPdfSource,
  buildEstebanMunozReport,
  formatMonthLabel,
  listMonthsAfterThrough,
} from './estebanMunozReport';

describe('Esteban Muñoz account report', () => {
  it('calculates rent due from the month after the last paid receipt through June 2026', () => {
    const report = buildEstebanMunozReport();

    expect(report.unpaidRentMonths).toEqual([
      '2026-02',
      '2026-03',
      '2026-04',
      '2026-05',
      '2026-06',
    ]);
    expect(report.rentDueCents).toBe(125_000);
  });

  it('calculates two production courses at 16 hours each and 25 dollars per hour', () => {
    const report = buildEstebanMunozReport();

    expect(report.courseRows).toHaveLength(2);
    expect(report.courseRows.map((course) => course.sessionDates)).toEqual([
      ['2026-02-28', '2026-03-07', '2026-03-14', '2026-03-21'],
      ['2026-06-06', '2026-06-13', '2026-06-20', '2026-06-27'],
    ]);
    expect(report.courseRows.map((course) => course.subtotalCents)).toEqual([40_000, 40_000]);
    expect(report.coursePayableCents).toBe(80_000);
  });

  it('includes Paula Roman masters promotion with Esteban receiving 40 percent', () => {
    const report = buildEstebanMunozReport();

    expect(report.promotionShareRow).toMatchObject({
      payerName: 'Paula Roman',
      concept: 'Promoción de 5 masters',
      totalPaidCents: 20_000,
      estebanSharePercent: 40,
      units: 5,
      estebanShareCents: 8_000,
    });
    expect(report.payableToEstebanCents).toBe(88_000);
    expect(report.accountPositions.find((item) => item.id === 'masters-promotion')).toMatchObject({
      direction: 'tdf_owes_esteban',
      amountCents: 8_000,
      status: 'Por pagar',
    });
  });

  it('shows the compensated net as an amount owed from Esteban to TDF', () => {
    const report = buildEstebanMunozReport();

    expect(report.netAfterOffsetCents).toBe(37_000);
    expect(report.netDirection).toBe('esteban_owes_tdf');
    expect(report.accountPositions.find((item) => item.id === 'net')).toMatchObject({
      amountCents: 37_000,
      status: 'Saldo neto por cobrar',
    });
  });

  it('keeps the receipt policy explicit', () => {
    expect(ESTEBAN_MUNOZ_REPORT_SOURCE.lastReceipt.originalDateLabel).toBe('04-01-2026');
    expect(formatMonthLabel(ESTEBAN_MUNOZ_REPORT_SOURCE.rent.lastPaidMonth)).toBe('enero 2026');
    expect(listMonthsAfterThrough('2026-06', '2026-01')).toEqual([]);
  });

  it('generates a downloadable PDF document with report totals', () => {
    const pdf = buildEstebanMunozReportPdfBlob(buildEstebanMunozReport());
    const source = buildEstebanMunozReportPdfSource(buildEstebanMunozReport());

    expect(pdf.type).toBe('application/pdf');
    expect(source.startsWith('%PDF-1.4')).toBe(true);
    expect(source).toContain('Reporte Esteban Munoz');
    expect(source).toContain('Paula Roman pago $200.00');
    expect(source).toContain('Participacion promocion masters: $80.00');
    expect(source).toContain('Esteban debe a TDF: $370.00');
  });
});
