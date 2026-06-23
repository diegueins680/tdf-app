import {
  ESTEBAN_MUNOZ_REPORT_SOURCE,
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

  it('calculates two production courses at 16 hours each and 30 dollars per hour', () => {
    const report = buildEstebanMunozReport();

    expect(report.courseRows).toHaveLength(2);
    expect(report.courseRows.map((course) => course.sessionDates)).toEqual([
      ['2026-02-28', '2026-03-07', '2026-03-14', '2026-03-21'],
      ['2026-06-06', '2026-06-13', '2026-06-20', '2026-06-27'],
    ]);
    expect(report.courseRows.map((course) => course.subtotalCents)).toEqual([48_000, 48_000]);
    expect(report.coursePayableCents).toBe(96_000);
  });

  it('shows the compensated net as an amount owed from Esteban to TDF', () => {
    const report = buildEstebanMunozReport();

    expect(report.netAfterOffsetCents).toBe(29_000);
    expect(report.netDirection).toBe('esteban_owes_tdf');
    expect(report.accountPositions.find((item) => item.id === 'net')).toMatchObject({
      amountCents: 29_000,
      status: 'Saldo neto por cobrar',
    });
  });

  it('keeps the receipt policy explicit', () => {
    expect(ESTEBAN_MUNOZ_REPORT_SOURCE.lastReceipt.originalDateLabel).toBe('04-01-2026');
    expect(formatMonthLabel(ESTEBAN_MUNOZ_REPORT_SOURCE.rent.lastPaidMonth)).toBe('enero 2026');
    expect(listMonthsAfterThrough('2026-06', '2026-01')).toEqual([]);
  });
});
