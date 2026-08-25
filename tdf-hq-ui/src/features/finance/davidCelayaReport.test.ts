import {
  DAVID_CELAYA_REPORT_SOURCE,
  buildDavidCelayaReport,
  buildDavidCelayaReportPdfBlob,
  buildDavidCelayaReportPdfSource,
  formatDateLabel,
  formatDuration,
} from './davidCelayaReport';

describe('David Celaya account report', () => {
  it('builds two work blocks from the provided camera timestamps', () => {
    const report = buildDavidCelayaReport();

    expect(report.source.personName).toBe('David Celaya');
    expect(report.workBlocks).toHaveLength(2);
    expect(report.workBlocks.map((block) => block.durationSeconds)).toEqual([22_120, 22_384]);
    expect(report.workBlocks.map((block) => block.billableHours)).toEqual([6, 6]);
    expect(report.workBlocks.map((block) => block.discountHours)).toEqual([1, 2]);
    expect(report.workBlocks.map((block) => block.netBillableHours)).toEqual([5, 4]);
    expect(report.workBlocks.map((block) => block.billableCents)).toEqual([12_500, 10_000]);
    expect(report.totalDurationSeconds).toBe(44_504);
    expect(report.totalBillableHours).toBe(9);
    expect(report.totalBillableCents).toBe(22_500);
    expect(report.totalDiscountHours).toBe(3);
    expect(report.totalDiscountCents).toBe(7_500);
    expect(report.averageDurationSeconds).toBe(22_252);
    expect(report.evidenceCount).toBe(4);
    expect(report.firstStart?.getFullYear()).toBe(2026);
    expect(report.firstStart?.getMonth()).toBe(6);
    expect(report.firstStart?.getDate()).toBe(9);
    expect(report.firstStart?.getHours()).toBe(14);
    expect(report.firstStart?.getMinutes()).toBe(35);
    expect(report.lastEnd?.getFullYear()).toBe(2026);
    expect(report.lastEnd?.getMonth()).toBe(6);
    expect(report.lastEnd?.getDate()).toBe(10);
    expect(report.lastEnd?.getHours()).toBe(23);
    expect(report.lastEnd?.getMinutes()).toBe(50);
  });

  it('keeps the source dates and helper formatting stable', () => {
    expect(DAVID_CELAYA_REPORT_SOURCE.workBlocks[0]?.date).toBe('2026-07-09');
    expect(DAVID_CELAYA_REPORT_SOURCE.workBlocks[1]?.endTime).toBe('23:50:32');
    expect(formatDateLabel('2026-07-10')).toBe('10 de julio de 2026');
    expect(formatDuration(44_504)).toBe('12h 21m 44s');
  });

  it('generates a downloadable PDF document with the account summary', () => {
    const report = buildDavidCelayaReport();
    const pdf = buildDavidCelayaReportPdfBlob(report);
    const source = buildDavidCelayaReportPdfSource(report);

    expect(pdf.type).toBe('application/pdf');
    expect(source.startsWith('%PDF-1.4')).toBe(true);
    expect(source).toContain('Reporte de cuenta');
    expect(source).toContain('David Celaya');
    expect(source).toContain('Bloques de trabajo');
    expect(source).toContain('$225,00');
    expect(source).toContain('$75,00');
    expect(source).toContain('descuento explicito');
  });
});
