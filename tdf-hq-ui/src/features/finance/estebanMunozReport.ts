export type YearMonth = `${number}-${string}`;

export type AccountDirection = 'esteban_owes_tdf' | 'tdf_owes_esteban' | 'settled';

export interface CourseCompensationSource {
  slug: string;
  title: string;
  sourceLabel: string;
  sessionDates: string[];
  hours: number;
}

export interface PromotionShareSource {
  payerName: string;
  concept: string;
  totalPaidCents: number;
  estebanSharePercent: number;
  units: number;
  sourceLabel: string;
}

export interface AccountPosition {
  id: string;
  account: string;
  concept: string;
  direction: AccountDirection;
  amountCents: number;
  status: string;
  detail: string;
}

export const ESTEBAN_MUNOZ_REPORT_SOURCE = {
  personName: 'Esteban Muñoz',
  cutoffDate: '2026-06-23',
  rent: {
    monthlyAmountCents: 25_000,
    lastPaidMonth: '2026-01' as YearMonth,
    throughMonth: '2026-06' as YearMonth,
  },
  lastReceipt: {
    source: 'Comprobante Banco del Austro adjunto',
    date: '2026-01-04',
    originalDateLabel: '04-01-2026',
    receiptNumber: '11591',
    amountCents: 50_000,
    description: 'ABONO TDF',
    originAccount: 'AHO - 071XXX6991',
    destinationAccount: 'AHO - 22XXXXXX00',
    financialEntity: 'Banco Pichincha',
    beneficiary: 'Muñoz Muñoz Esteban Hora',
  },
  coursePayment: {
    hourlyRateCents: 2_500,
    courses: [
      {
        slug: 'produccion-musical-feb-2026',
        title: 'Curso de Producción Musical - Feb 2026 / Mar 2026',
        sourceLabel: 'Sistema TDF: inscripción producción musical Feb 2026 / Mar 2026',
        sessionDates: ['2026-02-28', '2026-03-07', '2026-03-14', '2026-03-21'],
        hours: 16,
      },
      {
        slug: 'produccion-musical-jun-2026',
        title: 'Curso de Producción Musical - Jun 2026',
        sourceLabel: 'Sistema TDF: curso producción musical Jun 2026',
        sessionDates: ['2026-06-06', '2026-06-13', '2026-06-20', '2026-06-27'],
        hours: 16,
      },
    ] satisfies CourseCompensationSource[],
  },
  promotionShare: {
    payerName: 'Paula Roman',
    concept: 'Realización de mastering',
    totalPaidCents: 20_000,
    estebanSharePercent: 40,
    units: 5,
    sourceLabel: 'Pago Paula Roman por realización de mastering',
  } satisfies PromotionShareSource,
} as const;

const MONTH_NAMES = [
  'enero',
  'febrero',
  'marzo',
  'abril',
  'mayo',
  'junio',
  'julio',
  'agosto',
  'septiembre',
  'octubre',
  'noviembre',
  'diciembre',
] as const;

const parseYearMonth = (value: YearMonth) => {
  const match = /^(\d{4})-(\d{2})$/.exec(value);
  if (!match) throw new Error(`Invalid year-month: ${value}`);
  const year = Number.parseInt(match[1]!, 10);
  const month = Number.parseInt(match[2]!, 10);
  if (!Number.isSafeInteger(year) || month < 1 || month > 12) {
    throw new Error(`Invalid year-month: ${value}`);
  }
  return { year, month };
};

const monthIndex = (value: YearMonth) => {
  const { year, month } = parseYearMonth(value);
  return year * 12 + month - 1;
};

const monthFromIndex = (index: number): YearMonth => {
  const year = Math.floor(index / 12);
  const month = (index % 12) + 1;
  return `${year}-${String(month).padStart(2, '0')}`;
};

export const formatMonthLabel = (value: YearMonth) => {
  const { year, month } = parseYearMonth(value);
  return `${MONTH_NAMES[month - 1]} ${year}`;
};

export const formatDateLabel = (isoDate: string) => {
  const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(isoDate);
  if (!match) return isoDate;
  const year = Number.parseInt(match[1]!, 10);
  const month = Number.parseInt(match[2]!, 10);
  const day = Number.parseInt(match[3]!, 10);
  if (!Number.isSafeInteger(year) || month < 1 || month > 12 || day < 1 || day > 31) {
    return isoDate;
  }
  return `${day} de ${MONTH_NAMES[month - 1]} de ${year}`;
};

export const listMonthsAfterThrough = (lastPaidMonth: YearMonth, throughMonth: YearMonth) => {
  const start = monthIndex(lastPaidMonth) + 1;
  const end = monthIndex(throughMonth);
  if (end < start) return [];
  return Array.from({ length: end - start + 1 }, (_, offset) => monthFromIndex(start + offset));
};

const calculatePercentShareCents = (amountCents: number, percent: number) =>
  Math.round((amountCents * percent) / 100);

export const buildEstebanMunozReport = () => {
  const source = ESTEBAN_MUNOZ_REPORT_SOURCE;
  const unpaidRentMonths = listMonthsAfterThrough(source.rent.lastPaidMonth, source.rent.throughMonth);
  const rentDueCents = unpaidRentMonths.length * source.rent.monthlyAmountCents;
  const courseRows = source.coursePayment.courses.map((course) => ({
    ...course,
    subtotalCents: course.hours * source.coursePayment.hourlyRateCents,
  }));
  const coursePayableCents = courseRows.reduce((total, course) => total + course.subtotalCents, 0);
  const promotionShareRow = {
    ...source.promotionShare,
    estebanShareCents: calculatePercentShareCents(
      source.promotionShare.totalPaidCents,
      source.promotionShare.estebanSharePercent,
    ),
  };
  const payableToEstebanCents = coursePayableCents + promotionShareRow.estebanShareCents;
  const netAfterOffsetCents = rentDueCents - payableToEstebanCents;
  const netDirection: AccountDirection =
    netAfterOffsetCents > 0
      ? 'esteban_owes_tdf'
      : netAfterOffsetCents < 0
        ? 'tdf_owes_esteban'
        : 'settled';
  const netStatus =
    netDirection === 'settled'
      ? 'Saldo cero'
      : netDirection === 'esteban_owes_tdf'
        ? 'Saldo neto por cobrar'
        : 'Saldo neto por pagar';

  const accountPositions: AccountPosition[] = [
    {
      id: 'rent',
      account: 'Arriendo mensual',
      concept: 'Renta de espacio TDF',
      direction: 'esteban_owes_tdf',
      amountCents: rentDueCents,
      status: 'Por cobrar',
      detail: `${unpaidRentMonths.length} meses pendientes: ${unpaidRentMonths.map(formatMonthLabel).join(', ')}.`,
    },
    {
      id: 'course-payable',
      account: 'Honorarios por clases',
      concept: 'Cursos de producción musical',
      direction: 'tdf_owes_esteban',
      amountCents: coursePayableCents,
      status: 'Por pagar',
      detail: `${courseRows.length} cursos x 16 horas x $${source.coursePayment.hourlyRateCents / 100} por hora.`,
    },
    {
      id: 'mastering-work',
      account: 'Participación por mastering',
      concept: source.promotionShare.concept,
      direction: 'tdf_owes_esteban',
      amountCents: promotionShareRow.estebanShareCents,
      status: 'Por pagar',
      detail: `${source.promotionShare.payerName} pagó $${source.promotionShare.totalPaidCents / 100}; ${source.promotionShare.estebanSharePercent}% corresponde a Esteban.`,
    },
    {
      id: 'last-receipt',
      account: 'Último comprobante',
      concept: source.lastReceipt.description,
      direction: 'settled',
      amountCents: source.lastReceipt.amountCents,
      status: `Aplicado hasta ${formatMonthLabel(source.rent.lastPaidMonth)}`,
      detail: `Comprobante ${source.lastReceipt.receiptNumber}, ${source.lastReceipt.originalDateLabel}.`,
    },
    {
      id: 'net',
      account: 'Saldo neto',
      concept: 'Compensación arriendo - cuentas a favor de Esteban',
      direction: netDirection,
      amountCents: Math.abs(netAfterOffsetCents),
      status: netStatus,
      detail: 'Escenario contable si TDF y Esteban aprueban compensar cuentas.',
    },
  ];

  return {
    source,
    unpaidRentMonths,
    rentDueCents,
    courseRows,
    coursePayableCents,
    promotionShareRow,
    payableToEstebanCents,
    netAfterOffsetCents,
    netDirection,
    accountPositions,
  };
};

export type EstebanMunozReport = ReturnType<typeof buildEstebanMunozReport>;

export const ESTEBAN_MUNOZ_REPORT_PDF_FILENAME = 'reporte-esteban-munoz.pdf';

const formatMoneyPdf = (cents: number) =>
  `$${(cents / 100).toLocaleString('en-US', {
    minimumFractionDigits: 2,
    maximumFractionDigits: 2,
  })}`;

const directionLabel = (direction: AccountDirection) => {
  if (direction === 'esteban_owes_tdf') return 'Esteban debe a TDF';
  if (direction === 'tdf_owes_esteban') return 'TDF debe a Esteban';
  return 'Registrado';
};

const normalizePdfText = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[^\x20-\x7E]/g, ' ');

const escapePdfString = (value: string) =>
  normalizePdfText(value).replace(/\\/g, '\\\\').replace(/\(/g, '\\(').replace(/\)/g, '\\)');

const wrapPdfText = (text: string, maxChars: number) => {
  const words = text.split(/\s+/).filter(Boolean);
  const lines: string[] = [];
  let current = '';

  words.forEach((word) => {
    const next = current ? `${current} ${word}` : word;
    if (next.length > maxChars && current) {
      lines.push(current);
      current = word;
    } else {
      current = next;
    }
  });

  if (current) lines.push(current);
  return lines.length > 0 ? lines : [''];
};

const buildPdfContentStream = (report: EstebanMunozReport) => {
  const commands: string[] = [];
  let y = 744;

  const writeLine = (text: string, options: { size?: number; bold?: boolean; indent?: number } = {}) => {
    const size = options.size ?? 10;
    const x = 48 + (options.indent ?? 0);
    const font = options.bold ? 'F2' : 'F1';
    commands.push(`BT /${font} ${size} Tf ${x} ${y} Td (${escapePdfString(text)}) Tj ET`);
    y -= size + 5;
  };

  const writeWrapped = (
    text: string,
    options: { size?: number; bold?: boolean; indent?: number; maxChars?: number } = {},
  ) => {
    wrapPdfText(text, options.maxChars ?? 92).forEach((line) => {
      writeLine(line, options);
    });
  };

  const writeSpace = (height = 8) => {
    y -= height;
  };

  writeLine('Reporte Esteban Munoz', { size: 18, bold: true });
  writeLine(`Estado de cuentas con TDF al ${formatDateLabel(report.source.cutoffDate)}.`, { size: 10 });
  writeSpace();

  writeLine('Resumen', { size: 12, bold: true });
  writeLine(`Arriendo pendiente: ${formatMoneyPdf(report.rentDueCents)}`);
  writeLine(`Honorarios por clases: ${formatMoneyPdf(report.coursePayableCents)}`);
  writeLine(`Participacion realizacion de mastering: ${formatMoneyPdf(report.promotionShareRow.estebanShareCents)}`);
  writeLine(`Total a favor de Esteban: ${formatMoneyPdf(report.payableToEstebanCents)}`);
  writeLine(`${directionLabel(report.netDirection)}: ${formatMoneyPdf(Math.abs(report.netAfterOffsetCents))}`);
  writeSpace();

  writeLine('Arriendo', { size: 12, bold: true });
  writeWrapped(
    `${report.unpaidRentMonths.length} meses pendientes (${report.unpaidRentMonths.map(formatMonthLabel).join(', ')}), a ${formatMoneyPdf(report.source.rent.monthlyAmountCents)} por mes.`,
  );
  writeSpace();

  writeLine('Cursos de produccion', { size: 12, bold: true });
  report.courseRows.forEach((course) => {
    writeWrapped(
      `${course.title}: ${course.hours} horas x ${formatMoneyPdf(report.source.coursePayment.hourlyRateCents)} = ${formatMoneyPdf(course.subtotalCents)}. Fechas: ${course.sessionDates.map(formatDateLabel).join(', ')}.`,
      { indent: 12, maxChars: 88 },
    );
  });
  writeSpace();

  writeLine('Realizacion de mastering', { size: 12, bold: true });
  writeWrapped(
    `${report.promotionShareRow.payerName} pago ${formatMoneyPdf(report.promotionShareRow.totalPaidCents)} por ${report.promotionShareRow.concept.toLowerCase()}; ${report.promotionShareRow.estebanSharePercent}% corresponde a Esteban = ${formatMoneyPdf(report.promotionShareRow.estebanShareCents)}.`,
  );
  writeSpace();

  writeLine('Comprobante base', { size: 12, bold: true });
  writeWrapped(
    `Comprobante ${report.source.lastReceipt.receiptNumber}, ${report.source.lastReceipt.originalDateLabel}, ${report.source.lastReceipt.description}, valor ${formatMoneyPdf(report.source.lastReceipt.amountCents)}. Aplicado hasta ${formatMonthLabel(report.source.rent.lastPaidMonth)}.`,
  );
  writeSpace();

  writeLine('Cuentas mantenidas con TDF', { size: 12, bold: true });
  report.accountPositions.forEach((position) => {
    writeWrapped(
      `${position.account}: ${position.concept}. ${position.status}. ${directionLabel(position.direction)}. Monto ${formatMoneyPdf(position.amountCents)}. ${position.detail}`,
      { indent: 12, maxChars: 88 },
    );
  });

  return commands.join('\n');
};

const buildPdfDocument = (contentStream: string) => {
  const objects = [
    '<< /Type /Catalog /Pages 2 0 R >>',
    '<< /Type /Pages /Kids [3 0 R] /Count 1 >>',
    '<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 4 0 R /F2 5 0 R >> >> /Contents 6 0 R >>',
    '<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>',
    '<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica-Bold >>',
    `<< /Length ${contentStream.length} >>\nstream\n${contentStream}\nendstream`,
  ];

  let pdf = '%PDF-1.4\n';
  const offsets: number[] = [];
  objects.forEach((object, index) => {
    offsets.push(pdf.length);
    pdf += `${index + 1} 0 obj\n${object}\nendobj\n`;
  });

  const xrefOffset = pdf.length;
  pdf += `xref\n0 ${objects.length + 1}\n0000000000 65535 f \n`;
  offsets.forEach((offset) => {
    pdf += `${String(offset).padStart(10, '0')} 00000 n \n`;
  });
  pdf += `trailer\n<< /Size ${objects.length + 1} /Root 1 0 R >>\nstartxref\n${xrefOffset}\n%%EOF\n`;
  return pdf;
};

export const buildEstebanMunozReportPdfSource = (report: EstebanMunozReport = buildEstebanMunozReport()) =>
  buildPdfDocument(buildPdfContentStream(report));

export const buildEstebanMunozReportPdfBlob = (report: EstebanMunozReport = buildEstebanMunozReport()) =>
  new Blob([buildEstebanMunozReportPdfSource(report)], { type: 'application/pdf' });
