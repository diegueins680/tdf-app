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

const directionAmountLabel = (direction: AccountDirection, amountCents: number) =>
  `${directionLabel(direction)}: ${formatMoneyPdf(amountCents)}`;

const normalizePdfText = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[^\x20-\x7E]/g, ' ');

const escapePdfString = (value: string) =>
  normalizePdfText(value).replace(/\\/g, '\\\\').replace(/\(/g, '\\(').replace(/\)/g, '\\)');

const escapePdfStringForWidth = (value: string) => normalizePdfText(value);

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

type PdfAlign = 'left' | 'right' | 'center';

interface PdfColumn {
  label: string;
  width: number;
  align?: PdfAlign;
}

interface PdfCell {
  text: string;
  bold?: boolean;
}

type PdfRow = Array<string | PdfCell>;

const PDF = {
  pageWidth: 612,
  pageHeight: 792,
  marginX: 42,
  marginBottom: 46,
  contentWidth: 528,
  headerTop: 748,
  colors: {
    ink: [0.12, 0.13, 0.16],
    muted: [0.42, 0.45, 0.5],
    line: [0.82, 0.84, 0.88],
    softLine: [0.9, 0.91, 0.94],
    panel: [0.96, 0.97, 0.99],
    dark: [0.08, 0.09, 0.12],
    red: [0.82, 0.18, 0.16],
    green: [0.12, 0.56, 0.27],
    purple: [0.42, 0.25, 0.9],
    white: [1, 1, 1],
  },
} as const;

const color = (rgb: readonly number[]) => rgb.join(' ');

const textWidthEstimate = (value: string, fontSize: number, bold = false) =>
  escapePdfStringForWidth(value).length * fontSize * (bold ? 0.55 : 0.5);

const buildPdfContentStream = (report: EstebanMunozReport) => {
  const pages: string[][] = [];
  let commands: string[] = [];
  let y: number = PDF.headerTop;
  let pageNumber = 0;

  const push = (command: string) => commands.push(command);

  const newPage = () => {
    if (commands.length > 0) {
      pages.push(commands);
    }
    commands = [];
    pageNumber += 1;
    y = PDF.headerTop;
    push(`${color(PDF.colors.dark)} rg 0 742 612 50 re f`);
    push(`${color(PDF.colors.white)} rg`);
    push(`BT /F2 15 Tf 42 765 Td (TDF RECORDS) Tj ET`);
    push(`BT /F1 8 Tf 42 752 Td (Estado de cuenta / pagina ${pageNumber}) Tj ET`);
    y = 720;
  };

  const ensureSpace = (height: number) => {
    if (y - height < PDF.marginBottom) newPage();
  };

  const rect = (
    x: number,
    top: number,
    width: number,
    height: number,
    options: { fill?: readonly number[]; stroke?: readonly number[] } = {},
  ) => {
    if (options.fill) {
      push(`${color(options.fill)} rg ${x} ${top - height} ${width} ${height} re f`);
    }
    if (options.stroke) {
      push(`${color(options.stroke)} RG ${x} ${top - height} ${width} ${height} re S`);
    }
  };

  const line = (x1: number, y1: number, x2: number, y2: number, stroke: readonly number[] = PDF.colors.line) => {
    push(`${color(stroke)} RG ${x1} ${y1} m ${x2} ${y2} l S`);
  };

  const text = (
    value: string,
    x: number,
    baseline: number,
    options: {
      size?: number;
      bold?: boolean;
      fill?: readonly number[];
      align?: PdfAlign;
      width?: number;
    } = {},
  ) => {
    const size = options.size ?? 9;
    const font = options.bold ? 'F2' : 'F1';
    const fill = options.fill ?? PDF.colors.ink;
    const align = options.align ?? 'left';
    const estimated = textWidthEstimate(value, size, Boolean(options.bold));
    const offset =
      align === 'right' && options.width
        ? Math.max(options.width - estimated, 0)
        : align === 'center' && options.width
          ? Math.max((options.width - estimated) / 2, 0)
          : 0;
    push(`${color(fill)} rg BT /${font} ${size} Tf ${x + offset} ${baseline} Td (${escapePdfString(value)}) Tj ET`);
  };

  const wrapped = (
    value: string,
    x: number,
    baseline: number,
    width: number,
    options: { size?: number; bold?: boolean; fill?: readonly number[]; maxLines?: number } = {},
  ) => {
    const size = options.size ?? 9;
    const maxChars = Math.max(10, Math.floor(width / (size * 0.5)));
    const lines = wrapPdfText(value, maxChars).slice(0, options.maxLines ?? 6);
    lines.forEach((lineText, index) => {
      text(lineText, x, baseline - index * (size + 3), options);
    });
    return lines.length;
  };

  const sectionTitle = (title: string, subtitle?: string) => {
    ensureSpace(subtitle ? 44 : 28);
    text(title, PDF.marginX, y, { size: 13, bold: true });
    y -= 15;
    if (subtitle) {
      wrapped(subtitle, PDF.marginX, y, PDF.contentWidth, { size: 8, fill: PDF.colors.muted, maxLines: 2 });
      y -= 18;
    } else {
      y -= 5;
    }
    line(PDF.marginX, y, PDF.marginX + PDF.contentWidth, y, PDF.colors.softLine);
    y -= 12;
  };

  const summaryBox = (
    label: string,
    value: string,
    x: number,
    top: number,
    width: number,
    tone: 'debt' | 'credit' | 'neutral',
  ) => {
    const toneColor = tone === 'debt' ? PDF.colors.red : tone === 'credit' ? PDF.colors.green : PDF.colors.purple;
    rect(x, top, width, 58, { fill: PDF.colors.panel, stroke: PDF.colors.softLine });
    text(label, x + 10, top - 20, { size: 8, fill: PDF.colors.muted });
    text(value, x + 10, top - 42, { size: 13, bold: true, fill: toneColor });
  };

  const table = (columns: PdfColumn[], rows: PdfRow[], options: { footerRows?: number } = {}) => {
    const headerHeight = 22;
    ensureSpace(headerHeight + 24);
    rect(PDF.marginX, y, PDF.contentWidth, headerHeight, { fill: PDF.colors.dark });
    let x = PDF.marginX;
    columns.forEach((column) => {
      text(column.label, x + 7, y - 14, {
        size: 7.5,
        bold: true,
        fill: PDF.colors.white,
        align: column.align,
        width: column.width - 14,
      });
      x += column.width;
    });
    y -= headerHeight;

    rows.forEach((row, rowIndex) => {
      const footerStart = rows.length - (options.footerRows ?? 0);
      const isFooter = rowIndex >= footerStart;
      const cellLines = row.map((raw, cellIndex) => {
        const cell = typeof raw === 'string' ? { text: raw } : raw;
        const column = columns[cellIndex]!;
        const size = isFooter || cell.bold ? 8.5 : 8;
        const maxChars = Math.max(8, Math.floor((column.width - 14) / (size * 0.5)));
        return wrapPdfText(cell.text, maxChars).slice(0, 4);
      });
      const lineCount = Math.max(...cellLines.map((lines) => lines.length), 1);
      const rowHeight = Math.max(24, 10 + lineCount * 10);
      ensureSpace(rowHeight);
      if (isFooter) {
        rect(PDF.marginX, y, PDF.contentWidth, rowHeight, { fill: [0.94, 0.95, 0.98] });
      } else if (rowIndex % 2 === 1) {
        rect(PDF.marginX, y, PDF.contentWidth, rowHeight, { fill: [0.985, 0.988, 0.995] });
      }
      line(PDF.marginX, y - rowHeight, PDF.marginX + PDF.contentWidth, y - rowHeight, PDF.colors.softLine);

      x = PDF.marginX;
      row.forEach((raw, cellIndex) => {
        const cell = typeof raw === 'string' ? { text: raw } : raw;
        const column = columns[cellIndex]!;
        const bold = isFooter || Boolean(cell.bold);
        const fill = isFooter ? PDF.colors.ink : PDF.colors.ink;
        const lines = cellLines[cellIndex]!;
        lines.forEach((lineText, lineIndex) => {
          text(lineText, x + 7, y - 15 - lineIndex * 10, {
            size: bold ? 8.5 : 8,
            bold,
            fill,
            align: column.align,
            width: column.width - 14,
          });
        });
        x += column.width;
      });
      y -= rowHeight;
    });
    y -= 18;
  };

  newPage();

  text('Reporte Esteban Munoz', PDF.marginX, y, { size: 24, bold: true });
  text(`Estado de cuentas con TDF al ${formatDateLabel(report.source.cutoffDate)}.`, PDF.marginX, y - 20, {
    size: 10,
    fill: PDF.colors.muted,
  });
  rect(390, y + 2, 180, 64, { fill: [0.98, 0.93, 0.93], stroke: [0.93, 0.7, 0.7] });
  text(directionLabel(report.netDirection), 402, y - 18, { size: 8, bold: true, fill: PDF.colors.red });
  text(formatMoneyPdf(Math.abs(report.netAfterOffsetCents)), 402, y - 43, { size: 18, bold: true, fill: PDF.colors.red });
  text('Saldo neto si se aprueba compensacion', 402, y - 56, { size: 7.5, fill: PDF.colors.muted });
  y -= 98;

  sectionTitle(
    'Resumen ejecutivo',
    'La compensacion de saldos no esta ejecutada; requiere aprobacion contable antes de liquidar.',
  );
  const gap = 8;
  const boxWidth = (PDF.contentWidth - gap * 3) / 4;
  const boxTop = y;
  summaryBox('Arriendo pendiente', formatMoneyPdf(report.rentDueCents), PDF.marginX, boxTop, boxWidth, 'debt');
  summaryBox(
    'Clases de produccion',
    formatMoneyPdf(report.coursePayableCents),
    PDF.marginX + boxWidth + gap,
    boxTop,
    boxWidth,
    'credit',
  );
  summaryBox(
    'Mastering',
    formatMoneyPdf(report.promotionShareRow.estebanShareCents),
    PDF.marginX + (boxWidth + gap) * 2,
    boxTop,
    boxWidth,
    'credit',
  );
  summaryBox(
    'Saldo neto',
    formatMoneyPdf(Math.abs(report.netAfterOffsetCents)),
    PDF.marginX + (boxWidth + gap) * 3,
    boxTop,
    boxWidth,
    'debt',
  );
  y -= 82;

  sectionTitle('Arriendo mensual');
  table(
    [
      { label: 'Concepto', width: 210 },
      { label: 'Periodo', width: 170 },
      { label: 'Cantidad', width: 70, align: 'right' },
      { label: 'Subtotal', width: 78, align: 'right' },
    ],
    [
      [
        { text: 'Meses pendientes de renta', bold: true },
        report.unpaidRentMonths.map(formatMonthLabel).join(', '),
        String(report.unpaidRentMonths.length),
        formatMoneyPdf(report.rentDueCents),
      ],
      [
        'Base aplicada',
        `Ultimo mes pagado: ${formatMonthLabel(report.source.rent.lastPaidMonth)}. Corte: ${formatMonthLabel(report.source.rent.throughMonth)}.`,
        '1',
        formatMoneyPdf(report.source.rent.monthlyAmountCents),
      ],
    ],
  );

  sectionTitle('Comprobante base');
  table(
    [
      { label: 'Fecha', width: 92 },
      { label: 'No.', width: 60 },
      { label: 'Descripcion', width: 170 },
      { label: 'Destino', width: 126 },
      { label: 'Valor', width: 80, align: 'right' },
    ],
    [
      [
        `${report.source.lastReceipt.originalDateLabel}`,
        report.source.lastReceipt.receiptNumber,
        `${report.source.lastReceipt.description}. Aplicado hasta ${formatMonthLabel(report.source.rent.lastPaidMonth)}.`,
        `${report.source.lastReceipt.financialEntity} / ${report.source.lastReceipt.destinationAccount}`,
        formatMoneyPdf(report.source.lastReceipt.amountCents),
      ],
    ],
  );

  sectionTitle('Honorarios por clases de produccion');
  table(
    [
      { label: 'Curso', width: 185 },
      { label: 'Fechas registradas', width: 193 },
      { label: 'Horas', width: 48, align: 'right' },
      { label: 'Tarifa', width: 50, align: 'right' },
      { label: 'Subtotal', width: 52, align: 'right' },
    ],
    [
      ...report.courseRows.map((course): PdfRow => [
        { text: course.title, bold: true },
        course.sessionDates.map(formatDateLabel).join(', '),
        String(course.hours),
        formatMoneyPdf(report.source.coursePayment.hourlyRateCents),
        formatMoneyPdf(course.subtotalCents),
      ]),
      ['Total honorarios', '', '', '', { text: formatMoneyPdf(report.coursePayableCents), bold: true }],
    ],
    { footerRows: 1 },
  );

  newPage();

  sectionTitle('Participacion por realizacion de mastering');
  table(
    [
      { label: 'Cliente', width: 110 },
      { label: 'Concepto', width: 178 },
      { label: 'Pago recibido', width: 90, align: 'right' },
      { label: 'Participacion', width: 74, align: 'right' },
      { label: 'A favor de Esteban', width: 76, align: 'right' },
    ],
    [
      [
        report.promotionShareRow.payerName,
        report.promotionShareRow.concept,
        formatMoneyPdf(report.promotionShareRow.totalPaidCents),
        `${report.promotionShareRow.estebanSharePercent}%`,
        formatMoneyPdf(report.promotionShareRow.estebanShareCents),
      ],
      ['Total mastering', '', '', '', { text: formatMoneyPdf(report.promotionShareRow.estebanShareCents), bold: true }],
    ],
    { footerRows: 1 },
  );

  sectionTitle('Cuentas mantenidas con TDF');
  table(
    [
      { label: 'Cuenta', width: 134 },
      { label: 'Concepto', width: 135 },
      { label: 'Estado', width: 96 },
      { label: 'Direccion', width: 91 },
      { label: 'Monto', width: 72, align: 'right' },
    ],
    report.accountPositions.map((position): PdfRow => [
      { text: `${position.account}. ${position.detail}`, bold: position.id === 'net' },
      position.concept,
      position.status,
      directionLabel(position.direction),
      { text: formatMoneyPdf(position.amountCents), bold: position.id === 'net' },
    ]),
  );

  sectionTitle('Cierre');
  const closingLines = [
    `Total a favor de Esteban: ${formatMoneyPdf(report.payableToEstebanCents)}.`,
    `Arriendo pendiente a favor de TDF: ${formatMoneyPdf(report.rentDueCents)}.`,
    directionAmountLabel(report.netDirection, Math.abs(report.netAfterOffsetCents)),
    'Este reporte presenta un escenario contable de compensacion; no reemplaza la aprobacion final de TDF y Esteban.',
  ];
  closingLines.forEach((lineText) => {
    wrapped(lineText, PDF.marginX, y, PDF.contentWidth, { size: 10, maxLines: 2 });
    y -= 18;
  });

  pages.push(commands);
  return pages.map((pageCommands) => pageCommands.join('\n'));
};

const buildPdfDocument = (contentStreams: string[]) => {
  const pageCount = contentStreams.length;
  const pageObjectStart = 3;
  const contentObjectStart = pageObjectStart + pageCount;
  const fontObjectStart = contentObjectStart + pageCount;
  const fontRegularId = fontObjectStart;
  const fontBoldId = fontObjectStart + 1;
  const pageIds = Array.from({ length: pageCount }, (_, index) => pageObjectStart + index);
  const contentIds = Array.from({ length: pageCount }, (_, index) => contentObjectStart + index);
  const objects = [
    '<< /Type /Catalog /Pages 2 0 R >>',
    `<< /Type /Pages /Kids [${pageIds.map((id) => `${id} 0 R`).join(' ')}] /Count ${pageCount} >>`,
    ...pageIds.map((_, index) =>
      `<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 ${fontRegularId} 0 R /F2 ${fontBoldId} 0 R >> >> /Contents ${contentIds[index]} 0 R >>`,
    ),
    ...contentStreams.map((contentStream) =>
      `<< /Length ${contentStream.length} >>\nstream\n${contentStream}\nendstream`,
    ),
    '<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>',
    '<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica-Bold >>',
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
