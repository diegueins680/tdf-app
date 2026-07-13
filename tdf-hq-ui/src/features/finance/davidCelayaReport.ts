export type ReportDate = `${number}-${number}-${number}`;

export interface WorkBlockSource {
  id: string;
  date: ReportDate;
  startTime: string;
  endTime: string;
  startPhotoLabel: string;
  endPhotoLabel: string;
  description: string;
  discountHours: number;
  discountReason: string;
}

export interface DavidCelayaReportOptions {
  asOfDate?: Date | string;
}

export interface WorkAccountReportSource {
  personName: string;
  reportTitle: string;
  periodLabel: string;
  hourlyRateCents: number;
  notes: string;
  workBlocks: WorkBlockSource[];
}

export interface WorkAccountReportOptions {
  asOfDate?: Date | string;
}

export const DAVID_CELAYA_REPORT_SOURCE: WorkAccountReportSource = {
  personName: 'David Celaya',
  reportTitle: 'Reporte de cuenta',
  periodLabel: '9 y 10 de julio de 2026',
  hourlyRateCents: 2_500,
  notes:
    'Los bloques se tomaron de los timestamps visibles en las capturas compartidas. La tarifa aplicada es de USD 25 por hora; cada bloque se redondea al entero más cercano y además registra un descuento explícito.',
  workBlocks: [
    {
      id: 'work-block-1',
      date: '2026-07-09',
      startTime: '14:35:10',
      endTime: '20:43:50',
      startPhotoLabel: 'Foto 1',
      endPhotoLabel: 'Foto 2',
      description: 'Bloque continuo en el estudio con supervisión de consola y trabajo operativo.',
      discountHours: 1,
      discountReason: 'Descuento explícito de 1 hora en el primer bloque.',
    },
    {
      id: 'work-block-2',
      date: '2026-07-10',
      startTime: '17:37:28',
      endTime: '23:50:32',
      startPhotoLabel: 'Foto 3',
      endPhotoLabel: 'Foto 4',
      description: 'Bloque nocturno con presencia sostenida en el estudio y monitoreo de sesión.',
      discountHours: 2,
      discountReason: 'Descuento explícito de 2 horas en el segundo bloque.',
    },
  ] satisfies WorkBlockSource[],
};

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

const parseReportDate = (value: ReportDate) => {
  const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(value);
  if (!match) throw new Error(`Invalid report date: ${value}`);
  const year = Number.parseInt(match[1]!, 10);
  const month = Number.parseInt(match[2]!, 10);
  const day = Number.parseInt(match[3]!, 10);
  if (!Number.isSafeInteger(year) || month < 1 || month > 12 || day < 1 || day > 31) {
    throw new Error(`Invalid report date: ${value}`);
  }
  return { year, month, day };
};

const toLocalIsoDate = (date: Date) => {
  if (Number.isNaN(date.getTime())) throw new Error('Invalid report date');
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const day = String(date.getDate()).padStart(2, '0');
  return `${year}-${month}-${day}` as ReportDate;
};

const normalizeReportDate = (date: Date | string = new Date()) => {
  if (typeof date !== 'string') return toLocalIsoDate(date);
  if (!/^(\d{4})-(\d{2})-(\d{2})$/.test(date)) throw new Error(`Invalid report date: ${date}`);
  return date as ReportDate;
};

const parseLocalDateTime = (date: ReportDate, time: string) => {
  const parsed = parseReportDate(date);
  if (!/^(\d{2}):(\d{2}):(\d{2})$/.test(time)) {
    throw new Error(`Invalid report time: ${time}`);
  }
  return new Date(`${parsed.year}-${String(parsed.month).padStart(2, '0')}-${String(parsed.day).padStart(2, '0')}T${time}`);
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

export const formatDateTimeLabel = (date: Date) =>
  new Intl.DateTimeFormat('es-EC', {
    day: '2-digit',
    month: 'short',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  }).format(date);

export const formatDuration = (totalSeconds: number) => {
  const sign = totalSeconds < 0 ? '-' : '';
  const absolute = Math.abs(totalSeconds);
  const hours = Math.floor(absolute / 3600);
  const minutes = Math.floor((absolute % 3600) / 60);
  const seconds = absolute % 60;
  return `${sign}${hours}h ${minutes}m ${seconds}s`;
};

const formatShortDateLabel = (isoDate: string) => {
  const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(isoDate);
  if (!match) return isoDate;
  const day = Number.parseInt(match[3]!, 10);
  const month = Number.parseInt(match[2]!, 10);
  const shortMonths = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'] as const;
  return `${day} ${shortMonths[month - 1]} ${match[1]}`;
};

export interface WorkBlockRow extends WorkBlockSource {
  startDateTime: Date;
  endDateTime: Date;
  durationSeconds: number;
  startLabel: string;
  endLabel: string;
  durationLabel: string;
  billableHours: number;
  netBillableHours: number;
  billableCents: number;
  billableLabel: string;
  discountLabel: string;
}

const calculateBillableHours = (durationSeconds: number) => Math.round(durationSeconds / 3600);

const calculateBillableCents = (billableHours: number, hourlyRateCents: number) => billableHours * hourlyRateCents;

export const buildWorkAccountReport = (
  source: WorkAccountReportSource,
  options: WorkAccountReportOptions = {},
) => {
  const lastWorkBlock = source.workBlocks[source.workBlocks.length - 1]!;
  const cutoffDate = normalizeReportDate(options.asOfDate ?? lastWorkBlock.date);
  const workBlocks = source.workBlocks.map((block) => {
    const startDateTime = parseLocalDateTime(block.date, block.startTime);
    const endDateTime = parseLocalDateTime(block.date, block.endTime);
    if (Number.isNaN(startDateTime.getTime()) || Number.isNaN(endDateTime.getTime())) {
      throw new Error(`Invalid work block: ${block.id}`);
    }
    const durationSeconds = Math.max(0, Math.round((endDateTime.getTime() - startDateTime.getTime()) / 1000));
    const billableHours = calculateBillableHours(durationSeconds);
    const netBillableHours = Math.max(0, billableHours - block.discountHours);
    const billableCents = calculateBillableCents(netBillableHours, source.hourlyRateCents);
    return {
      ...block,
      startDateTime,
      endDateTime,
      durationSeconds,
      startLabel: formatDateTimeLabel(startDateTime),
      endLabel: formatDateTimeLabel(endDateTime),
      durationLabel: formatDuration(durationSeconds),
      billableHours,
      netBillableHours,
      billableCents,
      billableLabel: formatCurrency(billableCents),
      discountLabel: `${block.discountHours}h de descuento`,
    } satisfies WorkBlockRow;
  });

  const totalDurationSeconds = workBlocks.reduce((total, block) => total + block.durationSeconds, 0);
  const totalBillableCents = workBlocks.reduce((total, block) => total + block.billableCents, 0);
  const totalBillableHours = workBlocks.reduce((total, block) => total + block.netBillableHours, 0);
  const totalDiscountHours = workBlocks.reduce((total, block) => total + block.discountHours, 0);
  const totalDiscountCents = totalDiscountHours * source.hourlyRateCents;
  const averageDurationSeconds = workBlocks.length > 0 ? Math.round(totalDurationSeconds / workBlocks.length) : 0;
  const evidenceCount = workBlocks.length * 2;
  const firstStart = workBlocks[0]?.startDateTime ?? null;
  const lastEnd = workBlocks[workBlocks.length - 1]?.endDateTime ?? null;

  return {
    source: {
      ...source,
      cutoffDate,
    },
    workBlocks,
    totalDurationSeconds,
    totalBillableCents,
    totalBillableHours,
    totalDiscountHours,
    totalDiscountCents,
    averageDurationSeconds,
    evidenceCount,
    firstStart,
    lastEnd,
  };
};

export const buildDavidCelayaReport = (options: DavidCelayaReportOptions = {}) =>
  buildWorkAccountReport(DAVID_CELAYA_REPORT_SOURCE, options);

export type DavidCelayaReport = ReturnType<typeof buildDavidCelayaReport>;

export const DAVID_CELAYA_REPORT_PDF_FILENAME = 'reporte-david-celaya.pdf';

export const formatCurrency = (cents: number) =>
  new Intl.NumberFormat('es-EC', {
    style: 'currency',
    currency: 'USD',
    maximumFractionDigits: 2,
    minimumFractionDigits: 2,
  }).format(cents / 100).replace('USD', '$');

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

type PdfRow = (string | PdfCell)[];

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
    blue: [0.15, 0.28, 0.75],
    green: [0.12, 0.56, 0.27],
    white: [1, 1, 1],
  },
} as const;

const color = (rgb: readonly number[]) => rgb.join(' ');

const textWidthEstimate = (value: string, fontSize: number, bold = false) =>
  escapePdfStringForWidth(value).length * fontSize * (bold ? 0.55 : 0.5);

const buildPdfContentStream = (report: ReturnType<typeof buildWorkAccountReport>) => {
  const pages: string[][] = [];
  let commands: string[] = [];
  let y: number = PDF.headerTop;
  let pageNumber = 0;

  const push = (command: string) => commands.push(command);

  const newPage = () => {
    if (commands.length > 0) pages.push(commands);
    commands = [];
    pageNumber += 1;
    y = PDF.headerTop;
    push(`${color(PDF.colors.dark)} rg 0 742 612 50 re f`);
    push(`${color(PDF.colors.white)} rg`);
    push(`BT /F2 15 Tf 42 765 Td (TDF RECORDS) Tj ET`);
    push(`BT /F1 8 Tf 42 752 Td (Reporte de cuenta / pagina ${pageNumber}) Tj ET`);
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
    tone: 'blue' | 'green',
  ) => {
    const toneColor = tone === 'blue' ? PDF.colors.blue : PDF.colors.green;
    rect(x, top, width, 58, { fill: PDF.colors.panel, stroke: PDF.colors.softLine });
    text(label, x + 10, top - 20, { size: 8, fill: PDF.colors.muted });
    text(value, x + 10, top - 42, { size: 13, bold: true, fill: toneColor });
  };

  const table = (columns: PdfColumn[], rows: PdfRow[]) => {
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
      const cellLines = row.map((raw, cellIndex) => {
        const cell = typeof raw === 'string' ? { text: raw } : raw;
        const column = columns[cellIndex]!;
        const size = cell.bold ? 8.5 : 8;
        const maxChars = Math.max(8, Math.floor((column.width - 14) / (size * 0.5)));
        return wrapPdfText(cell.text, maxChars).slice(0, 4);
      });
      const lineCount = Math.max(...cellLines.map((lines) => lines.length), 1);
      const rowHeight = Math.max(24, 10 + lineCount * 10);
      ensureSpace(rowHeight);
      if (rowIndex % 2 === 1) {
        rect(PDF.marginX, y, PDF.contentWidth, rowHeight, { fill: [0.985, 0.988, 0.995] });
      }
      line(PDF.marginX, y - rowHeight, PDF.marginX + PDF.contentWidth, y - rowHeight, PDF.colors.softLine);

      x = PDF.marginX;
      row.forEach((raw, cellIndex) => {
        const cell = typeof raw === 'string' ? { text: raw } : raw;
        const column = columns[cellIndex]!;
        const lines = cellLines[cellIndex]!;
        lines.forEach((lineText, lineIndex) => {
          text(lineText, x + 7, y - 15 - lineIndex * 10, {
            size: cell.bold ? 8.5 : 8,
            bold: Boolean(cell.bold),
            fill: PDF.colors.ink,
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

  text('Reporte de cuenta', PDF.marginX, y, { size: 24, bold: true });
  text(`Cliente: ${report.source.personName}`, PDF.marginX, y - 20, { size: 10, fill: PDF.colors.muted });
  text(`Periodo: ${report.source.periodLabel}`, PDF.marginX, y - 34, { size: 10, fill: PDF.colors.muted });
  rect(382, y + 2, 188, 64, { fill: [0.93, 0.95, 0.99], stroke: [0.74, 0.81, 0.95] });
  text('Importe total', 394, y - 18, { size: 8, bold: true, fill: PDF.colors.blue });
  text(formatCurrency(report.totalBillableCents), 394, y - 43, { size: 18, bold: true, fill: PDF.colors.blue });
  text('USD 25/h + descuento explicito por bloque', 394, y - 56, { size: 7.5, fill: PDF.colors.muted });
  y -= 98;

  sectionTitle(
    'Resumen ejecutivo',
    'Se registran dos jornadas continuas en estudio. Las capturas visibles se usaron como referencia para abrir y cerrar cada bloque.',
  );

  const gap = 8;
  const boxWidth = (PDF.contentWidth - gap * 4) / 5;
  const boxTop = y;
  summaryBox('Bloques', String(report.workBlocks.length), PDF.marginX, boxTop, boxWidth, 'blue');
  summaryBox('Capturas', String(report.evidenceCount), PDF.marginX + boxWidth + gap, boxTop, boxWidth, 'blue');
  summaryBox('Total', formatCurrency(report.totalBillableCents), PDF.marginX + (boxWidth + gap) * 2, boxTop, boxWidth, 'green');
  summaryBox(
    'Horas netas',
    `${report.totalBillableHours}h`,
    PDF.marginX + (boxWidth + gap) * 3,
    boxTop,
    boxWidth,
    'green',
  );
  summaryBox(
    'Corte',
    report.source.cutoffDate,
    PDF.marginX + (boxWidth + gap) * 4,
    boxTop,
    boxWidth,
    'blue',
  );
  y -= 82;

  sectionTitle('Bloques de trabajo', report.source.notes);
  table(
    [
      { label: 'Fecha', width: 58 },
      { label: 'Inicio', width: 108 },
      { label: 'Fin', width: 108 },
      { label: 'Base', width: 44, align: 'right' },
      { label: 'Desc.', width: 44, align: 'right' },
      { label: 'Neto', width: 44, align: 'right' },
      { label: 'Importe', width: 58, align: 'right' },
      { label: 'Nota', width: 64 },
    ],
    report.workBlocks.map((block): PdfRow => [
      { text: formatShortDateLabel(block.date), bold: true },
      `${block.startLabel} (${block.startPhotoLabel})`,
      `${block.endLabel} (${block.endPhotoLabel})`,
      { text: `${block.billableHours}h`, bold: true },
      { text: `-${block.discountHours}h`, bold: true },
      { text: `${block.netBillableHours}h`, bold: true },
      { text: block.billableLabel, bold: true },
      { text: block.description, bold: false },
    ]),
  );

  sectionTitle('Cierre');
  const closingLines = [
    `Primer registro: ${report.firstStart ? formatDateTimeLabel(report.firstStart) : 'N/D'}.`,
    `Último cierre: ${report.lastEnd ? formatDateTimeLabel(report.lastEnd) : 'N/D'}.`,
    `Horas netas facturables: ${report.totalBillableHours}h.`,
    `Descuento acumulado: ${report.totalDiscountHours}h = ${formatCurrency(report.totalDiscountCents)}.`,
    `Tarifa aplicada: ${formatCurrency(report.source.hourlyRateCents)} por hora con descuento explicito por bloque.`,
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

export const buildWorkAccountReportPdfSource = (report: ReturnType<typeof buildWorkAccountReport>) =>
  buildPdfDocument(buildPdfContentStream(report));

export const buildWorkAccountReportPdfBlob = (report: ReturnType<typeof buildWorkAccountReport>) =>
  new Blob([buildWorkAccountReportPdfSource(report)], { type: 'application/pdf' });

export const buildDavidCelayaReportPdfSource = (report: DavidCelayaReport = buildDavidCelayaReport()) =>
  buildWorkAccountReportPdfSource(report);

export const buildDavidCelayaReportPdfBlob = (report: DavidCelayaReport = buildDavidCelayaReport()) =>
  buildWorkAccountReportPdfBlob(report);
