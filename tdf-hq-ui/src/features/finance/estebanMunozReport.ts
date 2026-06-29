export type YearMonth = `${number}-${string}`;

export type AccountDirection = 'esteban_owes_tdf' | 'tdf_owes_esteban' | 'settled';

export interface CourseCompensationSource {
  slug: string;
  title: string;
  sourceLabel: string;
  sessionDates: string[];
  hours: number;
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

export const buildEstebanMunozReport = () => {
  const source = ESTEBAN_MUNOZ_REPORT_SOURCE;
  const unpaidRentMonths = listMonthsAfterThrough(source.rent.lastPaidMonth, source.rent.throughMonth);
  const rentDueCents = unpaidRentMonths.length * source.rent.monthlyAmountCents;
  const courseRows = source.coursePayment.courses.map((course) => ({
    ...course,
    subtotalCents: course.hours * source.coursePayment.hourlyRateCents,
  }));
  const coursePayableCents = courseRows.reduce((total, course) => total + course.subtotalCents, 0);
  const netAfterOffsetCents = rentDueCents - coursePayableCents;
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
      concept: 'Compensación arriendo - honorarios',
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
    netAfterOffsetCents,
    netDirection,
    accountPositions,
  };
};
