export type EventType = 'wedding' | 'corporate' | 'retreat' | 'concert' | 'workshop' | 'photo';

interface EventTypeConfig {
  label: string;
  perGuestCents: number;
  minimumHours: number;
  includedGuests: number;
}

export interface BookingFormState {
  fullName: string;
  email: string;
  phone: string;
  eventType: EventType;
  guests: number;
  startsAt: string;
  durationHours: number;
  setupHours: number;
  catering: boolean;
  production: boolean;
  transport: boolean;
  notes: string;
}

interface QuoteLine {
  label: string;
  amountCents: number;
}

const TAX_RATE = 0.12;
export const MAX_QUOTE_GUESTS = 220;

export const EVENT_TYPES: Record<EventType, EventTypeConfig> = {
  wedding: {
    label: 'Boda',
    perGuestCents: 800,
    minimumHours: 8,
    includedGuests: 60,
  },
  corporate: {
    label: 'Evento corporativo',
    perGuestCents: 600,
    minimumHours: 6,
    includedGuests: 40,
  },
  retreat: {
    label: 'Retiro o taller',
    perGuestCents: 500,
    minimumHours: 6,
    includedGuests: 25,
  },
  concert: {
    label: 'Concierto',
    perGuestCents: 700,
    minimumHours: 7,
    includedGuests: 80,
  },
  workshop: {
    label: 'Taller',
    perGuestCents: 450,
    minimumHours: 4,
    includedGuests: 20,
  },
  photo: {
    label: 'Sesión fotográfica',
    perGuestCents: 300,
    minimumHours: 3,
    includedGuests: 8,
  },
};

export const clampNumber = (value: number, min: number, max: number) => {
  if (!Number.isFinite(value)) return min;
  return Math.min(max, Math.max(min, Math.round(value)));
};

export const calculateQuote = (form: BookingFormState) => {
  const config = EVENT_TYPES[form.eventType];
  const guests = clampNumber(form.guests, 1, MAX_QUOTE_GUESTS);
  const billableHours = Math.max(config.minimumHours, clampNumber(form.durationHours, 1, 24));
  const setupHours = clampNumber(form.setupHours, 0, 12);
  const extraGuests = Math.max(0, guests - config.includedGuests);
  const lines: QuoteLine[] = [
    { label: `Uso del espacio por ${billableHours} horas`, amountCents: billableHours * 18000 },
  ];

  if (setupHours > 0) {
    lines.push({ label: `Montaje y desmontaje (${setupHours} horas)`, amountCents: setupHours * 7000 });
  }
  if (extraGuests > 0) {
    lines.push({ label: `${extraGuests} invitados adicionales`, amountCents: extraGuests * config.perGuestCents });
  }
  if (form.catering) {
    lines.push({ label: 'Catering y barra operados por Domo', amountCents: Math.max(35000, guests * 650) });
  }
  if (form.production) {
    lines.push({ label: 'Sonido e iluminación base', amountCents: 42000 });
  }
  if (form.transport) {
    lines.push({ label: 'Coordinación de transporte Quito - Pululahua', amountCents: 30000 });
  }

  const subtotalCents = lines.reduce((sum, line) => sum + line.amountCents, 0);
  const taxCents = Math.round(subtotalCents * TAX_RATE);
  const totalCents = subtotalCents + taxCents;
  const depositCents = Math.round(totalCents * 0.4);

  return { lines, subtotalCents, taxCents, totalCents, depositCents, billableHours, guests };
};
