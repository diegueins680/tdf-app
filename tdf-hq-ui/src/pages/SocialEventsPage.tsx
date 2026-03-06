import { useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient, useQueries } from '@tanstack/react-query';
import {
  Alert,
  Checkbox,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Divider,
  Grid,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import CalendarMonthIcon from '@mui/icons-material/CalendarMonth';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import CheckIcon from '@mui/icons-material/Check';
import ClearIcon from '@mui/icons-material/Clear';
import HelpOutlineIcon from '@mui/icons-material/HelpOutline';
import RefreshIcon from '@mui/icons-material/Refresh';
import ConfirmationNumberIcon from '@mui/icons-material/ConfirmationNumber';
import SellIcon from '@mui/icons-material/Sell';
import QrCodeScannerIcon from '@mui/icons-material/QrCodeScanner';
import { DateCalendar } from '@mui/x-date-pickers/DateCalendar';
import { LocalizationProvider } from '@mui/x-date-pickers/LocalizationProvider';
import { AdapterLuxon } from '@mui/x-date-pickers/AdapterLuxon';
import { DateTime } from 'luxon';
import {
  SocialEventsAPI,
  type SocialEventDTO,
  type SocialEventBudgetLineDTO,
  type SocialEventFinanceEntryDTO,
  type SocialEventFinanceSummaryDTO,
  type SocialInvitationDTO,
  type SocialRsvpStatus,
  type SocialTicketOrderDTO,
  type SocialTicketTierDTO,
} from '../api/socialEvents';
import { ContractsAPI } from '../api/contracts';
import { useSession } from '../session/SessionContext';
import { parseUnsignedSafeInt } from '../utils/ids';

interface InvitationState {
  partyId: string;
  message: string;
}

interface TicketPurchaseState {
  tierId: string;
  quantity: string;
  buyerName: string;
  buyerEmail: string;
}

interface TicketTierFormState {
  code: string;
  name: string;
  price: string;
  quantity: string;
  currency: string;
}

interface BudgetLineFormState {
  code: string;
  name: string;
  type: 'income' | 'expense';
  category: string;
  plannedCents: string;
  notes: string;
}

interface FinanceEntryFormState {
  budgetLineId: string;
  direction: 'income' | 'expense';
  source: string;
  category: string;
  concept: string;
  amountCents: string;
  currency: string;
  status: 'draft' | 'posted' | 'pending' | 'void';
  externalRef: string;
  notes: string;
  occurredAt: string;
}

interface ContractDraftState {
  kind: string;
  counterparty: string;
  concept: string;
  amountCents: string;
  currency: string;
  status: 'draft' | 'pending' | 'posted';
  notes: string;
}

interface EventDraftState {
  title: string;
  description: string;
  startAt: string;
  endAt: string;
  venueId: string;
  eventType: string;
  eventStatus: string;
  priceCents: string;
  capacity: string;
  currency: string;
  ticketUrl: string;
  isPublic: boolean;
}

const BUDGET_CATEGORY_OPTIONS = [
  'tickets',
  'sponsorship',
  'talent',
  'production',
  'marketing',
  'operations',
  'logistics',
  'permits',
  'security',
  'hospitality',
  'procurement',
  'contracting',
  'assets',
  'liabilities',
  'general',
] as const;

const FINANCE_SOURCE_OPTIONS: {
  value: string;
  label: string;
  direction: 'income' | 'expense';
  category: string;
}[] = [
  { value: 'ticket_sale', label: 'Venta de tickets', direction: 'income', category: 'tickets' },
  { value: 'ticket_refund', label: 'Reembolso de tickets', direction: 'expense', category: 'tickets' },
  { value: 'sponsorship', label: 'Patrocinio', direction: 'income', category: 'sponsorship' },
  { value: 'contract_commitment', label: 'Contrato (compromiso)', direction: 'expense', category: 'contracting' },
  { value: 'contract_payment', label: 'Contrato (pago)', direction: 'expense', category: 'contracting' },
  { value: 'purchase_order', label: 'Compra (orden)', direction: 'expense', category: 'procurement' },
  { value: 'purchase_payment', label: 'Compra (pago)', direction: 'expense', category: 'procurement' },
  { value: 'asset_purchase', label: 'Activo (adquisición)', direction: 'expense', category: 'assets' },
  { value: 'liability_loan', label: 'Pasivo (financiamiento)', direction: 'income', category: 'liabilities' },
  { value: 'liability_payment', label: 'Pasivo (pago)', direction: 'expense', category: 'liabilities' },
  { value: 'accounts_receivable', label: 'Cuenta por cobrar', direction: 'income', category: 'operations' },
  { value: 'accounts_receivable_collection', label: 'Cobro de CxC', direction: 'income', category: 'operations' },
  { value: 'vendor_payment', label: 'Pago a proveedor', direction: 'expense', category: 'operations' },
  { value: 'merchandise', label: 'Merchandising', direction: 'income', category: 'operations' },
  { value: 'operations', label: 'Operación', direction: 'expense', category: 'operations' },
  { value: 'manual', label: 'Manual', direction: 'expense', category: 'general' },
  { value: 'other', label: 'Otro', direction: 'expense', category: 'general' },
];

const FINANCE_STATUS_OPTIONS: {
  value: FinanceEntryFormState['status'];
  label: string;
}[] = [
  { value: 'posted', label: 'Publicado' },
  { value: 'pending', label: 'Pendiente' },
  { value: 'draft', label: 'Borrador' },
  { value: 'void', label: 'Anulado' },
];

const PROJECT_BUDGET_TEMPLATE: {
  code: string;
  name: string;
  type: 'income' | 'expense';
  category: string;
}[] = [
  { code: 'INC-TICKETS', name: 'Venta de tickets', type: 'income', category: 'tickets' },
  { code: 'INC-SPONSORS', name: 'Patrocinios', type: 'income', category: 'sponsorship' },
  { code: 'EXP-TALENT', name: 'Contratación de talento', type: 'expense', category: 'contracting' },
  { code: 'EXP-PROD', name: 'Producción técnica', type: 'expense', category: 'production' },
  { code: 'EXP-MKT', name: 'Marketing y pauta', type: 'expense', category: 'marketing' },
  { code: 'EXP-LOG', name: 'Logística y operación', type: 'expense', category: 'logistics' },
  { code: 'EXP-PERMITS', name: 'Permisos y legales', type: 'expense', category: 'operations' },
  { code: 'EXP-SECURITY', name: 'Seguridad', type: 'expense', category: 'security' },
];

const formatDate = (iso: string) =>
  DateTime.fromISO(iso).setLocale('es').toFormat('EEE d LLL, HH:mm');

const formatMoney = (amountCents?: number | null, currency?: string | null) => {
  if (typeof amountCents !== 'number') return 'Gratis';
  const code = (currency ?? 'USD').toUpperCase();
  return `${code} ${(amountCents / 100).toFixed(2)}`;
};

const formatPercent = (value?: number | null) => {
  if (typeof value !== 'number' || Number.isNaN(value)) return 'n/a';
  return `${value.toFixed(1)}%`;
};

const tierAvailability = (tier: SocialTicketTierDTO) =>
  Math.max(0, tier.ticketTierQuantityTotal - tier.ticketTierQuantitySold);

const toDateTimeInputValue = (iso?: string | null) => {
  if (!iso) return DateTime.local().toFormat("yyyy-LL-dd'T'HH:mm");
  const dt = DateTime.fromISO(iso);
  if (!dt.isValid) return DateTime.local().toFormat("yyyy-LL-dd'T'HH:mm");
  return dt.toLocal().toFormat("yyyy-LL-dd'T'HH:mm");
};

const toUtcIso = (dateTimeInput: string) => {
  const dt = DateTime.fromISO(dateTimeInput, { zone: 'local' });
  return dt.isValid ? (dt.toUTC().toISO() ?? new Date().toISOString()) : new Date().toISOString();
};

const parseOptionalUnsignedInt = (value: string): number | null => {
  const trimmed = value.trim();
  if (!trimmed) return null;
  return parseUnsignedSafeInt(trimmed);
};

const dateKeyFromIso = (iso: string) => DateTime.fromISO(iso).toISODate() ?? '';

const buildInitialEventDraft = (): EventDraftState => {
  const startAt = DateTime.local().plus({ hours: 1 }).startOf('hour');
  const endAt = startAt.plus({ hours: 2 });
  return {
    title: '',
    description: '',
    startAt: startAt.toFormat("yyyy-LL-dd'T'HH:mm"),
    endAt: endAt.toFormat("yyyy-LL-dd'T'HH:mm"),
    venueId: '',
    eventType: 'party',
    eventStatus: 'planning',
    priceCents: '',
    capacity: '',
    currency: 'USD',
    ticketUrl: '',
    isPublic: true,
  };
};

const csvEscape = (value: string | number | boolean | null | undefined) =>
  `"${String(value ?? '').replace(/"/g, '""')}"`;

const downloadCsvFile = (filename: string, rows: (string | number | boolean | null | undefined)[][]) => {
  const csv = rows.map((row) => row.map(csvEscape).join(',')).join('\n');
  const blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = url;
  link.download = filename;
  link.click();
  URL.revokeObjectURL(url);
};

const buildEventFinanceCsvRows = (
  event: SocialEventDTO,
  budgetLines: SocialEventBudgetLineDTO[],
  financeEntries: SocialEventFinanceEntryDTO[],
  financeSummary: SocialEventFinanceSummaryDTO | null,
) => {
  const rows: (string | number | boolean | null | undefined)[][] = [];
  rows.push([
    'record_type',
    'event_id',
    'event_title',
    'event_start',
    'event_end',
    'currency',
    'metric_name',
    'metric_value',
    'line_id',
    'line_code',
    'line_name',
    'line_type',
    'line_category',
    'line_planned_cents',
    'line_actual_cents',
    'entry_id',
    'entry_direction',
    'entry_source',
    'entry_category',
    'entry_status',
    'entry_concept',
    'entry_amount_cents',
    'entry_external_ref',
    'entry_occurred_at',
  ]);

  const eventId = event.eventId ?? '';
  const eventTitle = event.eventTitle;
  const eventStart = event.eventStart;
  const eventEnd = event.eventEnd;
  const eventCurrency = (financeSummary?.efsCurrency ?? event.eventCurrency ?? 'USD').toUpperCase();

  const summaryMetrics: [string, number | string | null | undefined][] = [
    ['budget_cents', financeSummary?.efsBudgetCents],
    ['planned_income_cents', financeSummary?.efsPlannedIncomeCents],
    ['planned_expense_cents', financeSummary?.efsPlannedExpenseCents],
    ['actual_income_cents', financeSummary?.efsActualIncomeCents],
    ['actual_expense_cents', financeSummary?.efsActualExpenseCents],
    ['net_cents', financeSummary?.efsNetCents],
    ['ticket_paid_revenue_cents', financeSummary?.efsTicketPaidRevenueCents],
    ['ticket_refunded_revenue_cents', financeSummary?.efsTicketRefundedRevenueCents],
    ['ticket_pending_revenue_cents', financeSummary?.efsTicketPendingRevenueCents],
    ['accounts_payable_cents', financeSummary?.efsAccountsPayableCents],
    ['accounts_receivable_cents', financeSummary?.efsAccountsReceivableCents],
    ['contract_committed_cents', financeSummary?.efsContractCommittedCents],
    ['contract_paid_cents', financeSummary?.efsContractPaidCents],
    ['procurement_committed_cents', financeSummary?.efsProcurementCommittedCents],
    ['procurement_paid_cents', financeSummary?.efsProcurementPaidCents],
    ['asset_investment_cents', financeSummary?.efsAssetInvestmentCents],
    ['liability_balance_cents', financeSummary?.efsLiabilityBalanceCents],
    ['budget_variance_cents', financeSummary?.efsBudgetVarianceCents],
    ['budget_utilization_pct', financeSummary?.efsBudgetUtilizationPct],
  ];

  summaryMetrics.forEach(([metricName, metricValue]) => {
    rows.push([
      'summary',
      eventId,
      eventTitle,
      eventStart,
      eventEnd,
      eventCurrency,
      metricName,
      metricValue,
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
    ]);
  });

  budgetLines.forEach((line) => {
    rows.push([
      'budget_line',
      eventId,
      eventTitle,
      eventStart,
      eventEnd,
      eventCurrency,
      '',
      '',
      line.eblId ?? '',
      line.eblCode,
      line.eblName,
      line.eblType,
      line.eblCategory,
      line.eblPlannedCents,
      line.eblActualCents ?? '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
    ]);
  });

  financeEntries.forEach((entry) => {
    rows.push([
      'finance_entry',
      eventId,
      eventTitle,
      eventStart,
      eventEnd,
      eventCurrency,
      '',
      '',
      entry.efeBudgetLineId ?? '',
      '',
      '',
      '',
      '',
      '',
      '',
      entry.efeId ?? '',
      entry.efeDirection,
      entry.efeSource,
      entry.efeCategory,
      entry.efeStatus,
      entry.efeConcept,
      entry.efeAmountCents,
      entry.efeExternalRef ?? '',
      entry.efeOccurredAt,
    ]);
  });

  return rows;
};

export default function SocialEventsPage() {
  const qc = useQueryClient();
  const { session } = useSession();
  const [city, setCity] = useState('');
  const [eventTypeFilter, setEventTypeFilter] = useState('');
  const [eventStatusFilter, setEventStatusFilter] = useState('');
  const [feedback, setFeedback] = useState<{ kind: 'success' | 'error'; message: string } | null>(null);
  const [invites, setInvites] = useState<Record<string, InvitationState>>({});
  const [ticketPurchases, setTicketPurchases] = useState<Record<string, TicketPurchaseState>>({});
  const [ticketTierForms, setTicketTierForms] = useState<Record<string, TicketTierFormState>>({});
  const [budgetLineForms, setBudgetLineForms] = useState<Record<string, BudgetLineFormState>>({});
  const [financeEntryForms, setFinanceEntryForms] = useState<Record<string, FinanceEntryFormState>>({});
  const [contractDrafts, setContractDrafts] = useState<Record<string, ContractDraftState>>({});
  const [checkInCodes, setCheckInCodes] = useState<Record<string, string>>({});
  const [eventPosterFiles, setEventPosterFiles] = useState<Record<string, File | null>>({});
  const [eventDraft, setEventDraft] = useState<EventDraftState>(() => buildInitialEventDraft());
  const [selectedCalendarDate, setSelectedCalendarDate] = useState<string>(() => DateTime.local().toISODate() ?? '');
  const startAfter = useMemo(() => new Date().toISOString(), []);
  const sessionPartyId = session?.partyId != null ? String(session.partyId) : null;
  const hasSession = Boolean(sessionPartyId);
  const eventsQueryKey = ['social-events', city, eventTypeFilter, eventStatusFilter, startAfter] as const;

  const eventsQuery = useQuery({
    queryKey: eventsQueryKey,
    queryFn: () =>
      SocialEventsAPI.listEvents({
        city: city.trim() || undefined,
        eventType: eventTypeFilter || undefined,
        eventStatus: eventStatusFilter || undefined,
        startAfter,
      }),
  });

  const venuesQuery = useQuery({
    queryKey: ['social-venues', city],
    queryFn: () => SocialEventsAPI.listVenues({ city: city.trim() || undefined }),
  });

  const venueById = useMemo(() => {
    const map = new Map<string, string>();
    (venuesQuery.data ?? []).forEach((v) => {
      if (v.venueId) map.set(v.venueId, v.venueName);
    });
    return map;
  }, [venuesQuery.data]);

  const events = useMemo(() => eventsQuery.data ?? [], [eventsQuery.data]);

  const eventsByDate = useMemo(() => {
    const grouped = new Map<string, SocialEventDTO[]>();
    events.forEach((event) => {
      const key = dateKeyFromIso(event.eventStart);
      if (!key) return;
      if (!grouped.has(key)) grouped.set(key, []);
      grouped.get(key)?.push(event);
    });
    grouped.forEach((dayEvents) => {
      dayEvents.sort((a, b) => a.eventStart.localeCompare(b.eventStart));
    });
    return grouped;
  }, [events]);

  const calendarDateOptions = useMemo(
    () => Array.from(eventsByDate.keys()).sort((a, b) => a.localeCompare(b)),
    [eventsByDate],
  );

  const selectedCalendarEvents = useMemo(
    () => (selectedCalendarDate ? eventsByDate.get(selectedCalendarDate) ?? [] : []),
    [eventsByDate, selectedCalendarDate],
  );

  const selectedCalendarValue = useMemo(() => {
    const parsed = DateTime.fromISO(selectedCalendarDate);
    return parsed.isValid ? parsed : DateTime.local();
  }, [selectedCalendarDate]);

  const selectedCalendarLabel = useMemo(() => {
    const parsed = DateTime.fromISO(selectedCalendarDate);
    if (!parsed.isValid) return 'Selecciona una fecha';
    return parsed.setLocale('es').toFormat('cccc d LLL yyyy');
  }, [selectedCalendarDate]);

  useEffect(() => {
    if (selectedCalendarDate) return;
    if (calendarDateOptions.length > 0) {
      setSelectedCalendarDate(calendarDateOptions[0] ?? '');
      return;
    }
    setSelectedCalendarDate(DateTime.local().toISODate() ?? '');
  }, [calendarDateOptions, selectedCalendarDate]);

  const invitationQueries = useQueries({
    queries:
      hasSession && events.length > 0
        ? events.map((ev) => ({
            queryKey: ['social-invitations', ev.eventId],
            queryFn: () => SocialEventsAPI.listInvitations(String(ev.eventId)),
            enabled: Boolean(ev.eventId),
            select: (list: SocialInvitationDTO[]) =>
              list?.filter((inv) => String(inv.invitationToPartyId) === String(sessionPartyId ?? '')) ?? [],
          }))
        : [],
  });

  const ticketTierQueries = useQueries({
    queries:
      events.length > 0
        ? events.map((ev) => ({
            queryKey: ['social-ticket-tiers', ev.eventId],
            queryFn: () => SocialEventsAPI.listTicketTiers(String(ev.eventId)),
            enabled: Boolean(ev.eventId),
          }))
        : [],
  });

  const ticketOrderQueries = useQueries({
    queries:
      hasSession && events.length > 0
        ? events.map((ev) => {
            const organizer =
              sessionPartyId != null &&
              ev.eventOrganizerPartyId != null &&
              String(ev.eventOrganizerPartyId) === sessionPartyId;
            return {
              queryKey: ['social-ticket-orders', ev.eventId, organizer ? 'organizer' : sessionPartyId],
              queryFn: () =>
                SocialEventsAPI.listTicketOrders(String(ev.eventId), organizer ? undefined : { buyerPartyId: sessionPartyId ?? undefined }),
              enabled: Boolean(ev.eventId),
            };
          })
        : [],
  });

  const budgetLineQueries = useQueries({
    queries:
      hasSession && events.length > 0
        ? events.map((ev) => {
            const organizer =
              sessionPartyId != null &&
              ev.eventOrganizerPartyId != null &&
              String(ev.eventOrganizerPartyId) === sessionPartyId;
            return {
              queryKey: ['social-budget-lines', ev.eventId],
              queryFn: () => SocialEventsAPI.listBudgetLines(String(ev.eventId)),
              enabled: organizer && Boolean(ev.eventId),
            };
          })
        : [],
  });

  const financeEntryQueries = useQueries({
    queries:
      hasSession && events.length > 0
        ? events.map((ev) => {
            const organizer =
              sessionPartyId != null &&
              ev.eventOrganizerPartyId != null &&
              String(ev.eventOrganizerPartyId) === sessionPartyId;
            return {
              queryKey: ['social-finance-entries', ev.eventId],
              queryFn: () => SocialEventsAPI.listFinanceEntries(String(ev.eventId)),
              enabled: organizer && Boolean(ev.eventId),
            };
          })
        : [],
  });

  const financeSummaryQueries = useQueries({
    queries:
      hasSession && events.length > 0
        ? events.map((ev) => {
            const organizer =
              sessionPartyId != null &&
              ev.eventOrganizerPartyId != null &&
              String(ev.eventOrganizerPartyId) === sessionPartyId;
            return {
              queryKey: ['social-finance-summary', ev.eventId],
              queryFn: () => SocialEventsAPI.getFinanceSummary(String(ev.eventId)),
              enabled: organizer && Boolean(ev.eventId),
            };
          })
        : [],
  });

  const createEventMutation = useMutation({
    mutationFn: () => {
      if (!hasSession) throw new Error('Inicia sesión para crear eventos.');
      const title = eventDraft.title.trim();
      if (!title) throw new Error('Título del evento requerido.');
      const startIso = toUtcIso(eventDraft.startAt);
      const endIso = toUtcIso(eventDraft.endAt);
      const startAt = DateTime.fromISO(startIso);
      const endAt = DateTime.fromISO(endIso);
      if (!startAt.isValid || !endAt.isValid || startAt >= endAt) {
        throw new Error('La fecha de fin debe ser posterior al inicio.');
      }
      const priceRaw = eventDraft.priceCents.trim();
      const priceCents = parseOptionalUnsignedInt(priceRaw);
      if (priceRaw !== '' && priceCents === null) {
        throw new Error('Precio inválido (usa centavos).');
      }
      const capacityRaw = eventDraft.capacity.trim();
      const capacity = parseOptionalUnsignedInt(capacityRaw);
      if (capacityRaw !== '' && capacity === null) {
        throw new Error('Capacidad inválida.');
      }
      const payload: SocialEventDTO = {
        eventTitle: title,
        eventDescription: eventDraft.description.trim() || null,
        eventStart: startIso,
        eventEnd: endIso,
        eventVenueId: eventDraft.venueId.trim() || null,
        eventPriceCents: priceCents,
        eventCapacity: capacity,
        eventType: eventDraft.eventType || null,
        eventStatus: eventDraft.eventStatus || null,
        eventCurrency: eventDraft.currency.trim().toUpperCase() || 'USD',
        eventBudgetCents: null,
        eventTicketUrl: eventDraft.ticketUrl.trim() || null,
        eventImageUrl: null,
        eventIsPublic: eventDraft.isPublic,
        eventArtists: [],
      };
      return SocialEventsAPI.createEvent(payload);
    },
    onSuccess: (createdEvent) => {
      const nextCalendarDate = dateKeyFromIso(createdEvent.eventStart);
      if (nextCalendarDate) setSelectedCalendarDate(nextCalendarDate);
      setEventDraft(buildInitialEventDraft());
      qc.setQueryData<SocialEventDTO[]>(eventsQueryKey, (prev = []) => {
        const createdId = createdEvent.eventId != null ? String(createdEvent.eventId) : null;
        const withoutCreated = createdId
          ? prev.filter((event) => String(event.eventId ?? '') !== createdId)
          : prev;
        return [createdEvent, ...withoutCreated];
      });
      void qc.invalidateQueries({ queryKey: ['social-events'] });
      setFeedback({ kind: 'success', message: 'Evento creado y agregado al Event Calendar.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const rsvpMutation = useMutation({
    mutationFn: ({ eventId, status }: { eventId: string; status: SocialRsvpStatus }) => {
      if (!sessionPartyId) throw new Error('Inicia sesión para confirmar asistencia.');
      return SocialEventsAPI.rsvp(eventId, sessionPartyId, status);
    },
    onSuccess: () => {
      setFeedback({ kind: 'success', message: 'RSVP registrado.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const inviteMutation = useMutation({
    mutationFn: ({ eventId }: { eventId: string }) => {
      if (!sessionPartyId) throw new Error('Inicia sesión para enviar invitaciones.');
      const draft = invites[eventId] ?? { partyId: '', message: '' };
      const toId = draft.partyId.trim();
      if (!toId) throw new Error('Ingresa el ID de la persona a invitar.');
      return SocialEventsAPI.sendInvitation(eventId, {
        invitationToPartyId: toId,
        invitationMessage: draft.message.trim() || null,
      });
    },
    onSuccess: (_resp, { eventId }) => {
      setInvites((prev) => ({ ...prev, [eventId]: { partyId: '', message: '' } }));
      void qc.invalidateQueries({ queryKey: ['social-invitations', eventId] });
      setFeedback({ kind: 'success', message: 'Invitación enviada.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const respondInvitationMutation = useMutation({
    mutationFn: ({ eventId, invitationId, status }: { eventId: string; invitationId: string; status: string }) => {
      if (!sessionPartyId) throw new Error('Inicia sesión para responder invitaciones.');
      return SocialEventsAPI.respondInvitation(eventId, invitationId, status);
    },
    onSuccess: (_resp, { eventId }) => {
      void qc.invalidateQueries({ queryKey: ['social-invitations', eventId] });
      setFeedback({ kind: 'success', message: 'Respuesta enviada.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const purchaseTicketsMutation = useMutation({
    mutationFn: ({ eventId, fallbackTierId }: { eventId: string; fallbackTierId?: string }) => {
      if (!sessionPartyId) throw new Error('Inicia sesión para comprar tickets.');
      const draft = ticketPurchases[eventId] ?? {
        tierId: fallbackTierId ?? '',
        quantity: '1',
        buyerName: '',
        buyerEmail: '',
      };
      const quantity = parseOptionalUnsignedInt(draft.quantity);
      if (!draft.tierId) throw new Error('Selecciona un tipo de ticket.');
      if (quantity === null || quantity <= 0) throw new Error('Cantidad inválida.');
      return SocialEventsAPI.buyTickets(eventId, {
        ticketPurchaseTierId: draft.tierId,
        ticketPurchaseQuantity: quantity,
        ticketPurchaseBuyerName: draft.buyerName.trim() || null,
        ticketPurchaseBuyerEmail: draft.buyerEmail.trim() || null,
      });
    },
    onSuccess: (order, { eventId }) => {
      setTicketPurchases((prev) => ({ ...prev, [eventId]: { tierId: '', quantity: '1', buyerName: '', buyerEmail: '' } }));
      void qc.invalidateQueries({ queryKey: ['social-ticket-tiers', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-ticket-orders', eventId] });
      setFeedback({
        kind: 'success',
        message: `Compra registrada (${order.ticketOrderQuantity} ticket${order.ticketOrderQuantity > 1 ? 's' : ''}).`,
      });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const createTierMutation = useMutation({
    mutationFn: ({ eventId }: { eventId: string }) => {
      const draft = ticketTierForms[eventId] ?? {
        code: '',
        name: '',
        price: '',
        quantity: '',
        currency: 'USD',
      };
      const priceCents = parseOptionalUnsignedInt(draft.price);
      const quantity = parseOptionalUnsignedInt(draft.quantity);
      if (!draft.name.trim()) throw new Error('Nombre del ticket requerido.');
      if (priceCents === null) throw new Error('Precio inválido (usa centavos).');
      if (quantity === null || quantity <= 0) throw new Error('Cantidad inválida.');
      const payload: SocialTicketTierDTO = {
        ticketTierCode: draft.code.trim() || draft.name,
        ticketTierName: draft.name.trim(),
        ticketTierDescription: null,
        ticketTierPriceCents: priceCents,
        ticketTierCurrency: draft.currency.trim().toUpperCase() || 'USD',
        ticketTierQuantityTotal: quantity,
        ticketTierQuantitySold: 0,
        ticketTierSalesStart: null,
        ticketTierSalesEnd: null,
        ticketTierActive: true,
        ticketTierPosition: null,
      };
      return SocialEventsAPI.createTicketTier(eventId, payload);
    },
    onSuccess: (_resp, { eventId }) => {
      setTicketTierForms((prev) => ({
        ...prev,
        [eventId]: { code: '', name: '', price: '', quantity: '', currency: 'USD' },
      }));
      void qc.invalidateQueries({ queryKey: ['social-ticket-tiers', eventId] });
      setFeedback({ kind: 'success', message: 'Tipo de ticket creado.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const updateOrderStatusMutation = useMutation({
    mutationFn: ({ eventId, orderId, status }: { eventId: string; orderId: string; status: 'cancelled' | 'refunded' }) =>
      SocialEventsAPI.updateTicketOrderStatus(eventId, orderId, status),
    onSuccess: (_resp, { eventId }) => {
      void qc.invalidateQueries({ queryKey: ['social-ticket-orders', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-ticket-tiers', eventId] });
      setFeedback({ kind: 'success', message: 'Estado de orden actualizado.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const checkInMutation = useMutation({
    mutationFn: ({ eventId }: { eventId: string }) => {
      const code = (checkInCodes[eventId] ?? '').trim();
      if (!code) throw new Error('Ingresa el código del ticket para check-in.');
      return SocialEventsAPI.checkInTicket(eventId, { ticketCheckInTicketCode: code });
    },
    onSuccess: (_resp, { eventId }) => {
      setCheckInCodes((prev) => ({ ...prev, [eventId]: '' }));
      void qc.invalidateQueries({ queryKey: ['social-ticket-orders', eventId] });
      setFeedback({ kind: 'success', message: 'Ticket marcado como check-in.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const uploadEventImageMutation = useMutation({
    mutationFn: ({ eventId, file }: { eventId: string; file: File }) =>
      SocialEventsAPI.uploadEventImage(eventId, file, file.name),
    onSuccess: (_resp, { eventId }) => {
      setEventPosterFiles((prev) => ({ ...prev, [eventId]: null }));
      void qc.invalidateQueries({ queryKey: ['social-events'] });
      setFeedback({ kind: 'success', message: 'Afiche subido y asociado al evento.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const createBudgetLineMutation = useMutation({
    mutationFn: ({ eventId }: { eventId: string }) => {
      const draft = budgetLineForms[eventId] ?? {
        code: '',
        name: '',
        type: 'expense',
        category: 'general',
        plannedCents: '',
        notes: '',
      };
      const plannedCents = parseOptionalUnsignedInt(draft.plannedCents);
      if (!draft.name.trim()) throw new Error('Nombre de línea presupuestaria requerido.');
      if (plannedCents === null) throw new Error('Presupuesto inválido (usa centavos).');
      const payload: SocialEventBudgetLineDTO = {
        eblCode: draft.code.trim() || draft.name,
        eblName: draft.name.trim(),
        eblType: draft.type,
        eblCategory: draft.category.trim() || 'general',
        eblPlannedCents: plannedCents,
        eblActualCents: null,
        eblNotes: draft.notes.trim() || null,
      };
      return SocialEventsAPI.createBudgetLine(eventId, payload);
    },
    onSuccess: (_line, { eventId }) => {
      setBudgetLineForms((prev) => ({
        ...prev,
        [eventId]: { code: '', name: '', type: 'expense', category: 'general', plannedCents: '', notes: '' },
      }));
      void qc.invalidateQueries({ queryKey: ['social-budget-lines', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-finance-summary', eventId] });
      setFeedback({ kind: 'success', message: 'Línea presupuestaria creada.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const seedBudgetTemplateMutation = useMutation({
    mutationFn: async ({ eventId, existingLines }: { eventId: string; existingLines: SocialEventBudgetLineDTO[] }) => {
      const existingCodes = new Set(
        existingLines.map((line) => String(line.eblCode ?? '').trim().toUpperCase()).filter(Boolean),
      );
      const missingLines = PROJECT_BUDGET_TEMPLATE.filter((line) => !existingCodes.has(line.code));
      for (const line of missingLines) {
        await SocialEventsAPI.createBudgetLine(eventId, {
          eblCode: line.code,
          eblName: line.name,
          eblType: line.type,
          eblCategory: line.category,
          eblPlannedCents: 0,
          eblActualCents: null,
          eblNotes: 'Creado automáticamente desde plantilla PM.',
        });
      }
      return missingLines.length;
    },
    onSuccess: (createdCount, { eventId }) => {
      void qc.invalidateQueries({ queryKey: ['social-budget-lines', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-finance-summary', eventId] });
      setFeedback({
        kind: 'success',
        message:
          createdCount > 0
            ? `Plantilla PM aplicada (${createdCount} líneas nuevas).`
            : 'La plantilla PM ya estaba aplicada.',
      });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const createContractCommitmentMutation = useMutation({
    mutationFn: async ({ eventId }: { eventId: string }) => {
      const draft = contractDrafts[eventId] ?? {
        kind: 'event_vendor_contract',
        counterparty: '',
        concept: '',
        amountCents: '',
        currency: 'USD',
        status: 'pending' as const,
        notes: '',
      };
      const amountCents = parseOptionalUnsignedInt(draft.amountCents);
      if (!draft.concept.trim()) throw new Error('Concepto de contrato requerido.');
      if (amountCents === null || amountCents <= 0) throw new Error('Monto de contrato inválido (usa centavos).');
      const currency = draft.currency.trim().toUpperCase() || 'USD';
      const contractResponse = await ContractsAPI.create({
        kind: draft.kind.trim() || 'event_vendor_contract',
        eventId,
        counterparty: draft.counterparty.trim() || null,
        concept: draft.concept.trim(),
        amountCents,
        currency,
        notes: draft.notes.trim() || null,
        metadata: {
          module: 'social_events',
          flow: 'project_contracting',
          createdAt: new Date().toISOString(),
        },
      });
      await SocialEventsAPI.createFinanceEntry(eventId, {
        efeBudgetLineId: null,
        efeDirection: 'expense',
        efeSource: 'contract_commitment',
        efeCategory: 'contracting',
        efeConcept: draft.concept.trim(),
        efeAmountCents: amountCents,
        efeCurrency: currency,
        efeStatus: draft.status,
        efeExternalRef: contractResponse.id,
        efeNotes: draft.notes.trim() || null,
        efeOccurredAt: new Date().toISOString(),
      });
      return contractResponse.id;
    },
    onSuccess: (contractId, { eventId }) => {
      setContractDrafts((prev) => ({
        ...prev,
        [eventId]: {
          kind: 'event_vendor_contract',
          counterparty: '',
          concept: '',
          amountCents: '',
          currency: 'USD',
          status: 'pending',
          notes: '',
        },
      }));
      void qc.invalidateQueries({ queryKey: ['social-finance-entries', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-budget-lines', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-finance-summary', eventId] });
      setFeedback({
        kind: 'success',
        message: `Contrato creado y compromiso registrado (ID ${contractId}).`,
      });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const downloadContractPdfMutation = useMutation({
    mutationFn: ({ contractId }: { contractId: string }) => ContractsAPI.downloadPdf(contractId),
    onSuccess: (blob, { contractId }) => {
      const url = URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.href = url;
      link.download = `contrato-${contractId}.pdf`;
      link.click();
      URL.revokeObjectURL(url);
      setFeedback({ kind: 'success', message: `PDF de contrato ${contractId} descargado.` });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const createFinanceEntryMutation = useMutation({
    mutationFn: ({ eventId }: { eventId: string }) => {
      const draft = financeEntryForms[eventId] ?? {
        budgetLineId: '',
        direction: 'expense',
        source: 'manual',
        category: 'general',
        concept: '',
        amountCents: '',
        currency: 'USD',
        status: 'posted',
        externalRef: '',
        notes: '',
        occurredAt: toDateTimeInputValue(),
      };
      const amountCents = parseOptionalUnsignedInt(draft.amountCents);
      if (!draft.concept.trim()) throw new Error('Concepto contable requerido.');
      if (amountCents === null || amountCents <= 0) throw new Error('Monto inválido (usa centavos).');
      const payload: SocialEventFinanceEntryDTO = {
        efeBudgetLineId: draft.budgetLineId.trim() || null,
        efeDirection: draft.direction,
        efeSource: draft.source.trim() || 'manual',
        efeCategory: draft.category.trim() || 'general',
        efeConcept: draft.concept.trim(),
        efeAmountCents: amountCents,
        efeCurrency: draft.currency.trim().toUpperCase() || 'USD',
        efeStatus: draft.status,
        efeExternalRef: draft.externalRef.trim() || null,
        efeNotes: draft.notes.trim() || null,
        efeOccurredAt: toUtcIso(draft.occurredAt),
      };
      return SocialEventsAPI.createFinanceEntry(eventId, payload);
    },
    onSuccess: (_entry, { eventId }) => {
      setFinanceEntryForms((prev) => ({
        ...prev,
        [eventId]: {
          budgetLineId: '',
          direction: 'expense',
          source: 'manual',
          category: 'general',
          concept: '',
          amountCents: '',
          currency: 'USD',
          status: 'posted',
          externalRef: '',
          notes: '',
          occurredAt: toDateTimeInputValue(),
        },
      }));
      void qc.invalidateQueries({ queryKey: ['social-finance-entries', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-budget-lines', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-finance-summary', eventId] });
      setFeedback({ kind: 'success', message: 'Asiento contable registrado.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const exportEventFinanceCsv = ({
    event,
    budgetLines,
    financeEntries,
    financeSummary,
  }: {
    event: SocialEventDTO;
    budgetLines: SocialEventBudgetLineDTO[];
    financeEntries: SocialEventFinanceEntryDTO[];
    financeSummary: SocialEventFinanceSummaryDTO | null;
  }) => {
    const eventId = event.eventId ? String(event.eventId) : '';
    if (!eventId) return;
    const rows = buildEventFinanceCsvRows(event, budgetLines, financeEntries, financeSummary);
    const safeTitle = event.eventTitle.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/(^-|-$)/g, '');
    const filename = `reporte-financiero-${safeTitle || 'evento'}-${eventId}-${Date.now()}.csv`;
    downloadCsvFile(filename, rows);
    setFeedback({ kind: 'success', message: `Reporte CSV exportado para ${event.eventTitle}.` });
  };

  const renderOrders = (orders: SocialTicketOrderDTO[], eventId: string, isOrganizer: boolean) => {
    if (orders.length === 0) {
      return (
        <Typography variant="body2" color="text.secondary">
          Sin órdenes registradas.
        </Typography>
      );
    }
    return (
      <Stack spacing={1}>
        {orders.map((order) => {
          const oid = order.ticketOrderId ?? '';
          return (
            <Box key={oid || `${eventId}-${order.ticketOrderPurchasedAt}`} sx={{ border: '1px solid', borderColor: 'divider', borderRadius: 2, p: 1.25 }}>
              <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                <Chip size="small" icon={<SellIcon />} label={`${order.ticketOrderQuantity} ticket(s)`} />
                <Chip size="small" label={order.ticketOrderStatusValue} color={order.ticketOrderStatusValue === 'paid' ? 'success' : 'default'} />
                <Typography variant="body2" color="text.secondary">
                  {formatMoney(order.ticketOrderAmountCents, order.ticketOrderCurrency)}
                </Typography>
              </Stack>
              {order.ticketOrderTickets.length > 0 && (
                <Stack direction="row" spacing={0.75} flexWrap="wrap" sx={{ mt: 1 }}>
                  {order.ticketOrderTickets.map((ticket) => (
                    <Chip
                      key={ticket.ticketId ?? ticket.ticketCode}
                      size="small"
                      icon={<ConfirmationNumberIcon />}
                      label={`${ticket.ticketCode} · ${ticket.ticketStatus}`}
                      variant="outlined"
                    />
                  ))}
                </Stack>
              )}
              {isOrganizer && order.ticketOrderStatusValue === 'paid' && oid && (
                <Stack direction="row" spacing={1} sx={{ mt: 1 }}>
                  <Button
                    size="small"
                    variant="outlined"
                    color="warning"
                    onClick={() => updateOrderStatusMutation.mutate({ eventId, orderId: oid, status: 'cancelled' })}
                    disabled={updateOrderStatusMutation.isPending}
                  >
                    Cancelar
                  </Button>
                  <Button
                    size="small"
                    variant="outlined"
                    color="error"
                    onClick={() => updateOrderStatusMutation.mutate({ eventId, orderId: oid, status: 'refunded' })}
                    disabled={updateOrderStatusMutation.isPending}
                  >
                    Reembolsar
                  </Button>
                </Stack>
              )}
            </Box>
          );
        })}
      </Stack>
    );
  };

  return (
    <Box>
      <Stack spacing={2} sx={{ mb: 3 }}>
        <Stack direction="row" alignItems="center" spacing={1}>
          <Typography variant="h4" fontWeight={800}>Eventos sociales</Typography>
          <Chip label="Beta" size="small" color="info" />
        </Stack>
        <Typography color="text.secondary">
          Descubre eventos, confirma asistencia y gestiona operación completa: tickets, presupuesto y contabilidad por evento.
        </Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'stretch', sm: 'center' }}>
          <TextField
            label="Filtrar por ciudad"
            size="small"
            value={city}
            onChange={(e) => setCity(e.target.value)}
            sx={{ minWidth: 220 }}
          />
          <TextField
            select
            label="Tipo"
            size="small"
            value={eventTypeFilter}
            onChange={(e) => setEventTypeFilter(e.target.value)}
            sx={{ minWidth: 150 }}
          >
            <MenuItem value="">Todos</MenuItem>
            <MenuItem value="party">Party</MenuItem>
            <MenuItem value="concert">Concert</MenuItem>
            <MenuItem value="festival">Festival</MenuItem>
            <MenuItem value="showcase">Showcase</MenuItem>
          </TextField>
          <TextField
            select
            label="Estado"
            size="small"
            value={eventStatusFilter}
            onChange={(e) => setEventStatusFilter(e.target.value)}
            sx={{ minWidth: 150 }}
          >
            <MenuItem value="">Todos</MenuItem>
            <MenuItem value="planning">Planning</MenuItem>
            <MenuItem value="announced">Announced</MenuItem>
            <MenuItem value="on_sale">On Sale</MenuItem>
            <MenuItem value="live">Live</MenuItem>
            <MenuItem value="completed">Completed</MenuItem>
            <MenuItem value="cancelled">Cancelled</MenuItem>
          </TextField>
          <Button
            variant="outlined"
            startIcon={<RefreshIcon />}
            onClick={() => {
              void eventsQuery.refetch();
              void venuesQuery.refetch();
            }}
            disabled={eventsQuery.isFetching}
          >
            Refrescar
          </Button>
          {!hasSession && (
            <Alert severity="info" sx={{ m: 0, py: 0.5 }}>
              Inicia sesión para RSVP, invitaciones, tickets y gestión financiera.
            </Alert>
          )}
        </Stack>
        {hasSession ? (
          <Card variant="outlined">
            <CardContent>
              <Stack spacing={1.5}>
                <Typography variant="h6" fontWeight={700}>Create Event</Typography>
                <Typography variant="body2" color="text.secondary">
                  Any logged in user can create events. Once created, events appear in the Event Calendar section.
                </Typography>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                  <TextField
                    label="Título"
                    size="small"
                    value={eventDraft.title}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, title: e.target.value }))}
                    sx={{ flex: 2 }}
                  />
                  <TextField
                    label="Venue"
                    size="small"
                    select
                    value={eventDraft.venueId}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, venueId: e.target.value }))}
                    sx={{ flex: 1 }}
                  >
                    <MenuItem value="">Sin venue</MenuItem>
                    {(venuesQuery.data ?? []).map((venue) => (
                      <MenuItem key={venue.venueId ?? venue.venueName} value={venue.venueId ?? ''}>
                        {venue.venueName}
                      </MenuItem>
                    ))}
                  </TextField>
                  <TextField
                    select
                    label="Tipo"
                    size="small"
                    value={eventDraft.eventType}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, eventType: e.target.value }))}
                    sx={{ width: 150 }}
                  >
                    <MenuItem value="party">Party</MenuItem>
                    <MenuItem value="concert">Concert</MenuItem>
                    <MenuItem value="festival">Festival</MenuItem>
                    <MenuItem value="showcase">Showcase</MenuItem>
                  </TextField>
                  <TextField
                    select
                    label="Estado"
                    size="small"
                    value={eventDraft.eventStatus}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, eventStatus: e.target.value }))}
                    sx={{ width: 150 }}
                  >
                    <MenuItem value="planning">Planning</MenuItem>
                    <MenuItem value="announced">Announced</MenuItem>
                    <MenuItem value="on_sale">On Sale</MenuItem>
                    <MenuItem value="live">Live</MenuItem>
                    <MenuItem value="completed">Completed</MenuItem>
                    <MenuItem value="cancelled">Cancelled</MenuItem>
                  </TextField>
                </Stack>
                <TextField
                  label="Descripción"
                  size="small"
                  value={eventDraft.description}
                  onChange={(e) => setEventDraft((prev) => ({ ...prev, description: e.target.value }))}
                  multiline
                  minRows={2}
                />
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                  <TextField
                    label="Inicio"
                    size="small"
                    type="datetime-local"
                    value={eventDraft.startAt}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, startAt: e.target.value }))}
                    InputLabelProps={{ shrink: true }}
                    sx={{ minWidth: 220 }}
                  />
                  <TextField
                    label="Fin"
                    size="small"
                    type="datetime-local"
                    value={eventDraft.endAt}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, endAt: e.target.value }))}
                    InputLabelProps={{ shrink: true }}
                    sx={{ minWidth: 220 }}
                  />
                  <TextField
                    label="Precio (centavos)"
                    size="small"
                    type="number"
                    value={eventDraft.priceCents}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, priceCents: e.target.value }))}
                    sx={{ width: 170 }}
                  />
                  <TextField
                    label="Capacidad"
                    size="small"
                    type="number"
                    value={eventDraft.capacity}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, capacity: e.target.value }))}
                    sx={{ width: 130 }}
                  />
                  <TextField
                    label="Moneda"
                    size="small"
                    value={eventDraft.currency}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, currency: e.target.value.toUpperCase() }))}
                    sx={{ width: 110 }}
                  />
                </Stack>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={1} alignItems={{ md: 'center' }}>
                  <TextField
                    label="Ticket URL"
                    size="small"
                    value={eventDraft.ticketUrl}
                    onChange={(e) => setEventDraft((prev) => ({ ...prev, ticketUrl: e.target.value }))}
                    sx={{ flex: 1 }}
                  />
                  <Stack direction="row" spacing={0.5} alignItems="center">
                    <Checkbox
                      size="small"
                      checked={eventDraft.isPublic}
                      onChange={(_, checked) => setEventDraft((prev) => ({ ...prev, isPublic: checked }))}
                    />
                    <Typography variant="body2" color="text.secondary">Evento público</Typography>
                  </Stack>
                  <Button
                    variant="contained"
                    onClick={() => createEventMutation.mutate()}
                    disabled={createEventMutation.isPending || !hasSession}
                  >
                    Crear evento
                  </Button>
                </Stack>
              </Stack>
            </CardContent>
          </Card>
        ) : (
          <Alert severity="info">Inicia sesión para crear eventos.</Alert>
        )}
        <Card variant="outlined">
          <CardContent>
            <Stack spacing={1.5}>
              <Stack direction="row" alignItems="center" spacing={1} justifyContent="space-between">
                <Typography variant="h6" fontWeight={700}>Event Calendar</Typography>
                <Chip size="small" label={`${events.length} evento${events.length === 1 ? '' : 's'}`} />
              </Stack>
              <Grid container spacing={2}>
                <Grid item xs={12} md={5}>
                  <LocalizationProvider dateAdapter={AdapterLuxon}>
                    <DateCalendar
                      value={selectedCalendarValue}
                      onChange={(value: DateTime | null) => {
                        const nextDate = value?.toISODate();
                        if (!nextDate) return;
                        setSelectedCalendarDate(nextDate);
                      }}
                    />
                  </LocalizationProvider>
                </Grid>
                <Grid item xs={12} md={7}>
                  <Stack spacing={1}>
                    <Typography variant="subtitle2" fontWeight={700}>
                      {selectedCalendarLabel}
                    </Typography>
                    {selectedCalendarEvents.length === 0 ? (
                      <Typography variant="body2" color="text.secondary">
                        Sin eventos programados para esta fecha.
                      </Typography>
                    ) : (
                      <Stack spacing={1}>
                        {selectedCalendarEvents.map((event) => (
                          <Box
                            key={event.eventId ?? `${event.eventTitle}-${event.eventStart}`}
                            sx={{
                              border: '1px solid',
                              borderColor: 'divider',
                              borderRadius: 2,
                              p: 1.25,
                            }}
                          >
                            <Typography variant="subtitle2" fontWeight={700}>
                              {event.eventTitle}
                            </Typography>
                            <Typography variant="caption" color="text.secondary">
                              {formatDate(event.eventStart)} - {formatDate(event.eventEnd)}
                            </Typography>
                            {event.eventVenueId && (
                              <Typography variant="caption" color="text.secondary" display="block">
                                {venueById.get(event.eventVenueId) ?? 'Venue por definir'}
                              </Typography>
                            )}
                          </Box>
                        ))}
                      </Stack>
                    )}
                    {calendarDateOptions.length === 0 && (
                      <Typography variant="caption" color="text.secondary">
                        Aún no hay eventos para mostrar en el calendario.
                      </Typography>
                    )}
                  </Stack>
                </Grid>
              </Grid>
            </Stack>
          </CardContent>
        </Card>
        {feedback && (
          <Alert severity={feedback.kind} onClose={() => setFeedback(null)}>
            {feedback.message}
          </Alert>
        )}
      </Stack>

      {eventsQuery.error ? (
        <Alert severity="error">No pudimos cargar los eventos. Intenta de nuevo.</Alert>
      ) : eventsQuery.isLoading || venuesQuery.isLoading ? (
        <Stack direction="row" spacing={1.5} alignItems="center">
          <CircularProgress size={18} />
          <Typography>Cargando eventos y venues...</Typography>
        </Stack>
      ) : events.length === 0 ? (
        <Alert severity="info">No hay eventos disponibles para este filtro.</Alert>
      ) : (
        <Grid container spacing={2}>
          {events.map((ev, index) => {
            const eventId = ev.eventId ? String(ev.eventId) : '';
            const isOrganizer =
              hasSession &&
              sessionPartyId != null &&
              ev.eventOrganizerPartyId != null &&
              String(ev.eventOrganizerPartyId) === sessionPartyId;
            const invitationForMe = invitationQueries[index]?.data?.[0] ?? null;
            const tiers = ticketTierQueries[index]?.data ?? [];
            const orders = ticketOrderQueries[index]?.data ?? [];
            const budgetLines = budgetLineQueries[index]?.data ?? [];
            const financeEntries = financeEntryQueries[index]?.data ?? [];
            const financeSummary = financeSummaryQueries[index]?.data ?? null;
            const inviteDraft = invites[eventId] ?? { partyId: '', message: '' };
            const purchaseDraft = ticketPurchases[eventId] ?? {
              tierId: tiers[0]?.ticketTierId ?? '',
              quantity: '1',
              buyerName: '',
              buyerEmail: '',
            };
            const tierDraft = ticketTierForms[eventId] ?? { code: '', name: '', price: '', quantity: '', currency: 'USD' };
            const budgetDraft = budgetLineForms[eventId] ?? {
              code: '',
              name: '',
              type: 'expense' as const,
              category: 'general',
              plannedCents: '',
              notes: '',
            };
            const financeDraft = financeEntryForms[eventId] ?? {
              budgetLineId: '',
              direction: 'expense' as const,
              source: 'manual',
              category: 'general',
              concept: '',
              amountCents: '',
              currency: ev.eventCurrency ?? 'USD',
              status: 'posted' as const,
              externalRef: '',
              notes: '',
              occurredAt: toDateTimeInputValue(),
            };
            const contractDraft = contractDrafts[eventId] ?? {
              kind: 'event_vendor_contract',
              counterparty: '',
              concept: '',
              amountCents: '',
              currency: ev.eventCurrency ?? 'USD',
              status: 'pending' as const,
              notes: '',
            };
            const contractRefs = Array.from(
              new Set(
                financeEntries
                  .filter(
                    (entry) =>
                      (entry.efeSource === 'contract_commitment' || entry.efeSource === 'contract_payment')
                      && Boolean(entry.efeExternalRef),
                  )
                  .map((entry) => String(entry.efeExternalRef)),
              ),
            );
            const checkInCode = checkInCodes[eventId] ?? '';
            const posterFile = eventPosterFiles[eventId] ?? null;

            return (
              <Grid key={eventId || `${ev.eventTitle}-${ev.eventStart}`} item xs={12} md={6}>
                <Card sx={{ height: '100%', display: 'flex', flexDirection: 'column', borderRadius: 3 }}>
                  <CardContent sx={{ display: 'flex', flexDirection: 'column', gap: 1.5, flex: 1 }}>
                    <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between" flexWrap="wrap">
                      <Stack direction="row" spacing={1} alignItems="center">
                        <Chip icon={<CalendarMonthIcon />} label={formatDate(ev.eventStart)} size="small" />
                        <Typography variant="caption" color="text.secondary">
                          Termina {formatDate(ev.eventEnd)}
                        </Typography>
                      </Stack>
                      <Stack direction="row" spacing={1} flexWrap="wrap">
                        {ev.eventCapacity ? <Chip label={`Cupo: ${ev.eventCapacity}`} size="small" /> : null}
                        {ev.eventType ? <Chip label={`Tipo: ${ev.eventType}`} size="small" color="secondary" variant="outlined" /> : null}
                        {ev.eventStatus ? <Chip label={`Estado: ${ev.eventStatus}`} size="small" variant="outlined" /> : null}
                      </Stack>
                    </Stack>

                    <Typography variant="h6" fontWeight={800}>{ev.eventTitle}</Typography>
                    {ev.eventVenueId && (
                      <Typography variant="body2" color="text.secondary">
                        {venueById.get(ev.eventVenueId) ?? 'Venue por definir'}
                      </Typography>
                    )}
                    {ev.eventArtists?.length ? (
                      <Typography variant="body2" color="text.secondary">
                        Artistas: {ev.eventArtists.map((a) => a.artistName).join(', ')}
                      </Typography>
                    ) : null}
                    {typeof ev.eventPriceCents === 'number' && (
                      <Typography variant="body2" color="text.secondary">
                        Cover referencial: {formatMoney(ev.eventPriceCents, ev.eventCurrency ?? 'USD')}
                      </Typography>
                    )}
                    {typeof ev.eventBudgetCents === 'number' && (
                      <Typography variant="body2" color="text.secondary">
                        Presupuesto general: {formatMoney(ev.eventBudgetCents, ev.eventCurrency ?? 'USD')}
                      </Typography>
                    )}
                    {ev.eventDescription && (
                      <Typography variant="body2" color="text.secondary">
                        {ev.eventDescription}
                      </Typography>
                    )}
                    {ev.eventImageUrl && (
                      <Box
                        component="img"
                        src={ev.eventImageUrl}
                        alt={`Afiche de ${ev.eventTitle}`}
                        sx={{
                          width: '100%',
                          maxHeight: 280,
                          objectFit: 'cover',
                          borderRadius: 2,
                          border: '1px solid',
                          borderColor: 'divider',
                        }}
                      />
                    )}
                    {isOrganizer && eventId && (
                      <Stack spacing={1}>
                        <Typography variant="subtitle2" fontWeight={700}>Afiche</Typography>
                        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
                          <Button
                            component="label"
                            size="small"
                            variant="outlined"
                          >
                            Seleccionar imagen
                            <input
                              hidden
                              type="file"
                              accept="image/*"
                              onChange={(e) => {
                                const file = e.target.files?.[0] ?? null;
                                setEventPosterFiles((prev) => ({ ...prev, [eventId]: file }));
                                e.currentTarget.value = '';
                              }}
                            />
                          </Button>
                          <Typography variant="caption" color="text.secondary" sx={{ flex: 1 }}>
                            {posterFile ? posterFile.name : ev.eventImageUrl ? 'Afiche actual cargado' : 'Sin afiche cargado'}
                          </Typography>
                          <Button
                            size="small"
                            variant="contained"
                            onClick={() => {
                              if (!posterFile) return;
                              uploadEventImageMutation.mutate({ eventId, file: posterFile });
                            }}
                            disabled={!posterFile || uploadEventImageMutation.isPending}
                          >
                            Subir afiche
                          </Button>
                        </Stack>
                      </Stack>
                    )}

                    {invitationForMe?.invitationStatus && (
                      <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                        <Chip label={`Invitación: ${invitationForMe.invitationStatus}`} size="small" color="secondary" sx={{ width: 'fit-content' }} />
                        {invitationForMe.invitationId && eventId && (
                          <>
                            <Button
                              variant="contained"
                              size="small"
                              onClick={() => respondInvitationMutation.mutate({ eventId, invitationId: invitationForMe.invitationId!, status: 'Accepted' })}
                              disabled={respondInvitationMutation.isPending}
                            >
                              Aceptar
                            </Button>
                            <Button
                              variant="text"
                              size="small"
                              onClick={() => respondInvitationMutation.mutate({ eventId, invitationId: invitationForMe.invitationId!, status: 'Declined' })}
                              disabled={respondInvitationMutation.isPending}
                            >
                              Rechazar
                            </Button>
                          </>
                        )}
                      </Stack>
                    )}

                    <Stack direction="row" spacing={1} flexWrap="wrap">
                      <Button
                        variant="contained"
                        size="small"
                        startIcon={<CheckIcon />}
                        onClick={() => eventId && rsvpMutation.mutate({ eventId, status: 'Accepted' })}
                        disabled={rsvpMutation.isPending || !hasSession || !eventId}
                      >
                        Asistiré
                      </Button>
                      <Button
                        variant="outlined"
                        size="small"
                        startIcon={<HelpOutlineIcon />}
                        onClick={() => eventId && rsvpMutation.mutate({ eventId, status: 'Maybe' })}
                        disabled={rsvpMutation.isPending || !hasSession || !eventId}
                      >
                        Tal vez
                      </Button>
                      <Button
                        variant="text"
                        size="small"
                        startIcon={<ClearIcon />}
                        onClick={() => eventId && rsvpMutation.mutate({ eventId, status: 'Declined' })}
                        disabled={rsvpMutation.isPending || !hasSession || !eventId}
                      >
                        No puedo
                      </Button>
                    </Stack>

                    <Divider />

                    <Stack spacing={1}>
                      <Typography variant="subtitle2" fontWeight={700}>Invitar</Typography>
                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                        <TextField
                          label="Party ID"
                          size="small"
                          value={inviteDraft.partyId}
                          onChange={(e) => setInvites((prev) => ({ ...prev, [eventId]: { ...inviteDraft, partyId: e.target.value } }))}
                          sx={{ flex: 1 }}
                        />
                        <TextField
                          label="Mensaje (opcional)"
                          size="small"
                          value={inviteDraft.message}
                          onChange={(e) => setInvites((prev) => ({ ...prev, [eventId]: { ...inviteDraft, message: e.target.value } }))}
                          sx={{ flex: 2 }}
                        />
                        <Button
                          variant="contained"
                          size="small"
                          startIcon={<PersonAddAltIcon />}
                          onClick={() => eventId && inviteMutation.mutate({ eventId })}
                          disabled={inviteMutation.isPending || !hasSession || !eventId}
                        >
                          Enviar
                        </Button>
                      </Stack>
                    </Stack>

                    <Divider />

                    <Stack spacing={1}>
                      <Typography variant="subtitle2" fontWeight={700}>Tickets</Typography>
                      {ticketTierQueries[index]?.error ? (
                        <Alert severity="warning">No se pudieron cargar los tipos de ticket.</Alert>
                      ) : tiers.length === 0 ? (
                        <Typography variant="body2" color="text.secondary">Aún no hay tipos de ticket para este evento.</Typography>
                      ) : (
                        <Stack direction="row" spacing={1} flexWrap="wrap">
                          {tiers.map((tier) => (
                            <Chip
                              key={tier.ticketTierId ?? tier.ticketTierCode}
                              icon={<ConfirmationNumberIcon />}
                              label={`${tier.ticketTierName} · ${formatMoney(tier.ticketTierPriceCents, tier.ticketTierCurrency)} · Disp: ${tierAvailability(tier)}`}
                              size="small"
                              variant={tier.ticketTierActive ? 'filled' : 'outlined'}
                            />
                          ))}
                        </Stack>
                      )}

                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                        <TextField
                          select
                          label="Tipo"
                          size="small"
                          value={purchaseDraft.tierId}
                          onChange={(e) => setTicketPurchases((prev) => ({
                            ...prev,
                            [eventId]: { ...purchaseDraft, tierId: e.target.value },
                          }))}
                          sx={{ minWidth: 180 }}
                        >
                          {tiers.map((tier) => (
                            <MenuItem
                              key={tier.ticketTierId ?? tier.ticketTierCode}
                              value={tier.ticketTierId ?? ''}
                              disabled={!tier.ticketTierActive || tierAvailability(tier) <= 0}
                            >
                              {tier.ticketTierName} ({formatMoney(tier.ticketTierPriceCents, tier.ticketTierCurrency)})
                            </MenuItem>
                          ))}
                        </TextField>
                        <TextField
                          label="Cantidad"
                          size="small"
                          type="number"
                          value={purchaseDraft.quantity}
                          onChange={(e) => setTicketPurchases((prev) => ({
                            ...prev,
                            [eventId]: { ...purchaseDraft, quantity: e.target.value },
                          }))}
                          sx={{ width: 110 }}
                        />
                        <TextField
                          label="Nombre comprador"
                          size="small"
                          value={purchaseDraft.buyerName}
                          onChange={(e) => setTicketPurchases((prev) => ({
                            ...prev,
                            [eventId]: { ...purchaseDraft, buyerName: e.target.value },
                          }))}
                          sx={{ flex: 1 }}
                        />
                        <TextField
                          label="Email comprador"
                          size="small"
                          value={purchaseDraft.buyerEmail}
                          onChange={(e) => setTicketPurchases((prev) => ({
                            ...prev,
                            [eventId]: { ...purchaseDraft, buyerEmail: e.target.value },
                          }))}
                          sx={{ flex: 1 }}
                        />
                        <Button
                          variant="contained"
                          size="small"
                          startIcon={<SellIcon />}
                          onClick={() => eventId && purchaseTicketsMutation.mutate({ eventId, fallbackTierId: tiers[0]?.ticketTierId ?? undefined })}
                          disabled={purchaseTicketsMutation.isPending || !hasSession || !eventId || tiers.length === 0}
                        >
                          Comprar
                        </Button>
                      </Stack>
                    </Stack>

                    <Divider />

                    <Stack spacing={1}>
                      <Typography variant="subtitle2" fontWeight={700}>
                        {isOrganizer ? 'Órdenes del evento' : 'Mis órdenes'}
                      </Typography>
                      {ticketOrderQueries[index]?.isLoading ? (
                        <Typography variant="body2" color="text.secondary">Cargando órdenes...</Typography>
                      ) : ticketOrderQueries[index]?.error ? (
                        <Alert severity="warning">No se pudieron cargar las órdenes de tickets.</Alert>
                      ) : (
                        renderOrders(orders, eventId, isOrganizer)
                      )}
                    </Stack>

                    {isOrganizer && (
                      <>
                        <Divider />
                        <Stack spacing={1}>
                          <Typography variant="subtitle2" fontWeight={700}>Gestión de tickets (organizador)</Typography>
                          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                            <TextField
                              label="Nombre"
                              size="small"
                              value={tierDraft.name}
                              onChange={(e) => setTicketTierForms((prev) => ({
                                ...prev,
                                [eventId]: { ...tierDraft, name: e.target.value },
                              }))}
                              sx={{ flex: 1 }}
                            />
                            <TextField
                              label="Código"
                              size="small"
                              value={tierDraft.code}
                              onChange={(e) => setTicketTierForms((prev) => ({
                                ...prev,
                                [eventId]: { ...tierDraft, code: e.target.value },
                              }))}
                              sx={{ width: 140 }}
                            />
                            <TextField
                              label="Precio (centavos)"
                              size="small"
                              type="number"
                              value={tierDraft.price}
                              onChange={(e) => setTicketTierForms((prev) => ({
                                ...prev,
                                [eventId]: { ...tierDraft, price: e.target.value },
                              }))}
                              sx={{ width: 150 }}
                            />
                            <TextField
                              label="Cantidad"
                              size="small"
                              type="number"
                              value={tierDraft.quantity}
                              onChange={(e) => setTicketTierForms((prev) => ({
                                ...prev,
                                [eventId]: { ...tierDraft, quantity: e.target.value },
                              }))}
                              sx={{ width: 120 }}
                            />
                            <TextField
                              label="Moneda"
                              size="small"
                              value={tierDraft.currency}
                              onChange={(e) => setTicketTierForms((prev) => ({
                                ...prev,
                                [eventId]: { ...tierDraft, currency: e.target.value },
                              }))}
                              sx={{ width: 110 }}
                            />
                            <Button
                              variant="contained"
                              size="small"
                              startIcon={<ConfirmationNumberIcon />}
                              onClick={() => eventId && createTierMutation.mutate({ eventId })}
                              disabled={createTierMutation.isPending || !eventId}
                            >
                              Crear tier
                            </Button>
                          </Stack>

                          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                            <TextField
                              label="Código ticket para check-in"
                              size="small"
                              value={checkInCode}
                              onChange={(e) => setCheckInCodes((prev) => ({ ...prev, [eventId]: e.target.value }))}
                              sx={{ flex: 1 }}
                            />
                            <Button
                              variant="outlined"
                              size="small"
                              startIcon={<QrCodeScannerIcon />}
                              onClick={() => eventId && checkInMutation.mutate({ eventId })}
                              disabled={checkInMutation.isPending || !eventId}
                            >
                              Check-in
                            </Button>
                          </Stack>

                          <Divider />

                          <Stack spacing={1}>
                            <Typography variant="subtitle2" fontWeight={700}>Finanzas del evento</Typography>
                            {financeSummaryQueries[index]?.isLoading ? (
                              <Typography variant="body2" color="text.secondary">Calculando resumen financiero...</Typography>
                            ) : financeSummaryQueries[index]?.error ? (
                              <Alert severity="warning">No se pudo cargar el resumen financiero.</Alert>
                            ) : financeSummary ? (
                              <Stack spacing={1}>
                                <Stack direction="row" spacing={1} flexWrap="wrap">
                                  <Chip size="small" label={`Ingresos: ${formatMoney(financeSummary.efsActualIncomeCents, financeSummary.efsCurrency)}`} color="success" />
                                  <Chip size="small" label={`Gastos: ${formatMoney(financeSummary.efsActualExpenseCents, financeSummary.efsCurrency)}`} color="warning" />
                                  <Chip size="small" label={`Neto: ${formatMoney(financeSummary.efsNetCents, financeSummary.efsCurrency)}`} />
                                  <Chip size="small" label={`Utilización: ${formatPercent(financeSummary.efsBudgetUtilizationPct)}`} variant="outlined" />
                                  <Chip size="small" label={`CxP: ${formatMoney(financeSummary.efsAccountsPayableCents ?? 0, financeSummary.efsCurrency)}`} variant="outlined" />
                                  <Chip size="small" label={`CxC: ${formatMoney(financeSummary.efsAccountsReceivableCents ?? 0, financeSummary.efsCurrency)}`} variant="outlined" />
                                  <Chip size="small" label={`Contratos comprometidos: ${formatMoney(financeSummary.efsContractCommittedCents ?? 0, financeSummary.efsCurrency)}`} variant="outlined" />
                                  <Chip size="small" label={`Contratos pagados: ${formatMoney(financeSummary.efsContractPaidCents ?? 0, financeSummary.efsCurrency)}`} variant="outlined" />
                                  <Chip size="small" label={`Compras comprometidas: ${formatMoney(financeSummary.efsProcurementCommittedCents ?? 0, financeSummary.efsCurrency)}`} variant="outlined" />
                                  <Chip size="small" label={`Compras pagadas: ${formatMoney(financeSummary.efsProcurementPaidCents ?? 0, financeSummary.efsCurrency)}`} variant="outlined" />
                                  <Chip size="small" label={`Activos: ${formatMoney(financeSummary.efsAssetInvestmentCents ?? 0, financeSummary.efsCurrency)}`} variant="outlined" />
                                  <Chip size="small" label={`Pasivo neto: ${formatMoney(financeSummary.efsLiabilityBalanceCents ?? 0, financeSummary.efsCurrency)}`} variant="outlined" />
                                  <Chip size="small" label={`Tickets pagados: ${formatMoney(financeSummary.efsTicketPaidRevenueCents, financeSummary.efsCurrency)}`} variant="outlined" />
                                  <Chip size="small" label={`Tickets reembolsados: ${formatMoney(financeSummary.efsTicketRefundedRevenueCents, financeSummary.efsCurrency)}`} variant="outlined" />
                                </Stack>
                                <Stack direction="row" spacing={1}>
                                  <Button
                                    size="small"
                                    variant="outlined"
                                    onClick={() =>
                                      exportEventFinanceCsv({
                                        event: ev,
                                        budgetLines,
                                        financeEntries,
                                        financeSummary,
                                      })
                                    }
                                  >
                                    Exportar CSV
                                  </Button>
                                </Stack>
                              </Stack>
                            ) : null}

                            <Typography variant="body2" fontWeight={700}>Líneas de presupuesto</Typography>
                            {budgetLineQueries[index]?.error ? (
                              <Alert severity="warning">No se pudieron cargar las líneas presupuestarias.</Alert>
                            ) : budgetLines.length === 0 ? (
                              <Typography variant="body2" color="text.secondary">Aún no hay líneas de presupuesto.</Typography>
                            ) : (
                              <Stack spacing={0.75}>
                                {budgetLines.map((line) => (
                                  <Stack key={line.eblId ?? line.eblCode} direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                                    <Chip size="small" label={`${line.eblCode} · ${line.eblType}`} />
                                    <Typography variant="caption" color="text.secondary">
                                      Plan: {formatMoney(line.eblPlannedCents, ev.eventCurrency ?? 'USD')} · Real: {formatMoney(line.eblActualCents ?? 0, ev.eventCurrency ?? 'USD')}
                                    </Typography>
                                  </Stack>
                                ))}
                              </Stack>
                            )}

                            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                              <TextField
                                label="Nombre línea"
                                size="small"
                                value={budgetDraft.name}
                                onChange={(e) => setBudgetLineForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...budgetDraft, name: e.target.value },
                                }))}
                                sx={{ flex: 1 }}
                              />
                              <TextField
                                label="Código"
                                size="small"
                                value={budgetDraft.code}
                                onChange={(e) => setBudgetLineForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...budgetDraft, code: e.target.value },
                                }))}
                                sx={{ width: 130 }}
                              />
                              <TextField
                                select
                                label="Tipo"
                                size="small"
                                value={budgetDraft.type}
                                onChange={(e) => setBudgetLineForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...budgetDraft, type: e.target.value as 'income' | 'expense' },
                                }))}
                                sx={{ width: 120 }}
                              >
                                <MenuItem value="expense">Gasto</MenuItem>
                                <MenuItem value="income">Ingreso</MenuItem>
                              </TextField>
                              <TextField
                                label="Categoría"
                                select
                                size="small"
                                value={budgetDraft.category}
                                onChange={(e) => setBudgetLineForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...budgetDraft, category: e.target.value },
                                }))}
                                sx={{ width: 140 }}
                              >
                                {BUDGET_CATEGORY_OPTIONS.map((option) => (
                                  <MenuItem key={option} value={option}>
                                    {option}
                                  </MenuItem>
                                ))}
                              </TextField>
                              <TextField
                                label="Plan (centavos)"
                                size="small"
                                type="number"
                                value={budgetDraft.plannedCents}
                                onChange={(e) => setBudgetLineForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...budgetDraft, plannedCents: e.target.value },
                                }))}
                                sx={{ width: 150 }}
                              />
                              <Button
                                variant="outlined"
                                size="small"
                                onClick={() => eventId && createBudgetLineMutation.mutate({ eventId })}
                                disabled={createBudgetLineMutation.isPending || !eventId}
                              >
                                Agregar línea
                              </Button>
                              <Button
                                variant="text"
                                size="small"
                                onClick={() => eventId && seedBudgetTemplateMutation.mutate({ eventId, existingLines: budgetLines })}
                                disabled={seedBudgetTemplateMutation.isPending || !eventId}
                              >
                                Plantilla PM
                              </Button>
                            </Stack>

                            <Divider />

                            <Typography variant="body2" fontWeight={700}>Contratos y compromisos</Typography>
                            {contractRefs.length === 0 ? (
                              <Typography variant="body2" color="text.secondary">
                                Aún no hay contratos vinculados.
                              </Typography>
                            ) : (
                              <Stack spacing={0.75}>
                                {contractRefs.map((contractId) => (
                                  <Stack key={contractId} direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                                    <Chip size="small" label={`Contrato ${contractId}`} />
                                    <Button
                                      size="small"
                                      variant="outlined"
                                      onClick={() => downloadContractPdfMutation.mutate({ contractId })}
                                      disabled={downloadContractPdfMutation.isPending}
                                    >
                                      PDF
                                    </Button>
                                    <Button
                                      size="small"
                                      variant="text"
                                      onClick={() =>
                                        setFinanceEntryForms((prev) => ({
                                          ...prev,
                                          [eventId]: {
                                            ...financeDraft,
                                            source: 'contract_payment',
                                            direction: 'expense',
                                            category: 'contracting',
                                            externalRef: contractId,
                                            concept: financeDraft.concept || `Pago contrato ${contractId}`,
                                          },
                                        }))
                                      }
                                    >
                                      Registrar pago
                                    </Button>
                                  </Stack>
                                ))}
                              </Stack>
                            )}

                            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                              <TextField
                                label="Tipo de contrato"
                                size="small"
                                value={contractDraft.kind}
                                onChange={(e) =>
                                  setContractDrafts((prev) => ({
                                    ...prev,
                                    [eventId]: { ...contractDraft, kind: e.target.value },
                                  }))
                                }
                                sx={{ minWidth: 180 }}
                              />
                              <TextField
                                label="Contraparte"
                                size="small"
                                value={contractDraft.counterparty}
                                onChange={(e) =>
                                  setContractDrafts((prev) => ({
                                    ...prev,
                                    [eventId]: { ...contractDraft, counterparty: e.target.value },
                                  }))
                                }
                                sx={{ flex: 1 }}
                              />
                              <TextField
                                label="Concepto"
                                size="small"
                                value={contractDraft.concept}
                                onChange={(e) =>
                                  setContractDrafts((prev) => ({
                                    ...prev,
                                    [eventId]: { ...contractDraft, concept: e.target.value },
                                  }))
                                }
                                sx={{ flex: 1 }}
                              />
                            </Stack>

                            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                              <TextField
                                label="Monto (centavos)"
                                size="small"
                                type="number"
                                value={contractDraft.amountCents}
                                onChange={(e) =>
                                  setContractDrafts((prev) => ({
                                    ...prev,
                                    [eventId]: { ...contractDraft, amountCents: e.target.value },
                                  }))
                                }
                                sx={{ width: 170 }}
                              />
                              <TextField
                                label="Moneda"
                                size="small"
                                value={contractDraft.currency}
                                onChange={(e) =>
                                  setContractDrafts((prev) => ({
                                    ...prev,
                                    [eventId]: { ...contractDraft, currency: e.target.value.toUpperCase() },
                                  }))
                                }
                                sx={{ width: 110 }}
                              />
                              <TextField
                                select
                                label="Estado compromiso"
                                size="small"
                                value={contractDraft.status}
                                onChange={(e) =>
                                  setContractDrafts((prev) => ({
                                    ...prev,
                                    [eventId]: {
                                      ...contractDraft,
                                      status: e.target.value as ContractDraftState['status'],
                                    },
                                  }))
                                }
                                sx={{ width: 180 }}
                              >
                                <MenuItem value="pending">Pendiente</MenuItem>
                                <MenuItem value="draft">Borrador</MenuItem>
                                <MenuItem value="posted">Publicado</MenuItem>
                              </TextField>
                              <TextField
                                label="Notas"
                                size="small"
                                value={contractDraft.notes}
                                onChange={(e) =>
                                  setContractDrafts((prev) => ({
                                    ...prev,
                                    [eventId]: { ...contractDraft, notes: e.target.value },
                                  }))
                                }
                                sx={{ flex: 1 }}
                              />
                              <Button
                                variant="contained"
                                size="small"
                                onClick={() => eventId && createContractCommitmentMutation.mutate({ eventId })}
                                disabled={createContractCommitmentMutation.isPending || !eventId}
                              >
                                Crear contrato
                              </Button>
                            </Stack>

                            <Typography variant="body2" fontWeight={700}>Asientos contables</Typography>
                            {financeEntryQueries[index]?.error ? (
                              <Alert severity="warning">No se pudieron cargar los asientos contables.</Alert>
                            ) : financeEntries.length === 0 ? (
                              <Typography variant="body2" color="text.secondary">Sin movimientos aún.</Typography>
                            ) : (
                              <Stack spacing={0.75}>
                                {financeEntries.slice(0, 8).map((entry) => (
                                  <Stack key={entry.efeId ?? `${entry.efeConcept}-${entry.efeOccurredAt}`} direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                                    <Chip size="small" label={`${entry.efeDirection} · ${entry.efeSource}`} variant="outlined" />
                                    <Typography variant="caption" color="text.secondary">
                                      {formatDate(entry.efeOccurredAt)} · {entry.efeConcept} · {formatMoney(entry.efeAmountCents, entry.efeCurrency)} · {entry.efeStatus}
                                      {entry.efeExternalRef ? ` · Ref: ${entry.efeExternalRef}` : ''}
                                    </Typography>
                                  </Stack>
                                ))}
                              </Stack>
                            )}

                            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                              <TextField
                                select
                                label="Línea presupuesto"
                                size="small"
                                value={financeDraft.budgetLineId}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, budgetLineId: e.target.value },
                                }))}
                                sx={{ minWidth: 180 }}
                              >
                                <MenuItem value="">Sin línea</MenuItem>
                                {budgetLines.map((line) => (
                                  <MenuItem key={line.eblId ?? line.eblCode} value={line.eblId ?? ''}>
                                    {line.eblCode} · {line.eblName}
                                  </MenuItem>
                                ))}
                              </TextField>
                              <TextField
                                select
                                label="Fuente"
                                size="small"
                                value={financeDraft.source}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: {
                                    ...financeDraft,
                                    source: e.target.value,
                                    direction:
                                      FINANCE_SOURCE_OPTIONS.find((opt) => opt.value === e.target.value)?.direction ?? financeDraft.direction,
                                    category:
                                      FINANCE_SOURCE_OPTIONS.find((opt) => opt.value === e.target.value)?.category ?? financeDraft.category,
                                  },
                                }))}
                                sx={{ minWidth: 210 }}
                              >
                                {FINANCE_SOURCE_OPTIONS.map((option) => (
                                  <MenuItem key={option.value} value={option.value}>
                                    {option.label}
                                  </MenuItem>
                                ))}
                              </TextField>
                              <TextField
                                select
                                label="Dirección"
                                size="small"
                                value={financeDraft.direction}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, direction: e.target.value as 'income' | 'expense' },
                                }))}
                                sx={{ width: 130 }}
                              >
                                <MenuItem value="expense">Gasto</MenuItem>
                                <MenuItem value="income">Ingreso</MenuItem>
                              </TextField>
                              <TextField
                                select
                                label="Estado"
                                size="small"
                                value={financeDraft.status}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: {
                                    ...financeDraft,
                                    status: e.target.value as FinanceEntryFormState['status'],
                                  },
                                }))}
                                sx={{ width: 130 }}
                              >
                                {FINANCE_STATUS_OPTIONS.map((option) => (
                                  <MenuItem key={option.value} value={option.value}>
                                    {option.label}
                                  </MenuItem>
                                ))}
                              </TextField>
                            </Stack>
                            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                              <TextField
                                select
                                label="Categoría"
                                size="small"
                                value={financeDraft.category}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, category: e.target.value },
                                }))}
                                sx={{ minWidth: 150 }}
                              >
                                {BUDGET_CATEGORY_OPTIONS.map((option) => (
                                  <MenuItem key={option} value={option}>
                                    {option}
                                  </MenuItem>
                                ))}
                              </TextField>
                              <TextField
                                label="Concepto"
                                size="small"
                                value={financeDraft.concept}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, concept: e.target.value },
                                }))}
                                sx={{ flex: 1 }}
                              />
                              <TextField
                                label="Referencia externa"
                                size="small"
                                value={financeDraft.externalRef}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, externalRef: e.target.value },
                                }))}
                                sx={{ flex: 1 }}
                              />
                            </Stack>
                            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                              <TextField
                                label="Monto (centavos)"
                                size="small"
                                type="number"
                                value={financeDraft.amountCents}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, amountCents: e.target.value },
                                }))}
                                sx={{ width: 160 }}
                              />
                              <TextField
                                label="Moneda"
                                size="small"
                                value={financeDraft.currency}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, currency: e.target.value.toUpperCase() },
                                }))}
                                sx={{ width: 110 }}
                              />
                              <TextField
                                label="Ocurrió en"
                                size="small"
                                type="datetime-local"
                                value={financeDraft.occurredAt}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, occurredAt: e.target.value },
                                }))}
                                InputLabelProps={{ shrink: true }}
                                sx={{ minWidth: 210 }}
                              />
                              <TextField
                                label="Notas"
                                size="small"
                                value={financeDraft.notes}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, notes: e.target.value },
                                }))}
                                sx={{ flex: 1 }}
                              />
                              <Button
                                variant="contained"
                                size="small"
                                onClick={() => eventId && createFinanceEntryMutation.mutate({ eventId })}
                                disabled={createFinanceEntryMutation.isPending || !eventId}
                              >
                                Registrar
                              </Button>
                            </Stack>
                          </Stack>
                        </Stack>
                      </>
                    )}
                  </CardContent>
                </Card>
              </Grid>
            );
          })}
        </Grid>
      )}
    </Box>
  );
}
