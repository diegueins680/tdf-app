import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient, useQueries } from '@tanstack/react-query';
import {
  Alert,
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
import { DateTime } from 'luxon';
import {
  SocialEventsAPI,
  type SocialEventBudgetLineDTO,
  type SocialEventFinanceEntryDTO,
  type SocialEventFinanceSummaryDTO,
  type SocialInvitationDTO,
  type SocialRsvpStatus,
  type SocialTicketOrderDTO,
  type SocialTicketTierDTO,
} from '../api/socialEvents';
import { useSession } from '../session/SessionContext';

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
  status: 'draft' | 'posted';
  notes: string;
  occurredAt: string;
}

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
  const [checkInCodes, setCheckInCodes] = useState<Record<string, string>>({});
  const startAfter = useMemo(() => new Date().toISOString(), []);
  const sessionPartyId = session?.partyId != null ? String(session.partyId) : null;
  const hasSession = Boolean(sessionPartyId);

  const eventsQuery = useQuery({
    queryKey: ['social-events', city, eventTypeFilter, eventStatusFilter, startAfter],
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

  const events = eventsQuery.data ?? [];

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
      const quantity = Number.parseInt(draft.quantity, 10);
      if (!draft.tierId) throw new Error('Selecciona un tipo de ticket.');
      if (!Number.isFinite(quantity) || quantity <= 0) throw new Error('Cantidad inválida.');
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
      const priceCents = Number.parseInt(draft.price, 10);
      const quantity = Number.parseInt(draft.quantity, 10);
      if (!draft.name.trim()) throw new Error('Nombre del ticket requerido.');
      if (!Number.isFinite(priceCents) || priceCents < 0) throw new Error('Precio inválido (usa centavos).');
      if (!Number.isFinite(quantity) || quantity <= 0) throw new Error('Cantidad inválida.');
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
      const plannedCents = Number.parseInt(draft.plannedCents, 10);
      if (!draft.name.trim()) throw new Error('Nombre de línea presupuestaria requerido.');
      if (!Number.isFinite(plannedCents) || plannedCents < 0) throw new Error('Presupuesto inválido (usa centavos).');
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
        notes: '',
        occurredAt: new Date().toISOString(),
      };
      const amountCents = Number.parseInt(draft.amountCents, 10);
      if (!draft.concept.trim()) throw new Error('Concepto contable requerido.');
      if (!Number.isFinite(amountCents) || amountCents <= 0) throw new Error('Monto inválido (usa centavos).');
      const payload: SocialEventFinanceEntryDTO = {
        efeBudgetLineId: draft.budgetLineId.trim() || null,
        efeDirection: draft.direction,
        efeSource: draft.source.trim() || 'manual',
        efeCategory: draft.category.trim() || 'general',
        efeConcept: draft.concept.trim(),
        efeAmountCents: amountCents,
        efeCurrency: draft.currency.trim().toUpperCase() || 'USD',
        efeStatus: draft.status,
        efeExternalRef: null,
        efeNotes: draft.notes.trim() || null,
        efeOccurredAt: draft.occurredAt || new Date().toISOString(),
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
          notes: '',
          occurredAt: new Date().toISOString(),
        },
      }));
      void qc.invalidateQueries({ queryKey: ['social-finance-entries', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-budget-lines', eventId] });
      void qc.invalidateQueries({ queryKey: ['social-finance-summary', eventId] });
      setFeedback({ kind: 'success', message: 'Asiento contable registrado.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

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
              notes: '',
              occurredAt: new Date().toISOString(),
            };
            const checkInCode = checkInCodes[eventId] ?? '';

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
                              <Stack direction="row" spacing={1} flexWrap="wrap">
                                <Chip size="small" label={`Ingresos: ${formatMoney(financeSummary.efsActualIncomeCents, financeSummary.efsCurrency)}`} color="success" />
                                <Chip size="small" label={`Gastos: ${formatMoney(financeSummary.efsActualExpenseCents, financeSummary.efsCurrency)}`} color="warning" />
                                <Chip size="small" label={`Neto: ${formatMoney(financeSummary.efsNetCents, financeSummary.efsCurrency)}`} />
                                <Chip size="small" label={`Utilización: ${formatPercent(financeSummary.efsBudgetUtilizationPct)}`} variant="outlined" />
                                <Chip size="small" label={`Tickets pagados: ${formatMoney(financeSummary.efsTicketPaidRevenueCents, financeSummary.efsCurrency)}`} variant="outlined" />
                                <Chip size="small" label={`Tickets reembolsados: ${formatMoney(financeSummary.efsTicketRefundedRevenueCents, financeSummary.efsCurrency)}`} variant="outlined" />
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
                                size="small"
                                value={budgetDraft.category}
                                onChange={(e) => setBudgetLineForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...budgetDraft, category: e.target.value },
                                }))}
                                sx={{ width: 140 }}
                              />
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
                                    </Typography>
                                  </Stack>
                                ))}
                              </Stack>
                            )}

                            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
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
                                label="Fuente"
                                size="small"
                                value={financeDraft.source}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, source: e.target.value },
                                }))}
                                sx={{ width: 130 }}
                              />
                              <TextField
                                label="Categoría"
                                size="small"
                                value={financeDraft.category}
                                onChange={(e) => setFinanceEntryForms((prev) => ({
                                  ...prev,
                                  [eventId]: { ...financeDraft, category: e.target.value },
                                }))}
                                sx={{ width: 130 }}
                              />
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
