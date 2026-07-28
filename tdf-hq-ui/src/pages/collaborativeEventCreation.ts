import { DateTime } from 'luxon';

import type {
  EventLogisticsMemberDTO,
  SocialEventDTO,
} from '../api/socialEvents';

export type CollaborativeEventRole = EventLogisticsMemberDTO['elmRole'];

export interface EventCollaboratorDraft {
  partyId: string;
  displayName: string;
  email?: string | null;
  role: CollaborativeEventRole;
}

export interface CollaborativeEventDraft {
  title: string;
  description: string;
  startAt: string;
  durationMinutes: number;
  venueId: string;
  eventType: 'party' | 'concert' | 'festival' | 'showcase' | 'meeting' | 'other';
  price: string;
  capacity: string;
  ticketUrl: string;
  isPublic: boolean;
  collaborators: EventCollaboratorDraft[];
}

export interface CollaborativeEventCreationDependencies {
  createEvent: (payload: SocialEventDTO) => Promise<SocialEventDTO>;
  addCollaborator: (
    eventId: string,
    collaborator: EventCollaboratorDraft,
  ) => Promise<EventLogisticsMemberDTO>;
}

export interface CollaborativeEventCreationResult {
  event: SocialEventDTO;
  addedCollaborators: EventCollaboratorDraft[];
  failedCollaborators: {
    collaborator: EventCollaboratorDraft;
    reason: string;
  }[];
}

const LOCAL_DATE_TIME_FORMAT = "yyyy-LL-dd'T'HH:mm";
const MAX_DURATION_MINUTES = 7 * 24 * 60;

const errorMessage = (reason: unknown) =>
  reason instanceof Error ? reason.message : 'No se pudo conceder acceso.';

export function buildInitialCollaborativeEventDraft(
  now: DateTime = DateTime.local(),
): CollaborativeEventDraft {
  const startAt = now.plus({ hours: 1 }).startOf('hour');
  return {
    title: '',
    description: '',
    startAt: startAt.toFormat(LOCAL_DATE_TIME_FORMAT),
    durationMinutes: 120,
    venueId: '',
    eventType: 'party',
    price: '',
    capacity: '',
    ticketUrl: '',
    isPublic: false,
    collaborators: [],
  };
}

const parseOptionalCapacity = (raw: string): number | null => {
  const value = raw.trim();
  if (!value) return null;
  if (!/^\d+$/.test(value)) {
    throw new Error('La capacidad debe ser un número entero.');
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error('La capacidad debe ser mayor que cero.');
  }
  return parsed;
};

export const parseOptionalPriceCents = (raw: string): number | null => {
  const value = raw.trim();
  if (!value) return null;
  if (!/^\d+(?:[.,]\d{1,2})?$/.test(value)) {
    throw new Error('El precio debe tener como máximo dos decimales.');
  }
  const [whole = '0', fractional = ''] = value.replace(',', '.').split('.');
  const cents = (Number(whole) * 100) + Number(fractional.padEnd(2, '0'));
  if (!Number.isSafeInteger(cents) || cents < 0) {
    throw new Error('El precio está fuera del rango permitido.');
  }
  return cents;
};

const normalizeTicketUrl = (raw: string): string | null => {
  const value = raw.trim();
  if (!value) return null;
  let parsed: URL;
  try {
    parsed = new URL(value);
  } catch {
    throw new Error('El enlace de tickets no es una URL válida.');
  }
  if (parsed.protocol !== 'https:' && parsed.protocol !== 'http:') {
    throw new Error('El enlace de tickets debe comenzar con http:// o https://.');
  }
  return parsed.toString();
};

export function buildCollaborativeEventPayload(
  draft: CollaborativeEventDraft,
): SocialEventDTO {
  const title = draft.title.trim();
  if (!title) throw new Error('Escribe un nombre para el evento.');
  if (title.length > 200) {
    throw new Error('El nombre del evento debe tener 200 caracteres o menos.');
  }
  if (
    !Number.isSafeInteger(draft.durationMinutes)
    || draft.durationMinutes <= 0
    || draft.durationMinutes > MAX_DURATION_MINUTES
  ) {
    throw new Error('Selecciona una duración válida.');
  }

  const start = DateTime.fromFormat(draft.startAt, LOCAL_DATE_TIME_FORMAT, {
    zone: 'local',
  });
  if (!start.isValid) throw new Error('Selecciona una fecha y hora válidas.');
  const end = start.plus({ minutes: draft.durationMinutes });
  const startIso = start.toUTC().toISO();
  const endIso = end.toUTC().toISO();
  if (!startIso || !endIso) throw new Error('No se pudo interpretar la fecha del evento.');

  return {
    eventTitle: title,
    eventDescription: draft.description.trim() || null,
    eventStart: startIso,
    eventEnd: endIso,
    eventVenueId: draft.venueId.trim() || null,
    eventPriceCents: parseOptionalPriceCents(draft.price),
    eventCapacity: parseOptionalCapacity(draft.capacity),
    eventType: draft.eventType,
    eventStatus: 'planning',
    eventCurrency: 'USD',
    eventBudgetCents: null,
    eventTicketUrl: normalizeTicketUrl(draft.ticketUrl),
    eventImageUrl: null,
    eventIsPublic: draft.isPublic,
    eventArtists: [],
  };
}

export function normalizeCollaborators(
  collaborators: EventCollaboratorDraft[],
): EventCollaboratorDraft[] {
  const seen = new Set<string>();
  return collaborators.filter((collaborator) => {
    const partyId = collaborator.partyId.trim();
    if (!partyId || seen.has(partyId)) return false;
    seen.add(partyId);
    return true;
  });
}

export async function createCollaborativeEvent(
  draft: CollaborativeEventDraft,
  dependencies: CollaborativeEventCreationDependencies,
): Promise<CollaborativeEventCreationResult> {
  const event = await dependencies.createEvent(buildCollaborativeEventPayload(draft));
  const eventId = event.eventId?.trim();
  if (!eventId) {
    throw new Error('El evento fue creado sin un identificador válido.');
  }

  const collaborators = normalizeCollaborators(draft.collaborators);
  const collaboratorResults = await Promise.allSettled(
    collaborators.map((collaborator) =>
      dependencies.addCollaborator(eventId, collaborator),
    ),
  );
  const addedCollaborators: EventCollaboratorDraft[] = [];
  const failedCollaborators: CollaborativeEventCreationResult['failedCollaborators'] = [];

  collaboratorResults.forEach((result, index) => {
    const collaborator = collaborators[index];
    if (!collaborator) return;
    if (result.status === 'fulfilled') {
      addedCollaborators.push(collaborator);
    } else {
      failedCollaborators.push({
        collaborator,
        reason: errorMessage(result.reason),
      });
    }
  });

  return { event, addedCollaborators, failedCollaborators };
}
