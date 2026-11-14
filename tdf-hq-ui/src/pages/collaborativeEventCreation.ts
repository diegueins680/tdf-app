import { DateTime } from 'luxon';

import type {
  EventLogisticsMemberDTO,
  SocialEventDTO,
} from '../api/socialEvents';
import { resolveRuntimeCurrency } from '../utils/formatters';
import { parseOptionalPriceCents } from '../utils/eventTicketPricing';
import { normalizeLineup, type EventLineupArtistDraft } from '../utils/eventLineup';
export { parseOptionalPriceCents } from '../utils/eventTicketPricing';

export {
  MAX_ARTIST_NAME_LENGTH,
  MAX_EVENT_ARTISTS,
  normalizeLineup,
  type EventLineupArtistDraft,
} from '../utils/eventLineup';

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
  timezone: string;
  durationMinutes: number | null;
  venueId: string;
  eventTypeId: string;
  price: string;
  capacity: string;
  ticketUrl: string;
  isPublic: boolean;
  collaborators: EventCollaboratorDraft[];
  artists: EventLineupArtistDraft[];
}

export interface CollaborativeEventCreationDependencies {
  createEvent: (payload: SocialEventDTO) => Promise<SocialEventDTO>;
  /** Saves the full lineup, including artists named but not yet on the platform. */
  saveLineup?: (eventId: string, payload: SocialEventDTO) => Promise<SocialEventDTO>;
  uploadArt?: (eventId: string, file: File) => Promise<unknown>;
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
  /** Named artists the server has not accepted yet; the event itself exists. */
  pendingArtists: EventLineupArtistDraft[];
  lineupError: string | null;
  artUploaded: boolean;
  artError: string | null;
}

export const EVENT_ART_MAX_BYTES = 10 * 1024 * 1024; // Server limit for event images.
export const EVENT_ART_ACCEPT = 'image/jpeg,image/png,image/webp,image/gif';

/** Returns a user-facing problem with the chosen art file, or null when it can be uploaded. */
export function validateEventArtFile(file: Pick<File, 'type' | 'size'>): string | null {
  if (!EVENT_ART_ACCEPT.split(',').includes(file.type.toLowerCase())) {
    return 'El arte debe ser una imagen JPG, PNG, WebP o GIF.';
  }
  if (file.size <= 0) return 'El archivo está vacío. Elige otra imagen.';
  if (file.size > EVENT_ART_MAX_BYTES) return 'El arte debe pesar 10 MB o menos.';
  return null;
}

const lineupPayload = (artists: readonly EventLineupArtistDraft[]): SocialEventDTO['eventArtists'] =>
  artists.map(({ artistId, artistName }) => (artistId ? { artistId, artistName } : { artistName }));

const LOCAL_DATE_TIME_FORMAT = "yyyy-LL-dd'T'HH:mm";
const MAX_DURATION_MINUTES = 7 * 24 * 60;

/** Intl rejects fixed-offset zones such as "UTC+5" that Luxon tolerates. */
const isIntlTimeZone = (zone: string): boolean => {
  try {
    new Intl.DateTimeFormat('en-US', { timeZone: zone });
    return true;
  } catch {
    return false;
  }
};

const errorMessage = (reason: unknown, fallback = 'No se pudo conceder acceso.') =>
  reason instanceof Error && reason.message ? reason.message : fallback;

export function buildInitialCollaborativeEventDraft(
  now: DateTime = DateTime.local(),
): CollaborativeEventDraft {
  const startAt = now.plus({ hours: 1 }).startOf('hour');
  return {
    title: '',
    description: '',
    startAt: startAt.toFormat(LOCAL_DATE_TIME_FORMAT),
    timezone: now.zoneName ?? 'UTC',
    durationMinutes: 120,
    venueId: '',
    eventTypeId: '',
    price: '',
    capacity: '',
    ticketUrl: '',
    isPublic: false,
    collaborators: [],
    artists: [],
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
  if (draft.durationMinutes !== null && (
    !Number.isSafeInteger(draft.durationMinutes)
    || draft.durationMinutes <= 0
    || draft.durationMinutes > MAX_DURATION_MINUTES
  )) {
    throw new Error('Selecciona una duración válida.');
  }

  const timezone = draft.timezone.trim();
  // Luxon accepts fixed-offset zones such as "UTC+5" that Intl.DateTimeFormat
  // rejects, and the event detail page formats dates through Intl. Require both
  // so a saved event can always be rendered.
  if (!timezone || !DateTime.now().setZone(timezone).isValid || !isIntlTimeZone(timezone)) {
    throw new Error('Selecciona una zona horaria válida, por ejemplo America/Guayaquil.');
  }
  const start = DateTime.fromFormat(draft.startAt, LOCAL_DATE_TIME_FORMAT, { zone: timezone });
  if (!start.isValid) throw new Error('Selecciona una fecha y hora válidas.');
  // Luxon shifts nonexistent clock times during a DST transition. Ask the user
  // to choose a real, unambiguous wall-clock time instead of silently moving it.
  if (start.toFormat(LOCAL_DATE_TIME_FORMAT) !== draft.startAt || start.getPossibleOffsets().length > 1) {
    throw new Error('Esa hora cambia por el horario de verano. Selecciona otra hora.');
  }
  const startIso = start.toUTC().toISO();
  const endIso = draft.durationMinutes === null
    ? null
    : start.plus({ minutes: draft.durationMinutes }).toUTC().toISO();
  if (!startIso || (draft.durationMinutes !== null && !endIso)) {
    throw new Error('No se pudo interpretar la fecha del evento.');
  }
  const eventTypeId = draft.eventTypeId.trim();
  if (!eventTypeId) throw new Error('Selecciona un tipo de evento publicado.');

  return {
    eventTitle: title,
    eventDescription: draft.description.trim() || null,
    eventStart: startIso,
    eventTimezone: timezone,
    eventEnd: endIso,
    eventVenueId: draft.venueId.trim() || null,
    eventPriceCents: parseOptionalPriceCents(draft.price),
    eventCapacity: parseOptionalCapacity(draft.capacity),
    eventTypeId,
    eventCurrency: resolveRuntimeCurrency(),
    eventBudgetCents: null,
    eventTicketUrl: normalizeTicketUrl(draft.ticketUrl),
    eventImageUrl: null,
    eventIsPublic: draft.isPublic,
    // Creation links artists already on the platform; named ones follow in saveLineup.
    eventArtists: lineupPayload(normalizeLineup(draft.artists ?? []).filter((artist) => artist.artistId)),
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

/**
 * Adds artists that are not on the platform yet. Runs after creation so a
 * rejected name never costs the user the event itself.
 */
export async function saveNamedLineup(
  eventId: string,
  draft: CollaborativeEventDraft,
  saveLineup: NonNullable<CollaborativeEventCreationDependencies['saveLineup']>,
): Promise<SocialEventDTO> {
  const created = buildCollaborativeEventPayload(draft);
  // Same core fields as creation. Image, ticket link and visibility are left
  // out so the update keeps what the event already stores.
  return saveLineup(eventId, {
    eventTitle: created.eventTitle,
    eventDescription: created.eventDescription,
    eventStart: created.eventStart,
    eventEnd: created.eventEnd,
    eventTimezone: created.eventTimezone,
    eventVenueId: created.eventVenueId,
    eventPriceCents: created.eventPriceCents,
    eventCapacity: created.eventCapacity,
    eventTypeId: created.eventTypeId,
    eventArtists: lineupPayload(normalizeLineup(draft.artists ?? [])),
  });
}

export async function createCollaborativeEvent(
  draft: CollaborativeEventDraft,
  dependencies: CollaborativeEventCreationDependencies,
  artFile: File | null = null,
): Promise<CollaborativeEventCreationResult> {
  if (artFile) {
    const artProblem = validateEventArtFile(artFile);
    if (artProblem) throw new Error(artProblem);
  }
  let event = await dependencies.createEvent(buildCollaborativeEventPayload(draft));
  const eventId = event.eventId?.trim();
  if (!eventId) {
    throw new Error('El evento fue creado sin un identificador válido.');
  }

  const namedArtists = normalizeLineup(draft.artists ?? []).filter((artist) => !artist.artistId);
  let pendingArtists: EventLineupArtistDraft[] = [];
  let lineupError: string | null = null;
  if (namedArtists.length > 0) {
    try {
      if (!dependencies.saveLineup) throw new Error('No se pudo guardar a los artistas nuevos.');
      event = { ...await saveNamedLineup(eventId, draft, dependencies.saveLineup), eventId };
    } catch (reason) {
      pendingArtists = namedArtists;
      lineupError = errorMessage(reason, 'No se pudo guardar a los artistas nuevos.');
    }
  }

  let artUploaded = false;
  let artError: string | null = null;
  if (artFile) {
    try {
      if (!dependencies.uploadArt) throw new Error('No se pudo subir el arte.');
      await dependencies.uploadArt(eventId, artFile);
      artUploaded = true;
    } catch (reason) {
      artError = errorMessage(reason, 'No se pudo subir el arte.');
    }
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

  return {
    event, addedCollaborators, failedCollaborators, pendingArtists, lineupError, artUploaded, artError,
  };
}
