import { jest } from '@jest/globals';
import { DateTime } from 'luxon';

import type { SocialEventDTO } from '../api/socialEvents';
import {
  buildCollaborativeEventPayload,
  buildInitialCollaborativeEventDraft,
  createCollaborativeEvent,
  normalizeLineup,
  parseOptionalPriceCents,
  resolveEventCreatorTimezone,
  validateEventArtFile,
  type CollaborativeEventDraft,
} from './collaborativeEventCreation';

describe('collaborative event creation', () => {
  const eventTypeId = '41000000-0000-4000-8000-000000000001';
  const draft = (): CollaborativeEventDraft => ({
    ...buildInitialCollaborativeEventDraft(
      DateTime.fromISO('2026-07-28T10:15:00', { zone: 'America/Guayaquil' }),
    ),
    title: '  TDF Summer Session  ',
    description: '  Show colaborativo  ',
    price: '12.50',
    capacity: '250',
    ticketUrl: 'https://tickets.example.com/tdf',
    eventTypeId,
  });

  it('starts one hour ahead with a private two-hour planning draft', () => {
    expect(buildInitialCollaborativeEventDraft(
      DateTime.fromISO('2026-07-28T10:15:00', { zone: 'America/Guayaquil' }),
    )).toMatchObject({
      startAt: '2026-07-28T11:00',
      timezone: 'America/Guayaquil',
      durationMinutes: 120,
      eventTypeId: '',
      isPublic: false,
      collaborators: [],
    });
  });

  it('turns human-friendly price and duration fields into the backend payload', () => {
    const { eventStart, eventEnd, ...payload } =
      buildCollaborativeEventPayload(draft());
    const start = DateTime.fromISO(eventStart);
    const end = DateTime.fromISO(eventEnd!);

    expect(payload).toEqual({
      eventTitle: 'TDF Summer Session',
      eventTimezone: 'America/Guayaquil',
      eventDescription: 'Show colaborativo',
      eventVenueId: null,
      eventPriceCents: 1250,
      eventCapacity: 250,
      eventTypeId,
      eventCurrency: 'USD',
      eventBudgetCents: null,
      eventTicketUrl: 'https://tickets.example.com/tdf',
      eventImageUrl: null,
      eventIsPublic: false,
      eventArtists: [],
    });
    expect(start.setZone('America/Guayaquil').toFormat("yyyy-MM-dd'T'HH:mm")).toBe(
      '2026-07-28T11:00',
    );
    expect(end.diff(start, 'minutes').minutes).toBe(120);
  });

  it('starts in the organizer\'s local zone unless the account chose one', () => {
    // A new account carries the server placeholder, not a choice.
    expect(resolveEventCreatorTimezone('UTC', 'America/Guayaquil')).toBe('America/Guayaquil');
    expect(resolveEventCreatorTimezone('', 'America/Guayaquil')).toBe('America/Guayaquil');
    expect(resolveEventCreatorTimezone('America/Bogota', 'America/Guayaquil')).toBe('America/Bogota');
    expect(resolveEventCreatorTimezone('UTC', 'UTC')).toBe('UTC');
    expect(resolveEventCreatorTimezone('UTC', 'Not/AZone')).toBe('UTC');
    expect(resolveEventCreatorTimezone('UTC', undefined)).toBe('UTC');
  });

  it('uses the event timezone independently of the browser and rejects DST shifts', () => {
    expect(buildCollaborativeEventPayload({ ...draft(), startAt: '2026-11-28T18:00' }).eventStart)
      .toBe('2026-11-28T23:00:00.000Z');
    expect(() => buildCollaborativeEventPayload({ ...draft(), timezone: 'Invalid/Zone' }))
      .toThrow('Selecciona una zona horaria válida');
    // Luxon accepts fixed-offset zones but Intl (used to render the event) does not.
    expect(() => buildCollaborativeEventPayload({ ...draft(), timezone: 'UTC+5' }))
      .toThrow('Selecciona una zona horaria válida');
    for (const startAt of ['2026-03-08T02:30', '2026-11-01T01:30']) {
      expect(() => buildCollaborativeEventPayload({ ...draft(), timezone: 'America/New_York', startAt }))
        .toThrow('horario de verano');
    }
  });

  it('does not invent an end when the official duration is unknown', () => {
    const payload = buildCollaborativeEventPayload({
      ...draft(),
      durationMinutes: null,
    });

    expect(payload.eventEnd).toBeNull();
    expect(payload.eventStart).toBeTruthy();
  });

  it('accepts comma decimals and rejects ambiguous prices', () => {
    expect(parseOptionalPriceCents('9,5')).toBe(950);
    expect(parseOptionalPriceCents('')).toBeNull();
    expect(() => parseOptionalPriceCents('9.999')).toThrow(
      'El precio debe tener como máximo dos decimales.',
    );
  });

  it('creates the event first, deduplicates collaborators, and reports partial access failures', async () => {
    const eventDraft = {
      ...draft(),
      collaborators: [
        { partyId: '2', displayName: 'Ana', role: 'editor' as const },
        { partyId: '2', displayName: 'Ana duplicada', role: 'viewer' as const },
        { partyId: '3', displayName: 'Luis', role: 'viewer' as const },
      ],
    };
    const createEvent = jest.fn().mockResolvedValue({
      ...buildCollaborativeEventPayload(eventDraft),
      eventId: 'event-42',
    });
    const addCollaborator = jest.fn()
      .mockResolvedValueOnce({ elmPartyId: '2', elmRole: 'editor' })
      .mockRejectedValueOnce(new Error('Sin acceso'));

    const result = await createCollaborativeEvent(eventDraft, {
      createEvent,
      addCollaborator,
    });

    expect(createEvent).toHaveBeenCalledTimes(1);
    expect(addCollaborator).toHaveBeenCalledTimes(2);
    expect(addCollaborator).toHaveBeenNthCalledWith(
      1,
      'event-42',
      expect.objectContaining({ partyId: '2', role: 'editor' }),
    );
    expect(result.addedCollaborators.map(({ partyId }) => partyId)).toEqual(['2']);
    expect(result.failedCollaborators).toEqual([
      {
        collaborator: expect.objectContaining({ partyId: '3', displayName: 'Luis' }),
        reason: 'Sin acceso',
      },
    ]);
  });

  it('accepts raster art up to 10 MB and explains what it rejects', () => {
    expect(validateEventArtFile({ type: 'image/png', size: 1024 })).toBeNull();
    expect(validateEventArtFile({ type: 'IMAGE/JPEG', size: 10 * 1024 * 1024 })).toBeNull();
    expect(validateEventArtFile({ type: 'application/pdf', size: 1024 })).toMatch(/JPG, PNG, WebP o GIF/);
    expect(validateEventArtFile({ type: 'image/png', size: 0 })).toMatch(/vacío/);
    expect(validateEventArtFile({ type: 'image/png', size: 10 * 1024 * 1024 + 1 })).toMatch(/10 MB/);
  });

  it('cleans a lineup: trims names, drops blanks and repeats, keeps the order', () => {
    expect(normalizeLineup([
      { artistId: ' 7 ', artistName: 'Vera' },
      { artistId: null, artistName: '  Los   Nuevos ' },
      { artistId: '7', artistName: 'Vera otra vez' },
      { artistId: null, artistName: 'los nuevos' },
      { artistId: null, artistName: '   ' },
    ])).toEqual([
      { artistId: '7', artistName: 'Vera' },
      { artistId: null, artistName: 'Los Nuevos' },
    ]);
  });

  it('creates with known artists, then adds named ones and uploads the art', async () => {
    const eventDraft = {
      ...draft(),
      artists: [
        { artistId: '7', artistName: 'Vera' },
        { artistId: null, artistName: 'Los Nuevos' },
      ],
    };
    const calls: string[] = [];
    const createEvent = jest.fn(async (payload: SocialEventDTO) => {
      calls.push('create');
      return { ...payload, eventId: '42' };
    });
    const saveLineup = jest.fn(async (_eventId: string, payload: SocialEventDTO) => {
      calls.push('lineup');
      return payload;
    });
    const uploadArt = jest.fn(async () => { calls.push('art'); });
    const art = new File(['png'], 'afiche.png', { type: 'image/png' });

    const result = await createCollaborativeEvent(
      eventDraft,
      { createEvent, saveLineup, uploadArt, addCollaborator: jest.fn<never>() },
      art,
    );

    // Lineup precedes the art so the update can never replace the uploaded image.
    expect(calls).toEqual(['create', 'lineup', 'art']);
    expect(createEvent.mock.calls[0]?.[0].eventArtists).toEqual([{ artistId: '7', artistName: 'Vera' }]);
    const [lineupEventId, lineupPayload] = saveLineup.mock.calls[0] ?? [];
    expect(lineupEventId).toBe('42');
    expect(lineupPayload?.eventArtists).toEqual([
      { artistId: '7', artistName: 'Vera' },
      { artistName: 'Los Nuevos' },
    ]);
    expect(lineupPayload).not.toHaveProperty('eventImageUrl');
    expect(lineupPayload).not.toHaveProperty('eventTicketUrl');
    expect(uploadArt).toHaveBeenCalledWith('42', art);
    expect(result).toMatchObject({ pendingArtists: [], lineupError: null, artUploaded: true, artError: null });
    expect(result.event.eventId).toBe('42');
  });

  it('keeps the created event when named artists or the art are refused', async () => {
    const eventDraft = { ...draft(), artists: [{ artistId: null, artistName: 'Los Nuevos' }] };
    const createEvent = jest.fn(async (payload: SocialEventDTO) => ({ ...payload, eventId: '42' }));
    const result = await createCollaborativeEvent(
      eventDraft,
      {
        createEvent,
        saveLineup: jest.fn(async () => { throw new Error('eventArtists[].artistId is required'); }),
        uploadArt: jest.fn(async () => { throw new Error('Sin conexión'); }),
        addCollaborator: jest.fn<never>(),
      },
      new File(['png'], 'afiche.png', { type: 'image/png' }),
    );

    expect(createEvent.mock.calls[0]?.[0].eventArtists).toEqual([]);
    expect(result.event.eventId).toBe('42');
    expect(result.pendingArtists).toEqual([{ artistId: null, artistName: 'Los Nuevos' }]);
    expect(result.lineupError).toBe('eventArtists[].artistId is required');
    expect(result).toMatchObject({ artUploaded: false, artError: 'Sin conexión' });
  });

  it('refuses invalid art before anything is created', async () => {
    const createEvent = jest.fn<never>();
    await expect(createCollaborativeEvent(
      draft(),
      { createEvent, addCollaborator: jest.fn<never>() },
      new File(['%PDF'], 'afiche.pdf', { type: 'application/pdf' }),
    )).rejects.toThrow(/JPG, PNG, WebP o GIF/);
    expect(createEvent).not.toHaveBeenCalled();
  });
});
