import { jest } from '@jest/globals';
import { DateTime } from 'luxon';

import {
  buildCollaborativeEventPayload,
  buildInitialCollaborativeEventDraft,
  createCollaborativeEvent,
  parseOptionalPriceCents,
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
    expect(start.toLocal().toFormat("yyyy-MM-dd'T'HH:mm")).toBe(
      '2026-07-28T11:00',
    );
    expect(end.diff(start, 'minutes').minutes).toBe(120);
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
});
