import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { WorkflowState } from '../../api/catalogs';
import type { SocialEventDTO } from '../../api/socialEvents';

const readEvent = jest.fn<() => Promise<SocialEventDTO>>();
const updateEvent = jest.fn<(eventId: string, payload: SocialEventDTO) => Promise<SocialEventDTO>>();
const readStates = jest.fn<() => Promise<{ states: WorkflowState[] }>>();
jest.unstable_mockModule('../../api/socialEvents', () => ({ SocialEventsAPI: { getEvent: readEvent, updateEvent } }));
jest.unstable_mockModule('../../api/catalogs', () => ({ Catalogs: { getPublicWorkflowStates: readStates } }));
const { default: EventPublishControl, resolvePublishTarget } = await import('./EventPublishControl');

// Capabilities and transition targets are space-separated here: these are
// fixtures of the server's lifecycle catalog, not lists the web app owns.
const words = (value: string) => value.split(' ').filter(Boolean);
const state = (id: string, code: string, capabilities: string, to: string): WorkflowState => ({
  id, code, capabilities: words(capabilities), workflowId: 'w', workflowCode: 'social-event-lifecycle', name: code, nameEs: code, nameEn: code,
  sortOrder: Number(id), terminal: false, active: true, initialContexts: [], version: 1,
  transitions: words(to).map((toStateId) => ({ toStateId, directExecutionAllowed: true, requiresReview: false, requiresDistinctApprover: false, version: 1 })),
});
const states = [
  state('1', 'planning', '', '3 2 9'),
  state('2', 'announced', 'public-listable rsvp', '3'),
  state('3', 'on-sale', 'public-listable rsvp ticket-purchase', ''),
  state('9', 'cancelled', '', ''),
];
const draft: SocialEventDTO = {
  eventId: '222', eventTitle: 'Noche de prueba', eventStart: '2027-03-21T01:00:00Z', eventEnd: '2027-03-21T03:00:00Z',
  eventTimezone: 'America/Guayaquil', eventTypeId: 't', eventWorkflowStateId: '1', eventIsPublic: false, eventPublicListable: false,
  eventArtists: [{ artistId: '51', artistName: 'Liquid Paper Girl' }],
};

function setup(event: SocialEventDTO = draft) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  return render(<QueryClientProvider client={client}><MemoryRouter><EventPublishControl event={event} /></MemoryRouter></QueryClientProvider>);
}
beforeEach(() => {
  readStates.mockReset().mockResolvedValue({ states });
  readEvent.mockReset().mockResolvedValue(draft);
  updateEvent.mockReset().mockResolvedValue({ ...draft, eventIsPublic: true, eventPublicListable: true });
});
afterEach(cleanup);

describe('resolvePublishTarget', () => {
  it('moves a draft to the earliest public state the organizer may enter directly', () => {
    expect(resolvePublishTarget(draft, states)).toEqual({ workflowStateId: '2' });
  });
  it('only turns visibility on when the state is already public', () => {
    expect(resolvePublishTarget({ eventWorkflowStateId: '2' }, states)).toEqual({});
  });
  it('offers nothing from a state with no direct public transition', () => {
    expect(resolvePublishTarget({ eventWorkflowStateId: '9' }, states)).toBeNull();
    expect(resolvePublishTarget({ eventWorkflowStateId: 'unknown' }, states)).toBeNull();
    const reviewed = [state('1', 'planning', '', '2'), state('2', 'announced', 'public-listable', '')];
    reviewed[0]!.transitions[0]!.requiresReview = true;
    expect(resolvePublishTarget(draft, reviewed)).toBeNull();
  });
});

describe('EventPublishControl', () => {
  it('publishes only after an explicit confirmation, from the latest saved fields', async () => {
    readEvent.mockResolvedValue({ ...draft, eventTitle: 'Título corregido por otra persona' });
    setup();
    fireEvent.click(await screen.findByRole('button', { name: 'Publicar evento' }));
    expect(updateEvent).not.toHaveBeenCalled();
    expect(screen.getByText(/Cualquier persona podrá ver el nombre/)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Publicar ahora' }));
    await waitFor(() => expect(updateEvent).toHaveBeenCalledTimes(1));
    expect(updateEvent).toHaveBeenCalledWith('222', {
      eventTitle: 'Título corregido por otra persona',
      eventStart: draft.eventStart,
      eventEnd: draft.eventEnd,
      eventDescription: undefined,
      eventTimezone: 'America/Guayaquil',
      eventVenueId: undefined,
      eventPriceCents: undefined,
      eventCapacity: undefined,
      eventTypeId: 't',
      eventArtists: [{ artistId: '51', artistName: '' }],
      eventIsPublic: true,
      eventWorkflowStateId: '2',
    });
  });

  it('lets the organizer back out without writing anything', async () => {
    setup();
    fireEvent.click(await screen.findByRole('button', { name: 'Publicar evento' }));
    fireEvent.click(screen.getByRole('button', { name: 'Cancelar' }));
    expect(screen.getByRole('button', { name: 'Publicar evento' })).toBeTruthy();
    expect(readEvent).not.toHaveBeenCalled();
    expect(updateEvent).not.toHaveBeenCalled();
  });

  it('keeps the confirmation open and says why when publishing fails', async () => {
    updateEvent.mockRejectedValue(new Error('Event workflow changed; reload before updating'));
    setup();
    fireEvent.click(await screen.findByRole('button', { name: 'Publicar evento' }));
    fireEvent.click(screen.getByRole('button', { name: 'Publicar ahora' }));
    expect(await screen.findByText('Event workflow changed; reload before updating')).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Publicar ahora' })).toBeTruthy();
  });

  it('refuses to publish when the event left the publishable state meanwhile', async () => {
    readEvent.mockResolvedValue({ ...draft, eventWorkflowStateId: '9' });
    setup();
    fireEvent.click(await screen.findByRole('button', { name: 'Publicar evento' }));
    fireEvent.click(screen.getByRole('button', { name: 'Publicar ahora' }));
    expect(await screen.findByText(/El estado del evento cambió/)).toBeTruthy();
    expect(updateEvent).not.toHaveBeenCalled();
  });

  it('links to the public page once published and offers nothing for a cancelled event', async () => {
    const view = setup({ ...draft, eventWorkflowStateId: '2', eventIsPublic: true, eventPublicListable: true });
    expect(screen.getByRole('link', { name: 'Ver página pública' }).getAttribute('href')).toBe('/eventos/222');
    expect(readStates).not.toHaveBeenCalled();
    view.unmount();

    setup({ ...draft, eventWorkflowStateId: '9' });
    await waitFor(() => expect(readStates).toHaveBeenCalled());
    expect(screen.queryByRole('button', { name: 'Publicar evento' })).toBeNull();
  });
});
