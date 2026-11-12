import { afterEach, beforeEach, expect, it, jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

import type { ClassifiedApplication } from '../../api/directory';
import { formatMinor } from './offers';

const setApplicationStatus = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const counterOffer = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const contact = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('../../api/directory', () => ({ Directory: { setApplicationStatus, counterOffer, contact } }));
jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ t: (key: string, options?: Record<string, unknown>) => (options && 'amount' in options ? `${key} ${String(options['amount'])}` : key), i18n: { language: 'es' } }),
}));
const { default: OfferCard } = await import('./OfferCard');

const currencies = [{ id: 'usd', code: 'USD', name: 'Dólar', symbol: '$', minorUnits: 2 }];
const money = (minor: number) => formatMinor(minor, 'usd', currencies, 'es') ?? '';
const base: ClassifiedApplication = {
  id: 'offer-1', classifiedId: 'classified-1',
  classified: { id: 'classified-1', authorProfileId: 'author-profile', title: 'Banda para boda', slug: 'banda-para-boda', status: 'published', priceMode: 'suggested', slots: 1, slotsRemaining: 1, budget: { maxMinor: 120000, currencyId: 'usd', negotiable: true } },
  applicantProfileId: 'band-profile', applicantProfile: { id: 'band-profile', name: 'Los Sintéticos', slug: 'los-sinteticos', kind: 'band' },
  message: 'Tocamos cuatro horas con backline incluido.', currencyId: 'usd', proposedAmountMinor: 130000, currentAmountMinor: 130000,
  currentRound: 1, lastActorRole: 'applicant', status: 'submitted', participantRole: 'author',
  submittedAt: '2026-11-01T12:00:00Z', version: 1,
};

let client: QueryClient;
beforeEach(() => {
  setApplicationStatus.mockReset(); counterOffer.mockReset(); contact.mockReset();
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
});
afterEach(() => { cleanup(); client.clear(); });
const show = (offer: ClassifiedApplication) => render(
  <MemoryRouter><QueryClientProvider client={client}><OfferCard offer={offer} currencies={currencies} /></QueryClientProvider></MemoryRouter>,
);

it('lets the author accept, counter, shortlist or reject a fresh offer and shows the amount', async () => {
  show(base);
  expect(screen.getByText(money(130000))).toBeTruthy();
  const buttons = screen.getAllByRole('button').map((button) => button.textContent);
  expect(buttons).toEqual(expect.arrayContaining([`marketplace.offer.accept ${money(130000)}`, 'marketplace.offer.counter', 'marketplace.offer.shortlist', 'marketplace.offer.reject']));
  expect(buttons).not.toContain('marketplace.offer.withdraw');
  setApplicationStatus.mockResolvedValue({ ...base, status: 'accepted' });
  fireEvent.click(screen.getByRole('button', { name: `marketplace.offer.accept ${money(130000)}` }));
  await waitFor(() => expect(setApplicationStatus).toHaveBeenCalledWith('offer-1', 'accepted'));
});

it('records a counter-offer round with the amount in minor units', async () => {
  show(base);
  fireEvent.click(screen.getByRole('button', { name: 'marketplace.offer.counter' }));
  fireEvent.change(screen.getByLabelText(/marketplace\.offer\.amount/), { target: { value: '1250' } });
  counterOffer.mockResolvedValue({ ...base, status: 'countered_by_author', currentAmountMinor: 125000 });
  fireEvent.click(screen.getByRole('button', { name: 'marketplace.offer.sendCounter' }));
  await waitFor(() => expect(counterOffer).toHaveBeenCalledWith('offer-1', { amountMinor: 125000, currencyId: 'usd', message: undefined }));
});

it('gives the offerer only accept, counter and decline while the author counter is pending', () => {
  show({ ...base, participantRole: 'applicant', status: 'countered_by_author', lastActorRole: 'author', currentRound: 2, currentAmountMinor: 125000 });
  const buttons = screen.getAllByRole('button').map((button) => button.textContent);
  expect(buttons).toEqual(expect.arrayContaining([`marketplace.offer.accept ${money(125000)}`, 'marketplace.offer.counter', 'marketplace.offer.decline']));
  expect(buttons).not.toContain('marketplace.offer.reject');
  expect(buttons).not.toContain('marketplace.offer.shortlist');
  expect(screen.getByText('marketplace.offer.yourTurn')).toBeTruthy();
});

it('never offers a counter on a fixed-price classified and explains a slots_filled rejection', () => {
  show({ ...base, classified: { ...base.classified, priceMode: 'fixed' } });
  expect(screen.queryByRole('button', { name: 'marketplace.offer.counter' })).toBeNull();
  cleanup();
  show({ ...base, participantRole: 'applicant', status: 'rejected', rejectionReason: 'slots_filled' });
  expect(screen.getByText('marketplace.offer.rejection.slots_filled')).toBeTruthy();
  expect(screen.queryAllByRole('button').map((button) => button.textContent)).not.toContain('marketplace.offer.withdraw');
});

it('shows the structured terms of the offer and of each negotiation round before any decision', () => {
  show({
    ...base,
    terms: { durationMinutes: 240, equipmentProvided: 'Backline completo', soundcheck: 'a las 16:00' },
    currentRound: 2,
    lastActorRole: 'author',
    status: 'countered_by_author',
    rounds: [
      { roundNumber: 1, actorRole: 'applicant', kind: 'initial', amountMinor: 130000, currencyId: 'usd', terms: { durationMinutes: 240 }, createdAt: '2026-11-01T12:00:00Z' },
      { roundNumber: 2, actorRole: 'author', kind: 'counter', amountMinor: 125000, currencyId: 'usd', terms: { durationMinutes: 180 }, message: 'Tres horas bastan.', createdAt: '2026-11-02T12:00:00Z' },
    ],
  });
  const terms = screen.getByTestId('offer-terms');
  expect(terms.textContent).toContain('marketplace.offer.termsDuration');
  expect(terms.textContent).toContain('marketplace.offer.termsEquipment');
  expect(terms.textContent).toContain('soundcheck: a las 16:00');
  fireEvent.click(screen.getByRole('button', { name: 'marketplace.offer.showRounds' }));
  expect(screen.getByText(/Tres horas bastan/).textContent).toContain('marketplace.offer.roundTerms');
});
