import { afterEach, beforeEach, expect, it, jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

import type { MarketplaceAgreement } from '../../api/directory';
import { formatMinor } from './offers';

const setAgreementStatus = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('../../api/directory', () => ({ Directory: { setAgreementStatus } }));
jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ t: (key: string, options?: Record<string, unknown>) => (options && 'percent' in options ? `${key} ${String(options['percent'])}` : key), i18n: { language: 'es' } }),
}));
const { default: AgreementCard } = await import('./AgreementCard');

const currencies = [{ id: 'usd', code: 'USD', name: 'Dólar', symbol: '$', minorUnits: 2 }];
const money = (minor: number) => formatMinor(minor, 'usd', currencies, 'es') ?? '';
const base: MarketplaceAgreement = {
  id: 'agreement-1', applicationId: 'offer-1',
  classified: { id: 'classified-1', title: 'Banda para boda', slug: 'banda-para-boda' },
  buyerProfile: { id: 'author-profile', name: 'Planner Quito', slug: 'planner-quito' },
  providerProfile: { id: 'band-profile', name: 'Los Sintéticos', slug: 'los-sinteticos' },
  amountMinor: 125000, currencyId: 'usd', commissionBps: 1000, commissionMinor: 12500, providerNetMinor: 112500,
  depositBps: 3000, depositMinor: 37500, balanceMinor: 87500,
  terms: { durationMinutes: 240 }, settlementMode: 'off_platform', status: 'confirmed', participantRole: 'buyer',
  createdAt: '2026-11-12T12:00:00Z', version: 3,
};

let client: QueryClient;
beforeEach(() => {
  setAgreementStatus.mockReset();
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
});
afterEach(() => { cleanup(); client.clear(); });
const show = (agreement: MarketplaceAgreement) => render(
  <MemoryRouter><QueryClientProvider client={client}><AgreementCard agreement={agreement} currencies={currencies} /></QueryClientProvider></MemoryRouter>,
);

it('shows the frozen amounts, the off-platform notice and the agreed terms', () => {
  show(base);
  const amounts = screen.getByTestId('agreement-amounts').textContent ?? '';
  expect(amounts).toContain('marketplace.agreement.deposit 30');
  expect(amounts).toContain(money(37500));
  expect(amounts).toContain(money(87500));
  expect(amounts).toContain('marketplace.agreement.commission 10');
  expect(amounts).toContain(money(12500));
  expect(amounts).toContain(money(112500));
  expect(screen.getByText('marketplace.agreement.offPlatform')).toBeTruthy();
  expect(screen.getByTestId('agreement-terms').textContent).toContain('marketplace.offer.termsDuration');
});

it('lets the buyer confirm completion bound to the displayed version', async () => {
  show(base);
  const buttons = screen.getAllByRole('button').map((button) => button.textContent);
  expect(buttons).toEqual(['marketplace.agreement.action.completed', 'marketplace.agreement.action.cancelled', 'marketplace.agreement.action.disputed']);
  fireEvent.click(screen.getByRole('button', { name: 'marketplace.agreement.action.completed' }));
  expect(screen.getByText('marketplace.agreement.confirm.completed')).toBeTruthy();
  setAgreementStatus.mockResolvedValue({ ...base, status: 'completed' });
  fireEvent.click(screen.getByRole('button', { name: 'marketplace.agreement.action.completed' }));
  await waitFor(() => expect(setAgreementStatus).toHaveBeenCalledWith('agreement-1', 'completed', undefined, 3));
});

it('asks for a reason when cancelling and sends it', async () => {
  show({ ...base, participantRole: 'provider' });
  expect(screen.getAllByRole('button').map((button) => button.textContent)).toEqual(['marketplace.agreement.action.in_progress', 'marketplace.agreement.action.cancelled', 'marketplace.agreement.action.disputed']);
  fireEvent.click(screen.getByRole('button', { name: 'marketplace.agreement.action.cancelled' }));
  fireEvent.change(screen.getByLabelText('marketplace.agreement.reason'), { target: { value: 'Cambio de fecha' } });
  setAgreementStatus.mockResolvedValue({ ...base, status: 'cancelled' });
  fireEvent.click(screen.getByRole('button', { name: 'marketplace.agreement.action.cancelled' }));
  await waitFor(() => expect(setAgreementStatus).toHaveBeenCalledWith('agreement-1', 'cancelled', 'Cambio de fecha', 3));
});

it('offers no action on a completed agreement and points to verified reviews', () => {
  show({ ...base, status: 'completed' });
  expect(screen.queryAllByRole('button')).toHaveLength(0);
  expect(screen.getByText('marketplace.agreement.reviewHint')).toBeTruthy();
});
