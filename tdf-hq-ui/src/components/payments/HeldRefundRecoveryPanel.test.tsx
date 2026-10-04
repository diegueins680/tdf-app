/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import type { ServiceRefundRecovery } from '../../api/serviceStorefront';

const readMock = jest.fn<(id: string) => Promise<ServiceRefundRecovery>>();
const queryMock = jest.fn<(id: string) => Promise<ServiceRefundRecovery>>();
jest.unstable_mockModule('../../api/serviceStorefront', () => ({ ServiceStorefront: {
  readRefundRecovery: readMock, reconcileRefund: queryMock,
} }));
let language = 'en';
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ i18n: { resolvedLanguage: language } }) }));
const { default: HeldRefundRecoveryPanel } = await import('./HeldRefundRecoveryPanel');
const { default: axe } = await import('axe-core');
const refundId = '00000000-0000-4000-8000-000000000123';
const record = (overrides: Partial<ServiceRefundRecovery> = {}): ServiceRefundRecovery => ({
  ssrrRefundId: refundId, ssrrEnvironment: 'sandbox', ssrrStatus: 'processing',
  ssrrAmountMinor: '12515', ssrrCurrency: 'USD', ssrrCanQuery: true,
  ssrrOutcome: 'not_queried', ssrrCheckedAt: null, ...overrides,
});
const inspect = async () => {
  fireEvent.change(screen.getByRole('textbox'), { target: { value: refundId } });
  fireEvent.click(screen.getByRole('button', { name: 'Inspect local status' }));
  await screen.findByText('Amount: USD 125.15');
};
beforeEach(() => {
  language = 'en'; readMock.mockReset().mockResolvedValue(record());
  queryMock.mockReset().mockResolvedValue(record({ ssrrOutcome: 'held', ssrrCheckedAt: '2026-09-16T16:00:00Z' }));
});
afterEach(cleanup);

it('makes no automatic request and requires local readiness before a remote query', async () => {
  render(<HeldRefundRecoveryPanel />);
  expect(readMock).not.toHaveBeenCalled(); expect(queryMock).not.toHaveBeenCalled();
  expect(screen.queryByRole('button', { name: 'Check original refund' })).toBeNull();
  await inspect();
  expect(readMock).toHaveBeenCalledWith(refundId); expect(queryMock).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Check original refund' }));
  await screen.findByText(/Funds stay reserved/);
  expect(queryMock).toHaveBeenCalledTimes(1); expect(queryMock).toHaveBeenCalledWith(refundId);
});

it('does not display an unavailable query operation', async () => {
  readMock.mockResolvedValue(record({ ssrrCanQuery: false }));
  render(<HeldRefundRecoveryPanel />); await inspect();
  expect(screen.queryByRole('button', { name: 'Check original refund' })).toBeNull();
  expect(screen.getByText(/Querying is not enabled/)).toBeTruthy();
  expect(queryMock).not.toHaveBeenCalled();
});

it('validates UUIDs before any request', async () => {
  render(<HeldRefundRecoveryPanel />);
  fireEvent.change(screen.getByRole('textbox'), { target: { value: '../other' } });
  fireEvent.click(screen.getByRole('button', { name: 'Inspect local status' }));
  await screen.findByText('Enter a valid refund UUID.');
  expect(readMock).not.toHaveBeenCalled();
});

it('prevents double submission while the original query is unresolved', async () => {
  let finish!: (value: ServiceRefundRecovery) => void;
  queryMock.mockImplementation(() => new Promise((resolve) => { finish = resolve; }));
  render(<HeldRefundRecoveryPanel />); await inspect();
  const button = screen.getByRole('button', { name: 'Check original refund' });
  fireEvent.click(button); fireEvent.click(button);
  expect(queryMock).toHaveBeenCalledTimes(1);
  expect(screen.getByRole('textbox').hasAttribute('disabled')).toBe(true);
  expect(screen.queryByText('Amount: USD 125.15')).toBeNull();
  finish(record({ ssrrStatus: 'succeeded', ssrrOutcome: 'completed', ssrrCanQuery: false,
    ssrrCheckedAt: '2026-09-16T16:00:00Z' }));
  await screen.findByText('Refund confirmed in local accounting.');
  expect(screen.queryByRole('button', { name: 'Check original refund' })).toBeNull();
});

it('hides stale evidence and sensitive messages after a failed query without retrying', async () => {
  queryMock.mockRejectedValue(new Error('synthetic-private-provider-token'));
  render(<HeldRefundRecoveryPanel />); await inspect();
  fireEvent.click(screen.getByRole('button', { name: 'Check original refund' }));
  await screen.findByText(/The refund could not be verified/);
  expect(screen.queryByText('Amount: USD 125.15')).toBeNull();
  expect(document.body.textContent).not.toContain('synthetic-private-provider-token');
  expect(queryMock).toHaveBeenCalledTimes(1);
  expect(screen.queryByRole('button', { name: 'Check original refund' })).toBeNull();
});

it.each([
  { ssrrRefundId: '00000000-0000-4000-8000-000000000999' },
  { ssrrAmountMinor: '9223372036854775808' },
  { ssrrCanQuery: true, ssrrStatus: 'failed' },
  { ssrrStatus: 'processing', ssrrOutcome: 'completed' },
  { ssrrStatus: 'succeeded', ssrrCanQuery: false, ssrrOutcome: 'held' },
  { ssrrCheckedAt: 'not-a-time' },
] as Partial<ServiceRefundRecovery>[])('rejects mismatched or inconsistent readiness: %j', async (invalid) => {
  readMock.mockResolvedValue(record(invalid));
  render(<HeldRefundRecoveryPanel />);
  fireEvent.change(screen.getByRole('textbox'), { target: { value: refundId } });
  fireEvent.click(screen.getByRole('button', { name: 'Inspect local status' }));
  await screen.findByText(/The refund could not be verified/);
  expect(screen.queryByRole('button', { name: 'Check original refund' })).toBeNull();
});

it('rejects a query response from a different environment', async () => {
  queryMock.mockResolvedValue(record({ ssrrEnvironment: 'production', ssrrStatus: 'succeeded',
    ssrrOutcome: 'completed', ssrrCanQuery: false }));
  render(<HeldRefundRecoveryPanel />); await inspect();
  fireEvent.click(screen.getByRole('button', { name: 'Check original refund' }));
  await screen.findByText(/The refund could not be verified/);
  expect(screen.queryByText('Refund confirmed in local accounting.')).toBeNull();
});

it('does not restore another refund view when the input changes', async () => {
  render(<HeldRefundRecoveryPanel />); await inspect();
  fireEvent.change(screen.getByRole('textbox'), { target: { value: 'another-refund' } });
  expect(screen.queryByText('Amount: USD 125.15')).toBeNull();
  expect(screen.queryByRole('button', { name: 'Check original refund' })).toBeNull();
});

it('discards an interrupted response after unmount', async () => {
  let finish!: (value: ServiceRefundRecovery) => void;
  readMock.mockImplementation(() => new Promise((resolve) => { finish = resolve; }));
  const mounted = render(<HeldRefundRecoveryPanel />);
  fireEvent.change(screen.getByRole('textbox'), { target: { value: refundId } });
  fireEvent.click(screen.getByRole('button', { name: 'Inspect local status' }));
  mounted.unmount(); finish(record());
  render(<HeldRefundRecoveryPanel />);
  await waitFor(() => expect(screen.queryByText('Amount: USD 125.15')).toBeNull());
  expect(queryMock).not.toHaveBeenCalled();
});

it('localizes the hold disclosure and preserves exact large amounts accessibly', async () => {
  language = 'es'; readMock.mockResolvedValue(record({ ssrrAmountMinor: '9223372036854775807', ssrrCanQuery: false }));
  const { container } = render(<HeldRefundRecoveryPanel />);
  fireEvent.change(screen.getByRole('textbox'), { target: { value: refundId } });
  fireEvent.click(screen.getByRole('button', { name: 'Revisar estado local' }));
  await screen.findByText('Monto: USD 92233720368547758.07');
  expect(screen.getByText(/Consultar no envía otro reembolso/)).toBeTruthy();
  const report = await axe.run(container);
  expect(report.violations.filter((violation) => violation.impact === 'critical' || violation.impact === 'serious')).toEqual([]);
});
