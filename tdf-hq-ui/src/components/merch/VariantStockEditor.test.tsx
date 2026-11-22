import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { MerchProduct } from '../../api/merch';

type Variant = NonNullable<MerchProduct['variants']>[number];
const patch = jest.fn<(...args: unknown[]) => Promise<Variant>>();
jest.unstable_mockModule('../../api/merch', () => ({ Merch: { updateVariantStock: patch } }));
const { default: Editor } = await import('./VariantStockEditor');
const initial: Variant = { id: 'variant-a', sku: 'SYNTHETIC', name: 'Synthetic variant', version: 7,
  stockOnHand: 12, stockReserved: 2, stockSold: 1, reorderThreshold: 3, active: true, stockMode: 'finite', priceMinor: 2500, currency: 'USD' };
let client: QueryClient;
const refresh = jest.fn();
const stock = () => screen.getByRole<HTMLInputElement>('textbox', { name: 'On hand' });
const threshold = () => screen.getByRole<HTMLInputElement>('textbox', { name: 'Low-stock alert' });
const save = () => screen.getByRole<HTMLButtonElement>('button', { name: 'Update stock' });
const edit = (input: HTMLElement, value: string) => fireEvent.change(input, { target: { value } });
const submit = () => fireEvent.submit(stock().closest('form')!);
function mount() {
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  const tree = (variant: Variant, storeId = 'store-a') => <QueryClientProvider client={client}><Editor storeId={storeId} variant={variant} language="en" onRefresh={refresh} /></QueryClientProvider>;
  const view = render(tree(initial));
  return (variant: Variant, storeId?: string) => view.rerender(tree(variant, storeId));
}
beforeEach(() => { patch.mockReset(); refresh.mockReset(); });
afterEach(() => { cleanup(); client.clear(); });

it('keeps raw invalid drafts across refresh, blocks submits, and requires review then a separate versioned save', async () => {
  const rerender = mount();
  const field = stock(); edit(field, '009'); edit(threshold(), ''); fireEvent.click(screen.getByRole('checkbox'));
  rerender({ ...initial, version: 8, stockOnHand: 15, reorderThreshold: 4 });
  expect(stock()).toBe(field); expect(field.value).toBe('009'); expect(threshold().value).toBe('');
  expect(screen.getByRole<HTMLInputElement>('checkbox').checked).toBe(false);
  expect(screen.getByRole('alert').textContent).toContain('on hand 15'); expect(save().disabled).toBe(true);
  submit(); expect(patch).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Keep my edits' }));
  expect(document.activeElement).toBe(field); expect(patch).not.toHaveBeenCalled();
  expect(threshold().value).toBe(''); expect(save().disabled).toBe(true); submit(); expect(patch).not.toHaveBeenCalled();
  edit(threshold(), '0');
  patch.mockResolvedValueOnce({ ...initial, version: 9, stockOnHand: 9, reorderThreshold: 0, active: false });
  fireEvent.click(save()); await waitFor(() => expect(refresh).toHaveBeenCalledTimes(1));
  expect(patch).toHaveBeenCalledWith('store-a', 'variant-a', { stockOnHand: 9, reorderThreshold: 0, active: false, version: 8 });
  expect(stock().value).toBe('9');
});

it('lets a clean editor follow a newer snapshot without remounting or moving focus', async () => {
  const rerender = mount(), field = stock(); act(() => field.focus());
  rerender({ ...initial, version: 8, stockOnHand: 20, reorderThreshold: 0, active: false });
  await waitFor(() => expect(field.value).toBe('20'));
  expect(stock()).toBe(field); expect(document.activeElement).toBe(field);
  expect(threshold().value).toBe('0'); expect(screen.getByRole<HTMLInputElement>('checkbox').checked).toBe(false);
  expect(patch).not.toHaveBeenCalled();
});

it('discards a draft only through the explicit current-values choice, without an API write', () => {
  const rerender = mount(); edit(stock(), '09');
  rerender({ ...initial, version: 8, stockOnHand: 15, reorderThreshold: 4, active: false });
  fireEvent.click(screen.getByRole('button', { name: 'Use current values' }));
  expect(stock().value).toBe('15'); expect(threshold().value).toBe('4'); expect(document.activeElement).toBe(stock());
  expect(screen.getByRole<HTMLInputElement>('checkbox').checked).toBe(false); expect(screen.queryByRole('alert')).toBeNull();
  expect(patch).not.toHaveBeenCalled();
});

it('retains a rejected draft and never retries or upgrades its version automatically after conflict refresh', async () => {
  const rerender = mount(); edit(stock(), '09'); edit(threshold(), '0');
  patch.mockRejectedValueOnce(new Error('synthetic conflict')); fireEvent.click(save());
  await screen.findByRole('alert'); expect(refresh).toHaveBeenCalledTimes(1); expect(stock().value).toBe('09');
  expect(patch).toHaveBeenCalledWith('store-a', 'variant-a', expect.objectContaining({ version: 7 }));
  rerender({ ...initial, version: 8, stockOnHand: 18 });
  submit(); expect(patch).toHaveBeenCalledTimes(1); expect(stock().value).toBe('09');
  expect(screen.getAllByRole('alert')).toHaveLength(1);
  expect(screen.getByRole('alert').textContent).toContain('Stock changed while you were editing');
  fireEvent.click(screen.getByRole('button', { name: 'Keep my edits' })); expect(patch).toHaveBeenCalledTimes(1);
  patch.mockResolvedValueOnce({ ...initial, version: 9, stockOnHand: 9, reorderThreshold: 0 });
  fireEvent.click(save()); await waitFor(() => expect(patch).toHaveBeenCalledTimes(2));
  expect(patch).toHaveBeenLastCalledWith('store-a', 'variant-a', expect.objectContaining({ version: 8 }));
});

it('locks a pending save, then follows a still newer server snapshot without issuing another write', async () => {
  const rerender = mount(); let accept!: (value: Variant) => void;
  patch.mockImplementationOnce(() => new Promise(resolve => { accept = resolve; }));
  edit(stock(), '9'); fireEvent.click(save()); await waitFor(() => expect(stock().disabled).toBe(true));
  rerender({ ...initial, version: 9, stockOnHand: 21 });
  expect(stock().value).toBe('9'); expect(threshold().disabled).toBe(true);
  expect(screen.getByRole<HTMLInputElement>('checkbox').disabled).toBe(true);
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Keep my edits' }).disabled).toBe(true);
  submit(); expect(patch).toHaveBeenCalledTimes(1);
  await act(async () => accept({ ...initial, version: 8, stockOnHand: 9 }));
  await waitFor(() => expect(stock().value).toBe('21')); expect(patch).toHaveBeenCalledTimes(1);
});

it('does not roll an accepted response back to an older query, and protects the next draft too', async () => {
  const rerender = mount(); patch.mockResolvedValueOnce({ ...initial, version: 8, stockOnHand: 9 });
  edit(stock(), '9'); fireEvent.click(save()); await waitFor(() => expect(refresh).toHaveBeenCalledTimes(1));
  rerender({ ...initial }); expect(stock().value).toBe('9'); expect(screen.queryByRole('alert')).toBeNull();
  edit(stock(), '06'); rerender({ ...initial, version: 9, stockOnHand: 11 });
  expect(stock().value).toBe('06'); expect(save().disabled).toBe(true); expect(patch).toHaveBeenCalledTimes(1);
});

it.each(['store', 'variant'])('resets the draft at the %s identity boundary', boundary => {
  const rerender = mount(); edit(stock(), '009');
  rerender({ ...initial, id: boundary === 'variant' ? 'variant-b' : initial.id, stockOnHand: 20 }, boundary === 'store' ? 'store-b' : 'store-a');
  expect(stock().value).toBe('20'); expect(patch).not.toHaveBeenCalled();
});
