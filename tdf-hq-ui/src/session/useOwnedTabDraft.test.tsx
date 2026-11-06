import { act, renderHook } from '@testing-library/react';
import { advanceAuthSessionEpoch } from './authEvents';
import { useOwnedTabDraft } from './useOwnedTabDraft';

const empty = { bio: '' };
const parse = (value: unknown) => value && typeof value === 'object' && 'bio' in value && typeof value.bio === 'string' ? { bio: value.bio } : undefined;
beforeEach(() => { sessionStorage.clear(); });

it('restores edited metadata after remount and clears it after explicit discard', () => {
  const first = renderHook(() => useOwnedTabDraft(empty, { ownerId: 42, scope: 'cover', parse }));
  act(() => first.result.current.field('bio')[1]('Antes de recargar'));
  first.unmount();
  const restored = renderHook(() => useOwnedTabDraft(empty, { ownerId: 42, scope: 'cover', parse }));
  expect(restored.result.current.value.bio).toBe('Antes de recargar');
  expect(restored.result.current.restored).toBe(true);
  act(() => restored.result.current.discard());
  restored.unmount();
  expect(renderHook(() => useOwnedTabDraft(empty, { ownerId: 42, scope: 'cover', parse })).result.current.value).toEqual(empty);
});
it('rejects callbacks from an old auth occurrence and never hydrates another owner or editor', () => {
  const form = renderHook(({ ownerId, scope }) => useOwnedTabDraft(empty, { ownerId, scope, parse }), { initialProps: { ownerId: 42, scope: 'cover' } });
  act(() => form.result.current.field('bio')[1]('Private draft'));
  const oldSetter = form.result.current.field('bio')[1];
  advanceAuthSessionEpoch();
  act(() => oldSetter('Stale callback'));
  expect(form.result.current.value.bio).toBe('Private draft');
  form.rerender({ ownerId: 84, scope: 'cover' });
  expect(form.result.current.value).toEqual(empty);
  form.rerender({ ownerId: 42, scope: 'different-cover' });
  expect(form.result.current.value).toEqual(empty);
});

it('preserves a restored draft over a server baseline and revokes callbacks on unmount', () => {
  const first = renderHook(() => useOwnedTabDraft(empty, { ownerId: 42, scope: 'notes', parse }));
  act(() => first.result.current.setValue({ bio: 'Unsaved notes' }));
  const oldDiscard = first.result.current.discard;
  first.unmount();
  const next = renderHook(() => useOwnedTabDraft(empty, { ownerId: 42, scope: 'notes', parse }));
  act(() => { next.result.current.setBaseline({ bio: 'Old server notes' }); oldDiscard(); });
  expect(next.result.current.value.bio).toBe('Unsaved notes');
  next.unmount();
  expect(renderHook(() => useOwnedTabDraft(empty, { ownerId: 42, scope: 'notes', parse })).result.current.value.bio).toBe('Unsaved notes');
});
