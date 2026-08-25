import { renderHook } from '@testing-library/react';
import { useAutoSave } from './useAutoSave';

describe('useAutoSave', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('returns no draft when stored JSON is malformed', () => {
    window.localStorage.setItem('draft:settings', '{not valid json');

    const { result } = renderHook(() =>
      useAutoSave('settings', { theme: 'dark' }, { enabled: false }),
    );

    expect(result.current.loadDraft()).toBeNull();
  });
});
