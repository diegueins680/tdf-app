import { suggestionReasonLabel } from './suggestionReason';

describe('suggestionReasonLabel', () => {
  it('keeps the mutual-count text when the entry carries no reason', () => {
    expect(suggestionReasonLabel({ sfMutualCount: 1 })).toBe('1 conexión en común');
    expect(suggestionReasonLabel({ sfMutualCount: 3 })).toBe('3 conexiones en común');
    expect(suggestionReasonLabel({ sfMutualCount: 0 })).toBe('0 conexiones en común');
  });

  it('shows no count for an entry ranked by the follow network', () => {
    const label = suggestionReasonLabel({ sfMutualCount: 0, sfReason: 'network' });
    expect(label).toBe('Cerca de tu red');
    expect(label).not.toMatch(/\d/);
  });

  it('treats an unknown reason as the mutual-count answer', () => {
    expect(suggestionReasonLabel({ sfMutualCount: 2, sfReason: 'other' })).toBe('2 conexiones en común');
  });
});
