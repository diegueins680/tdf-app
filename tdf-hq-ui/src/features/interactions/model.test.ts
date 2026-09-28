import { appendMention, reconcileMentions, optimisticReaction } from './model';
import type { InteractionSummary } from '../../api/interactions';

describe('mention identity transitions', () => {
  test('uses Unicode code points across emoji and preserves unchanged bindings', () => {
    const mention = { partyId: 42, start: 2, end: 6 };
    expect(reconcileMentions('🔥 @Ana', '🔥 wow @Ana', [mention])).toEqual([{ ...mention, start: 6, end: 10 }]);
    expect(reconcileMentions('🔥 @Ana', '🔥 @Eva', [mention])).toEqual([]);
    expect(reconcileMentions('🔥 @Ana', '🔥 @Anabel', [mention])).toEqual([]);
    expect(reconcileMentions('🔥 @Ana', '🔥 @Anañ', [mention])).toEqual([]);
    expect(reconcileMentions('🔥 @Ana', '🔥 @Ana hi', [mention])).toEqual([mention]);
  });
  test('generated prefix edits never rebind a stable identity to different text', () => {
    for (let length = 0; length < 200; length++) {
      const before = '🎵'.repeat(length) + ' @Persona';
      const mention = { partyId: 9, start: length + 1, end: length + 9 };
      const after = `New ${before}`;
      for (const kept of reconcileMentions(before, after, [mention])) {
        expect([...after].slice(kept.start, kept.end).join('')).toBe('@Persona');
        expect(kept.partyId).toBe(9);
      }
      expect(reconcileMentions(before, before.slice(0, -1) + 'X', [mention])).toEqual([]);
    }
  });
});

test('generated reaction transitions preserve one actor slot and reconcile reversible counts', () => {
  const original = { myReactionTypeId: null, reactions: ['like', 'heart', 'fire', 'clap'].map((id) => ({ id, count: 11 })) } as InteractionSummary;
  let state = original;
  let seed = 991;
  for (let i = 0; i < 2000; i++) {
    seed = (seed * 1664525 + 1013904223) >>> 0;
    const next = [null, 'like', 'heart', 'fire', 'clap'][seed % 5]!;
    state = optimisticReaction(state, next);
    expect(state.reactions.reduce((sum, reaction) => sum + reaction.count, 0)).toBe(44 + Number(next !== null));
    expect(optimisticReaction(state, next)).toEqual(state);
    expect(optimisticReaction(state, null)).toEqual(original);
  }
});

test('mention autocomplete replaces typed queries without corrupting Unicode identities', () => {
  const inserted = appendMention('🔥 Hello @an', [], 42, 'ana');
  expect(inserted.body).toBe('🔥 Hello @ana ');
  expect(inserted.mentions).toEqual([{ partyId: 42, start: 8, end: 12 }]);
  expect(appendMention(inserted.body, inserted.mentions, 9, 'Luis').mentions[0]).toEqual(inserted.mentions[0]);
});
