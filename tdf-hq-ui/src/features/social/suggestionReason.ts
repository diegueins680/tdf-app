import type { SuggestedFriendDTO } from '../../api/types';

/** Reason value of an entry ranked by the follow network (COMMUNITY-FRIEND-SUGGESTION-001). */
export const NETWORK_SUGGESTION_REASON = 'network';

/**
 * Text shown under a suggested person. A network-ranked entry never shows a
 * count: its sfMutualCount is a placeholder, and the server discloses no number
 * derived from other people's follows.
 */
export function suggestionReasonLabel(
  suggestion: Pick<SuggestedFriendDTO, 'sfMutualCount' | 'sfReason'>,
): string {
  if (suggestion.sfReason === NETWORK_SUGGESTION_REASON) return 'Cerca de tu red';
  const count = suggestion.sfMutualCount;
  return count === 1 ? '1 conexión en común' : `${count} conexiones en común`;
}
