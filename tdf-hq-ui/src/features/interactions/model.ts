import type { InteractionCommand, InteractionMention, InteractionSummary } from '../../api/interactions';

/** Unicode code-point offsets match PostgreSQL length/substring and native text. */
export function reconcileMentions(previous: string, next: string, mentions: InteractionMention[]): InteractionMention[] {
  const before = [...previous]; const after = [...next];
  let prefix = 0; let suffix = 0;
  while (prefix < before.length && prefix < after.length && before[prefix] === after[prefix]) prefix++;
  while (suffix < before.length - prefix && suffix < after.length - prefix
    && before[before.length - suffix - 1] === after[after.length - suffix - 1]) suffix++;
  const delta = after.length - before.length;
  return mentions.flatMap((mention) => {
    if (mention.end <= prefix) {
      // Extending a username must not silently keep the old person's binding.
      if (mention.end === prefix && /[\p{L}\p{N}_]/u.test(after[prefix] ?? '')) return [];
      return [mention];
    }
    if (mention.start >= before.length - suffix) return [{ ...mention, start: mention.start + delta, end: mention.end + delta }];
    return []; // Editing any part of a mention removes its identity binding.
  });
}

/** A typed @query is replaced atomically by the selected stable identity. */
export const mentionQuery = (body: string): string | undefined => /(?:^|\s)@([\p{L}\p{N}_.-]*)$/u.exec(body)?.[1];
export function appendMention(body: string, mentions: InteractionMention[], partyId: number, label: string) {
  const query = mentionQuery(body);
  const prefix = query === undefined ? body : body.slice(0, body.length - query.length - 1);
  const separator = prefix && !/\s$/.test(prefix) ? ' ' : '';
  const text = `@${label}`; const start = [...prefix].length + separator.length;
  const next = `${prefix}${separator}${text} `;
  return { body: next, mentions: [...reconcileMentions(body, prefix, mentions), { partyId, start, end: start + [...text].length }] };
}

export function optimisticReaction(summary: InteractionSummary, reactionTypeId: string | null): InteractionSummary {
  return { ...summary, myReactionTypeId: reactionTypeId, reactions: summary.reactions.map((reaction) => ({
    ...reaction, count: Math.max(0, reaction.count + Number(reaction.id === reactionTypeId) - Number(reaction.id === summary.myReactionTypeId)),
  })) };
}

export const discussionLink = (kind: 'comment' | 'target', id: string) => `/conversacion/${kind}/${encodeURIComponent(id)}`;

// Only disclosure state is retained, scoped by account. No text/private responses
// are persisted. Bounded memory avoids a growing cache on long feed sessions.
const disclosures = new Map<string, boolean>();
export const getDisclosure = (key: string) => disclosures.get(key) ?? false;
export function setDisclosure(key: string, expanded: boolean) {
  disclosures.delete(key); disclosures.set(key, expanded);
  if (disclosures.size > 256) disclosures.delete(disclosures.keys().next().value!);
}

/** Analytics deliberately contain no body, names, mention text or external URL. */
export function commandAnalyticsEvents(command: InteractionCommand): string[] {
  if (command.operation === 'comment.create') return [command.parentId ? 'reply_created' : 'comment_created', ...(command.mentions?.length ? ['mention_created'] : [])];
  if (command.operation === 'comment.edit') return ['comment_edited'];
  if (command.operation === 'comment.delete') return ['comment_deleted'];
  return [command.operation.replace(/\./g, '_')];
}

/** Keep only five result pages live. Cursor history contains no comment content and
 * lets the user return to discarded pages without fetching the intervening tree. */
export const discussionWindowPages = 5;
export function createDiscussionCursorHistory() {
  const previous = new Map<string, string>();
  return {
    remember(cursor: string, nextCursor: string | null | undefined) {
      if (nextCursor) previous.set(nextCursor, cursor);
    },
    previous(cursor: string) { return cursor ? previous.get(cursor) : undefined; },
  };
}

/** Following an audit creator is not following an institutional publication. */
export const commentPolicyAvailable = (policy: InteractionSummary['commentPolicy'], ownerId: number | null) =>
  policy !== 'followers' || ownerId !== null;
