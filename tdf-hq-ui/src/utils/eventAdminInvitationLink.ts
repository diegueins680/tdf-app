/** The only accepted shape of a delegated event-administrator invitation token. */
export const EVENT_ADMIN_INVITATION_TOKEN = /^[0-9a-f]{64}$/i;

const HASH_PREFIX = '#admin=';

/**
 * Invitation links open the public event page: `/eventos/<eventId>#admin=<token>`.
 * The single-use token is a URL fragment so it never reaches server or proxy logs.
 */
export const buildEventAdminInvitationLink = (origin: string, eventId: string, token: string) =>
  `${origin}${buildEventAdminInvitationPath(eventId, token)}`;

export const buildEventAdminInvitationPath = (eventId: string, token: string) =>
  `/eventos/${encodeURIComponent(eventId)}${HASH_PREFIX}${token}`;

export const readEventAdminInvitationHash = (hash: string): string | null => {
  if (!hash.startsWith(HASH_PREFIX)) return null;
  const candidate = hash.slice(HASH_PREFIX.length);
  return EVENT_ADMIN_INVITATION_TOKEN.test(candidate) ? candidate : null;
};
