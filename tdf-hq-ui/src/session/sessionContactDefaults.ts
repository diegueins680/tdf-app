const EMAIL_PATTERN = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

interface SessionContactSource {
  displayName?: string | null;
  username?: string | null;
}

/**
 * What a signed-in visitor's account already knows about them, for prefilling
 * public contact forms. Both values stay editable in the form; the username is
 * offered as the email only when it has the shape of one.
 */
export function sessionContactDefaults(session: SessionContactSource | null | undefined): { name: string; email: string } {
  const username = session?.username?.trim() ?? '';
  return {
    name: session?.displayName?.trim() ?? '',
    email: EMAIL_PATTERN.test(username) ? username : '',
  };
}
