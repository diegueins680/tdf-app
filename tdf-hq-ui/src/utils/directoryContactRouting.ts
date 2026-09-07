import { buildLoginRedirectPath } from './loginRouting';

const CONTACT_RESUME_ACTION = 'contact';

export function buildDirectoryContactLoginPath(pathname: string, profileId: string): string {
  const normalizedProfileId = profileId.trim();
  if (!normalizedProfileId) return '/login';

  const params = new URLSearchParams({
    resume: CONTACT_RESUME_ACTION,
    profileId: normalizedProfileId,
  });
  const loginPath = buildLoginRedirectPath(`${pathname}?${params.toString()}`);
  if (loginPath === '/login') return loginPath;

  const loginParams = new URLSearchParams(loginPath.slice('/login?'.length));
  loginParams.set('intent', 'professional_tools');
  return `/login?${loginParams.toString()}`;
}

export function shouldResumeDirectoryContact(search: string, profileId: string): boolean {
  const normalizedProfileId = profileId.trim();
  if (!normalizedProfileId) return false;

  const params = new URLSearchParams(search);
  return params.get('resume') === CONTACT_RESUME_ACTION
    && params.get('profileId') === normalizedProfileId;
}
