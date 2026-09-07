import {
  buildDirectoryContactLoginPath,
  shouldResumeDirectoryContact,
} from './directoryContactRouting';

describe('directory contact routing', () => {
  it('preserves a target-bound internal contact destination through login', () => {
    const loginPath = buildDirectoryContactLoginPath('/directorio/perfil/ana', 'profile-17');
    const loginUrl = new URL(loginPath, 'https://tdf.local');

    expect(loginUrl.pathname).toBe('/login');
    expect(loginUrl.searchParams.get('intent')).toBe('professional_tools');
    expect(loginUrl.searchParams.get('redirect')).toBe(
      '/directorio/perfil/ana?resume=contact&profileId=profile-17',
    );
  });

  it('rejects external, empty, and login-loop destinations', () => {
    expect(buildDirectoryContactLoginPath('//example.com/steal', 'profile-17')).toBe('/login');
    expect(buildDirectoryContactLoginPath('/login', 'profile-17')).toBe('/login');
    expect(buildDirectoryContactLoginPath('/directorio/perfil/ana', '   ')).toBe('/login');
  });

  it('offers resume only for the exact loaded profile target', () => {
    const search = '?resume=contact&profileId=profile-17';

    expect(shouldResumeDirectoryContact(search, 'profile-17')).toBe(true);
    expect(shouldResumeDirectoryContact(search, 'profile-18')).toBe(false);
    expect(shouldResumeDirectoryContact('?resume=contact&profileId=', 'profile-17')).toBe(false);
    expect(shouldResumeDirectoryContact('?resume=apply&profileId=profile-17', 'profile-17')).toBe(false);
  });
});
