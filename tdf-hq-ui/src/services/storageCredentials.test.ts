import { retireLegacyDriveCredentials } from './storageCredentials';

it('erases provider secrets for inactive accounts while preserving nonsecret descriptors and form drafts', () => {
  localStorage.clear();
  localStorage.setItem('tdf-google-drive-token', JSON.stringify({ accessToken: 'unbound-secret' }));
  localStorage.setItem('tdf-google-drive-token:10', JSON.stringify({ accessToken: 'inactive-secret', refreshToken: 'inactive-refresh' }));
  localStorage.setItem('tdf-google-drive-token:11', 'malformed');
  const descriptor = { ownerPartyId: 12, accessToken: 'tdf-vault-v1_00000000-0000-4000-8000-000000000012', expiresAt: 123 };
  localStorage.setItem('tdf-google-drive-token:12', JSON.stringify(descriptor));
  localStorage.setItem('tdf-google-drive-token:13', JSON.stringify({ ...descriptor, refreshToken: 'raw-provider-refresh' }));
  localStorage.setItem('unrelated-form-draft', 'keep');
  retireLegacyDriveCredentials();
  for (const key of ['tdf-google-drive-token', 'tdf-google-drive-token:10', 'tdf-google-drive-token:11', 'tdf-google-drive-token:13']) {
    expect(localStorage.getItem(key)).toBeNull();
  }
  expect(JSON.parse(localStorage.getItem('tdf-google-drive-token:12') ?? 'null')).toEqual(descriptor);
  expect(localStorage.getItem('unrelated-form-draft')).toBe('keep');
});
