import { parseUploadFailure, uploadFailureMessage } from './uploadFailure';

it('distinguishes provider reconnection from a shared TDF outage', () => {
  const raw = 'Refresh token expirado o revocado. Reautoriza Google Drive. synthetic-secret';
  expect(parseUploadFailure(401, raw, 'user').recovery).toBe('reconnect');
  const service = parseUploadFailure(503, raw, 'server');
  expect(service.recovery).toBe('retry');
  expect(uploadFailureMessage(service)).not.toContain('synthetic-secret');
  expect(uploadFailureMessage(service)).not.toContain('Reautoriza');
  const clientConfiguration = parseUploadFailure(503, '{"code":"storage_service_unavailable"}', 'user');
  expect(clientConfiguration.code).toBe('storage_service_unavailable');
  expect(clientConfiguration.recovery).toBe('retry');
  expect(uploadFailureMessage(clientConfiguration)).toContain('almacenamiento de TDF');
});
it('keeps quota, network uncertainty and permissions distinct', () => {
  expect(parseUploadFailure(413, '{"code":"storage_quota_exceeded"}', 'server').code).toBe('storage_quota_exceeded');
  expect(parseUploadFailure(0, '', 'user').code).toBe('storage_upload_uncertain');
  expect(parseUploadFailure(403, '', 'user').recovery).toBe('none');
});
it('provides both languages without reflecting untrusted payloads', () => {
  const failure = parseUploadFailure(502, '<script>synthetic-secret</script>', 'server');
  expect(uploadFailureMessage(failure, true)).toContain('TDF storage');
  expect(uploadFailureMessage(failure)).toContain('almacenamiento de TDF');
});
