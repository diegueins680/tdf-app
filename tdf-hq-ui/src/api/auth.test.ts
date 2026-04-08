import { jest } from '@jest/globals';
import { confirmPasswordReset } from './auth';

describe('auth api', () => {
  const fetchMock = jest.fn<typeof fetch>();

  beforeEach(() => {
    fetchMock.mockReset();
    (globalThis as unknown as { fetch: typeof fetch }).fetch = fetchMock;
  });

  it('posts password reset confirmations to the v1 confirm endpoint', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      json: jest.fn<() => Promise<unknown>>().mockResolvedValue({
        token: 'session-token',
        partyId: 45,
        roles: ['Fan'],
        modules: ['CRM'],
      }),
    } as unknown as Response);

    const result = await confirmPasswordReset({
      token: 'reset-token',
      newPassword: 'Password123',
    });

    expect(fetchMock).toHaveBeenCalledWith('/v1/password-reset/confirm', {
      method: 'POST',
      credentials: 'include',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        token: 'reset-token',
        newPassword: 'Password123',
      }),
    });
    expect(result).toEqual({
      token: 'session-token',
      partyId: 45,
      roles: ['Fan'],
      modules: ['CRM'],
    });
  });

  it('surfaces the backend error message when reset confirmation fails', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: false,
      text: jest.fn<() => Promise<string>>().mockResolvedValue('Invalid or expired token'),
    } as unknown as Response);

    await expect(
      confirmPasswordReset({ token: 'expired-token', newPassword: 'Password123' }),
    ).rejects.toThrow('Invalid or expired token');
  });
});
