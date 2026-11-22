import { sessionContactDefaults } from './sessionContactDefaults';

describe('sessionContactDefaults', () => {
  it('returns empty values without a session', () => {
    expect(sessionContactDefaults(null)).toEqual({ name: '', email: '' });
    expect(sessionContactDefaults(undefined)).toEqual({ name: '', email: '' });
  });

  it('offers the display name and an email-shaped username', () => {
    expect(sessionContactDefaults({ displayName: '  Elena Paredes ', username: ' elena@example.com ' }))
      .toEqual({ name: 'Elena Paredes', email: 'elena@example.com' });
  });

  it('does not offer a handle as the email', () => {
    expect(sessionContactDefaults({ displayName: 'Elena', username: 'elena.p' })).toEqual({ name: 'Elena', email: '' });
  });
});
