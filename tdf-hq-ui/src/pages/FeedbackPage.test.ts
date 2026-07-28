import { contactEmailFromSessionUsername } from './FeedbackPage';

describe('FeedbackPage contact email', () => {
  it('prefills email-shaped account usernames', () => {
    expect(contactEmailFromSessionUsername('diego@example.com')).toBe('diego@example.com');
  });

  it('does not submit account handles as contact email addresses', () => {
    expect(contactEmailFromSessionUsername('admin')).toBe('');
    expect(contactEmailFromSessionUsername()).toBe('');
  });
});
