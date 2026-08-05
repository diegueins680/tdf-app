import { formatCurrency, formatDate } from './formatters';

describe('international formatters', () => {
  it('uses locale-specific money separators and ISO currency precision', () => {
    expect(formatCurrency(1234.56, 'EUR', 'de-DE')).toMatch(/1\.234,56/);
    expect(formatCurrency(1234, 'JPY', 'ja-JP')).not.toMatch(/[.,]00/);
  });

  it('renders the same UTC timestamp in the requested timezone', () => {
    const timestamp = '2026-08-05T16:00:00Z';
    const utc = formatDate(timestamp, { locale: 'en-US', timeZone: 'UTC' }, { hour: '2-digit', minute: '2-digit', hourCycle: 'h23' });
    const tokyo = formatDate(timestamp, { locale: 'en-US', timeZone: 'Asia/Tokyo' }, { hour: '2-digit', minute: '2-digit', hourCycle: 'h23' });
    expect(utc).toBe('16:00');
    expect(tokyo).toBe('01:00');
  });

  it('respects daylight-saving transitions', () => {
    const before = formatDate('2026-03-08T06:30:00Z', { locale: 'en-US', timeZone: 'America/New_York' }, { hour: 'numeric', minute: '2-digit' });
    const after = formatDate('2026-03-08T07:30:00Z', { locale: 'en-US', timeZone: 'America/New_York' }, { hour: 'numeric', minute: '2-digit' });
    expect(before).toContain('1:30');
    expect(after).toContain('3:30');
  });
});
