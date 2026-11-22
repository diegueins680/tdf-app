import { directoryStatusLabel } from './directoryStatusLabel';

describe('directoryStatusLabel', () => {
  it('names the known statuses in Spanish', () => {
    expect(directoryStatusLabel('published')).toBe('Publicado');
    expect(directoryStatusLabel('conversation_open')).toBe('Conversación abierta');
    expect(directoryStatusLabel(' pending ')).toBe('Pendiente');
  });

  it('shows an unknown status as received and tolerates a missing one', () => {
    expect(directoryStatusLabel('on_hold')).toBe('on_hold');
    expect(directoryStatusLabel(undefined)).toBe('');
    expect(directoryStatusLabel('constructor')).toBe('constructor');
  });
});
