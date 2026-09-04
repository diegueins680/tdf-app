import { readFileSync } from 'node:fs';

const readPage = (name: string) => readFileSync(new URL(`./${name}`, import.meta.url), 'utf8');

describe('Party relationship picker migration', () => {
  it('uses a remote account selector for event collaborators', () => {
    const source = readPage('CollaborativeEventCreatorPage.tsx');
    expect(source).toContain('<UserSelector');
    expect(source).not.toMatch(/Parties\.list\s*\(/);
  });

  it('loads full musician details only after a canonical selection', () => {
    const source = readPage('LiveSessionIntakePage.tsx');
    expect(source).toContain('<PartySelector');
    expect(source).toContain('Parties.getOne(party.partyId)');
    expect(source).not.toMatch(/Parties\.list\s*\(/);
  });

  it('does not accept typed Party IDs for new chats or friendships', () => {
    const chatSource = readPage('ChatPage.tsx');
    const socialSource = readPage('SocialPage.tsx');
    expect(chatSource).not.toContain('parsePositiveInt(newChatInput)');
    expect(chatSource).not.toMatch(/Parties\.list\s*\(/);
    expect(socialSource).toContain('<UserSelector');
    expect(socialSource).not.toContain('parsePositivePartyId(addId)');
    expect(socialSource).not.toContain('label="Party ID"');
  });

  it('uses the booking display name instead of downloading the Party directory for orders', () => {
    const source = readPage('OrdersPage.tsx');
    expect(source).toContain('booking.partyDisplayName');
    expect(source).not.toMatch(/Parties\.list\s*\(?/);
    expect(source).not.toContain("from '../api/parties'");
  });

  it('uses the dedicated teacher projection instead of filtering the Party directory', () => {
    const source = readPage('TeachersPage.tsx');
    expect(source).toContain('Trials.listTeachers');
    expect(source).not.toMatch(/Parties\.list\s*\(?/);
    expect(source).not.toContain("from '../api/parties'");
  });

  it('loads CRM notes only for visible label artists instead of downloading the Party directory', () => {
    const source = readPage('LabelArtistsPage.tsx');
    expect(source).toContain('Parties.getOne(artist.apArtistId)');
    expect(source).not.toMatch(/Parties\.list\s*\(?/);
  });
});
