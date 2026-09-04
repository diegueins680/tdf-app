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
});
