import { readdirSync, readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

const readPage = (name: string) => readFileSync(new URL(`./${name}`, import.meta.url), 'utf8');

const collectTsxFiles = (directory: string): string[] => readdirSync(directory, { withFileTypes: true })
  .flatMap((entry) => {
    const path = `${directory}/${entry.name}`;
    if (entry.isDirectory()) return entry.name === 'generated' ? [] : collectTsxFiles(path);
    return entry.isFile() && entry.name.endsWith('.tsx') && !entry.name.includes('.test.') ? [path] : [];
  });

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

  it('does not expose an assignee Party ID in the operations inbox', () => {
    const source = readPage('OperationsControlCenterPage.tsx');
    expect(source).not.toContain('#${item.assigneePartyId}');
    expect(source).toContain("'Responsable asignado'");
  });

  it('uses privacy-safe fallbacks when a related Party cannot be resolved', () => {
    expect(readPage('ChatPage.tsx')).not.toMatch(/(?:Party|Perfil)\s*#\$\{/);
    expect(readPage('SocialPage.tsx')).not.toContain('Perfil #${partyId}');
    expect(readPage('EventLogisticsPage.tsx')).not.toContain('Usuario ${member.elmPartyId}');
    expect(readPage('FanClubMemberProfilePage.tsx')).not.toContain('@miembro-${profile.fcmpPartyId}');
  });

  it('scopes logistics discovery to the event authorization boundary', () => {
    const source = readPage('EventLogisticsPage.tsx');
    expect(source.match(/context: 'event_logistics', scopeId: eventId/g)).toHaveLength(2);
    expect(source).not.toContain("context: 'crm_assignment'");
  });

  it('keeps visible Party-number references on a closed administrative allowlist', () => {
    const srcRoot = fileURLToPath(new URL('../', import.meta.url));
    const visibleTechnicalIdFiles = collectTsxFiles(srcRoot)
      .filter((file) => /(?:Party|Perfil)\s*#\$\{/.test(readFileSync(file, 'utf8')))
      .map((file) => file.slice(srcRoot.length).replace(/^\//, ''))
      .sort();

    expect(visibleTechnicalIdFiles).toEqual([
      'pages/AdminTokenPage.tsx',
      'pages/AdminUsersPage.tsx',
      'pages/CourseRegistrationsAdminPage.tsx',
      'pages/LabelTracksPage.tsx',
    ]);
  });

  it('requires every production selector instance to declare an authorization context', () => {
    const srcRoot = fileURLToPath(new URL('../', import.meta.url));
    const selectorImplementation = `${srcRoot}/components/party-selector/PartySelector.tsx`;
    const missingContext = collectTsxFiles(srcRoot)
      .filter((file) => file !== selectorImplementation)
      .flatMap((file) => {
        const source = readFileSync(file, 'utf8');
        const selectorCount = source.match(/<(?:PartySelector|UserSelector|PartyMultiSelector)\b/g)?.length ?? 0;
        const contextCount = source.match(/search=\{\{\s*context:/g)?.length ?? 0;
        return selectorCount === contextCount ? [] : [file.slice(srcRoot.length).replace(/^\//, '')];
      });

    expect(missingContext).toEqual([]);
  });
});
