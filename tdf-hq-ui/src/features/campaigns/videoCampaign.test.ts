import { campaignLink, createCampaignPieces, emptyVideoCampaign, importedCampaignSchema, prepareMissingCampaignPieces, safeVideoUrl, validateVideoCampaign } from './videoCampaign';

describe('artist video campaigns', () => {
  it('builds inert organic plans with attribution per channel and piece', () => {
    const pieces = createCampaignPieces('Liquid Paper Girl', ['instagram', 'tiktok']);
    expect(pieces).toHaveLength(14);
    expect(Math.max(...pieces.map((p) => p.day)) - Math.min(...pieces.map((p) => p.day)) + 1).toBe(14);
    expect(pieces.every((p) => p.status === 'draft' && !p.publishedUrl && p.day <= 13)).toBe(true);
    expect(new Set(pieces.map((p) => p.id)).size).toBe(14);
    const url = new URL(campaignLink(51, 'campaign', pieces[0]!));
    expect(url.pathname).toBe('/a/51');
    expect(url.searchParams.get('utm_content')).toBe('instagram-1');
    expect(url.searchParams.get('utm_medium')).toBe('organic_social');
  });
  it('accepts unfinished drafts but refuses review-ready campaigns without files', () => {
    const doc = emptyVideoCampaign();
    expect(validateVideoCampaign(doc)).toBeNull();
    doc.status = 'ready';
    expect(validateVideoCampaign(doc)).not.toBeNull();
    doc.videoUrl = 'https://cdn.example.com/session.mp4';
    doc.pieces = createCampaignPieces('Artist', ['instagram']);
    expect(validateVideoCampaign(doc)).toMatch(/archivos/);
    doc.pieces.forEach((p) => { p.assetUrl = 'https://cdn.example.com/clip.mp4'; });
    expect(validateVideoCampaign(doc)).toBeNull();
  });
  it('rejects unsafe media and cuts beyond the original video', () => {
    for (const url of ['javascript:alert(1)', 'http://example.com/video', 'https://127.0.0.1/a.mp4', 'https://user:password@example.com/x']) expect(safeVideoUrl(url)).toBe(false);
    const doc = emptyVideoCampaign(); doc.durationSeconds = 10;
    doc.pieces = createCampaignPieces('Artist', ['instagram']);
    doc.pieces[0]!.sourceEnd = 11;
    expect(validateVideoCampaign(doc)).toMatch(/duración/);
  });
  it('does not equate export or a link on the wrong platform with publication', () => {
    const doc = emptyVideoCampaign(); doc.pieces = createCampaignPieces('Artist', ['instagram']);
    const p = doc.pieces[0]!; p.status = 'published_manual';
    expect(validateVideoCampaign(doc)).toMatch(/URL/);
    p.publishedUrl = 'https://www.tiktok.com/@artist/video/123';
    expect(validateVideoCampaign(doc)).toMatch(/red/);
    p.publishedUrl = 'https://www.instagram.com/reel/abc/';
    expect(validateVideoCampaign(doc)).toBeNull();
  });
  it('rejects corrupt imported plans before touching the server', () => {
    expect(importedCampaignSchema.safeParse({ ...emptyVideoCampaign(), artistId: 51 }).success).toBe(false);
    expect(importedCampaignSchema.safeParse({ ...emptyVideoCampaign(), artistId: 51,
      campaignId: '126f208c-2b38-47c1-952b-6ba633122c87' }).success).toBe(true);
  });
  it('adds only missing channels, retains edits and avoids imported ID collisions and repeated generation', () => {
    const existing = createCampaignPieces('Artist', ['instagram']);
    existing[0]!.id = 'tiktok-1';
    existing[0]!.caption = 'Mi texto revisado';
    const prepared = prepareMissingCampaignPieces('Artist', ['instagram', 'tiktok'], existing);
    expect(prepared).toHaveLength(14);
    expect(prepared.slice(0, 7)).toEqual(existing);
    expect(new Set(prepared.map((p) => p.id)).size).toBe(14);
    expect(prepareMissingCampaignPieces('Artist', ['instagram', 'tiktok'], prepared)).toEqual(prepared);
  });
  it('requires IDs that survive attribution and the authentication return path', () => {
    const doc = emptyVideoCampaign();
    doc.pieces = createCampaignPieces('Artist', ['instagram']);
    doc.pieces[0]!.id = 'sesión 1';
    expect(validateVideoCampaign(doc)).toMatch(/identificador/);
    expect(importedCampaignSchema.safeParse({ ...doc, artistId: 51,
      campaignId: '126f208c-2b38-47c1-952b-6ba633122c87' }).success).toBe(false);
    doc.pieces[0]!.id = 'clip_1-A';
    expect(validateVideoCampaign(doc)).toBeNull();
    const url = new URL(campaignLink(51, 'campaign', doc.pieces[0]!));
    expect(url.searchParams.get('utm_content')).toBe('clip_1-A');
  });
  it('retains an imported objective and rejects an empty or excessive goal', () => {
    const doc = emptyVideoCampaign();
    doc.objective = 'Priorizar nuevos registros en TDF';
    const imported = importedCampaignSchema.parse({ ...doc, artistId: 51,
      campaignId: '126f208c-2b38-47c1-952b-6ba633122c87' });
    expect(imported.objective).toBe(doc.objective);
    expect(validateVideoCampaign(imported)).toBeNull();
    doc.objective = ' ';
    expect(validateVideoCampaign(doc)).toMatch(/objetivo/);
    doc.objective = 'a'.repeat(201);
    expect(validateVideoCampaign(doc)).toMatch(/objetivo/);
  });
});

it('keeps optional proposed dates inert and rejects impossible calendar dates', () => {
  const doc = emptyVideoCampaign();
  doc.proposedLaunchDate = '2026-02-31';
  expect(validateVideoCampaign(doc)).toMatch(/fecha/);
  doc.proposedLaunchDate = '2026-12-01';
  expect(validateVideoCampaign(doc)).toBeNull();
  expect(doc.status).toBe('draft');
});
