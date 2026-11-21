import { z } from 'zod';
export type SocialChannel = 'instagram' | 'tiktok';
export interface VideoPiece {
  id: string;
  channel: SocialChannel;
  day: number;
  format: string;
  caption: string;
  assetUrl: string;
  sourceStart: number;
  sourceEnd: number;
  status: 'draft' | 'prepared' | 'exported' | 'published_manual';
  publishedUrl: string;
}
export interface VideoCampaignDocument {
  title: string;
  objective?: string;
  videoUrl: string;
  coverUrl: string;
  durationSeconds: number;
  status: 'draft' | 'ready';
  channels: SocialChannel[];
  proposedLaunchDate?: string;
  credits?: string;
  rightsConfirmed?: boolean;
  technicalReviewed?: boolean;
  pieces: VideoPiece[];
}
export interface VideoCampaign {
  id: string;
  artistId: number;
  revision: number;
  content: VideoCampaignDocument;
  createdAt: string;
  updatedAt: string;
}
export function emptyVideoCampaign(): VideoCampaignDocument {
  return { title: 'Mi live session', objective: 'Registros y seguidores del artista en TDF', videoUrl: '', coverUrl: '', durationSeconds: 0,
    status: 'draft', channels: ['instagram', 'tiktok'], pieces: [] };
}

const MAX_CAMPAIGN_PIECES = 30;

export const importedCampaignSchema = z.object({
  artistId: z.number().int().positive().safe(),
  campaignId: z.string().uuid(),
  title: z.string().min(1).max(200), videoUrl: z.string(), coverUrl: z.string(),
  objective: z.string().trim().min(1).max(200).optional(),
  durationSeconds: z.number().finite().min(0).max(43200), status: z.enum(['draft', 'ready']),
  proposedLaunchDate: z.string().max(10).optional(), credits: z.string().max(12000).optional(),
  rightsConfirmed: z.boolean().optional(), technicalReviewed: z.boolean().optional(),
  channels: z.array(z.enum(['instagram', 'tiktok'])).min(1).max(2),
  pieces: z.array(z.object({
    id: z.string().regex(/^[a-zA-Z0-9_-]{1,100}$/), channel: z.enum(['instagram', 'tiktok']), day: z.number().int().min(-3).max(13),
    format: z.string().max(50), caption: z.string().max(2200), assetUrl: z.string(),
    sourceStart: z.number().finite().min(0), sourceEnd: z.number().finite().min(0),
    status: z.enum(['draft', 'prepared', 'exported', 'published_manual']), publishedUrl: z.string(),
  })).max(MAX_CAMPAIGN_PIECES),
});

export function campaignLink(artistId: number, campaignId: string, piece: Pick<VideoPiece, 'channel' | 'id'>): string {
  const params = new URLSearchParams({ utm_source: piece.channel, utm_medium: 'organic_social',
    utm_campaign: campaignId, utm_content: piece.id });
  return `https://www.tdfrecords.net/a/${artistId}?${params}`;
}

// A relative calendar remains inert until a creator chooses to publish manually.
export function createCampaignPieces(artist: string, channels: SocialChannel[]): VideoPiece[] {
  const days = [-2, -1, 0, 2, 5, 8, 11];
  const angles = [
    `Pronto: ${artist} en vivo. Conoce el perfil en TDF y sigue al artista.`,
    `Mañana podrás descubrir la live session de ${artist} en TDF. Crea tu cuenta y sigue al artista para mantenerte cerca de su música.`,
    `Ya disponible: así suena ${artist} en vivo. Mira la sesión completa desde su perfil en TDF y síguelo.`,
    `Una mirada a la interpretación de ${artist}. Encuentra más en su perfil de TDF.`,
    `¿Qué momento de la sesión de ${artist} volverías a escuchar? Descúbrela completa en TDF.`,
    `Detrás de cada cambio hay una decisión. Escucha a ${artist} en vivo y sigue su perfil en TDF.`,
    `La sesión de ${artist} sigue disponible. Crea tu cuenta en TDF y sigue al artista.`,
  ];
  return channels.flatMap((channel) => days.map((day, i) => ({
    id: `${channel}-${i + 1}`, channel, day,
    format: channel === 'instagram' && (i === 0 || i === 6) ? 'story' : 'vertical-video',
    caption: `${angles[i]} ${channel === 'tiktok' ? 'Busca el perfil de TDF del artista; usa el enlace del perfil solo si está disponible y verificado.' : 'Mira la sesión desde el enlace de la bio; en Stories, usa un sticker de enlace verificado.'} #LiveSession #TDFRecords`,
    assetUrl: '', sourceStart: 0, sourceEnd: 0, status: 'draft', publishedUrl: '',
  })));
}

export function safeVideoUrl(value: string): boolean {
  if (!value) return true;
  try {
    const url = new URL(value);
    return url.protocol === 'https:' && !url.username && !url.password && url.hostname.includes('.')
      && !/^[\d.]+$/.test(url.hostname) && !url.hostname.endsWith('.local')
      && !url.hostname.includes(':') && value.length <= 2048 && !Array.from(value).some((c) => c.charCodeAt(0) < 32 || c.charCodeAt(0) === 127);
  } catch { return false; }
}

export function prepareMissingCampaignPieces(artist: string, channels: SocialChannel[], pieces: VideoPiece[]): VideoPiece[] {
  const missing = channels.filter((channel) => !pieces.some((p) => p.channel === channel));
  const calendar = createCampaignPieces(artist, missing);
  if (pieces.length + calendar.length > MAX_CAMPAIGN_PIECES) {
    throw new Error(`El calendario necesita ${calendar.length} espacios y solo quedan ${Math.max(0, MAX_CAMPAIGN_PIECES - pieces.length)} (máximo ${MAX_CAMPAIGN_PIECES} piezas). Se conservaron las piezas existentes.`);
  }
  const used = new Set(pieces.map((p) => p.id));
  const added = calendar.map((piece) => {
    let id = piece.id;
    let suffix = 2;
    while (used.has(id)) id = `${piece.id}-${suffix++}`;
    used.add(id);
    return { ...piece, id };
  });
  return [...pieces, ...added];
}

export function validateVideoCampaign(doc: VideoCampaignDocument): string | null {
  if (doc.proposedLaunchDate && (!/^\d{4}-\d{2}-\d{2}$/.test(doc.proposedLaunchDate)
    || Number.isNaN(Date.parse(doc.proposedLaunchDate)) || new Date(doc.proposedLaunchDate).toISOString().slice(0, 10) !== doc.proposedLaunchDate)) return 'Revisa la fecha propuesta.';
  if ((doc.credits?.length ?? 0) > 12000) return 'Los créditos admiten hasta 12000 caracteres.';
  if (!doc.title.trim() || doc.title.length > 200) return 'Escribe un título de hasta 200 caracteres.';
  if (doc.objective !== undefined && (typeof doc.objective !== 'string' || !doc.objective.trim() || doc.objective.length > 200))
    return 'Escribe un objetivo de hasta 200 caracteres.';
  if (!safeVideoUrl(doc.videoUrl) || !safeVideoUrl(doc.coverUrl)) return 'Usa enlaces HTTPS válidos para video y portada.';
  if (!Number.isFinite(doc.durationSeconds) || doc.durationSeconds < 0 || doc.durationSeconds > 43200) return 'Revisa la duración del video.';
  if (!doc.channels.length) return 'Selecciona al menos una red social.';
  if (doc.pieces.length > MAX_CAMPAIGN_PIECES) return `La campaña admite hasta ${MAX_CAMPAIGN_PIECES} piezas.`;
  if (doc.pieces.some((p) => !/^[a-zA-Z0-9_-]{1,100}$/.test(p.id)))
    return 'El identificador de cada pieza debe usar letras sin acentos, números, guiones o guiones bajos (hasta 100 caracteres).';
  if (new Set(doc.channels).size !== doc.channels.length || new Set(doc.pieces.map((p) => p.id)).size !== doc.pieces.length)
    return 'Hay redes o piezas duplicadas.';
  if (doc.pieces.some((p) => !doc.channels.includes(p.channel))) return 'Cada pieza debe pertenecer a una red seleccionada.';
  if (doc.status === 'ready' && (!doc.videoUrl || !doc.pieces.length)) return 'Añade el video y las piezas antes de marcar para revisión.';
  if (doc.status === 'ready' && doc.pieces.some((p) => !p.assetUrl || !p.caption.trim())) return 'Completa los archivos y textos de todas las piezas antes de marcar para revisión.';
  for (const piece of doc.pieces) {
    if (!safeVideoUrl(piece.assetUrl) || !safeVideoUrl(piece.publishedUrl)) return 'Revisa los enlaces de las piezas.';
    if (piece.caption.length > 2200) return 'Cada texto admite hasta 2200 caracteres.';
    if (!Number.isFinite(piece.sourceStart) || !Number.isFinite(piece.sourceEnd)
      || piece.sourceStart < 0 || piece.sourceEnd < piece.sourceStart
      || (doc.durationSeconds > 0 && piece.sourceEnd > doc.durationSeconds)) return 'El corte debe estar dentro de la duración del video.';
    if (!Number.isInteger(piece.day) || piece.day < -3 || piece.day > 13) return 'El calendario admite días de D−3 a D+13.';
    if (piece.status === 'published_manual') {
      if (!piece.publishedUrl) return 'Añade la URL de la publicación manual.';
      const host = new URL(piece.publishedUrl).hostname;
      if (![`${piece.channel}.com`, `www.${piece.channel}.com`].includes(host)) return 'La publicación debe pertenecer a la red seleccionada.';
    }
  }
  return null;
}
