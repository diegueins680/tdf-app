export interface Recording {
  title: string;
  artist: string;
  description: string;
  image: string;
  recordedAt: string;
  vibe: string;
}

export interface ReleaseLink {
  platform: string;
  url: string;
  accent: string;
}

export interface Release {
  title: string;
  artist: string;
  blurb: string;
  cover: string;
  releasedOn: string;
  links: ReleaseLink[];
}

export interface SessionVideo {
  title: string;
  guests: string;
  youtubeId: string;
  duration: string;
  description: string;
}

export const recordings: Recording[] = [
  {
    title: 'Late Night Brass',
    artist: 'La Bruma',
    description: 'Big-room horns tracked live through the API console. Ribbon mics on the room for extra glue.',
    image:
      'https://images.unsplash.com/photo-1520523839897-bd0b52f945a0?auto=format&fit=crop&w=1600&q=80',
    recordedAt: '2025-11-16 · Sala A',
    vibe: 'Soul / Live to 2-track',
  },
  {
    title: 'Guitarras en el domo',
    artist: 'Sofía Márquez',
    description: 'Layered guitars with analog delays, captured in the dome for natural diffusion.',
    image:
      'https://images.unsplash.com/photo-1511379938547-c1f69419868d?auto=format&fit=crop&w=1600&q=80',
    recordedAt: '2025-11-11 · Domo',
    vibe: 'Indie / Textural',
  },
  {
    title: 'Beat Tape Vol. 3',
    artist: 'TDF House Band',
    description: 'MPC + Prophet + live percussion. Recorded straight to 1/4" and digitized for mixing.',
    image:
      'https://images.unsplash.com/photo-1506157786151-b8491531f063?auto=format&fit=crop&w=1600&q=80',
    recordedAt: '2025-11-02 · Sala B',
    vibe: 'Beats / Hybrid',
  },
];

export const releases: Release[] = [
  {
    title: 'Luna Baja',
    artist: 'Sofía Márquez',
    blurb: 'Canciones grabadas en TDF con cuerdas y coros, mezcladas en cinta.',
    cover:
      'https://images.unsplash.com/photo-1511671782779-c97d3d27a1d4?auto=format&fit=crop&w=900&q=80',
    releasedOn: '2025-10-28',
    links: [
      { platform: 'Spotify', url: 'https://open.spotify.com/', accent: '#1db954' },
      { platform: 'Apple Music', url: 'https://music.apple.com/', accent: '#fa233b' },
      { platform: 'Bandcamp', url: 'https://bandcamp.com/', accent: '#629aa9' },
    ],
  },
  {
    title: 'Session Tapes: Live at TDF',
    artist: 'TDF House Band',
    blurb: 'Serie de jams capturadas en la sala A con público reducido.',
    cover:
      'https://images.unsplash.com/photo-1497032628192-86f99bcd76bc?auto=format&fit=crop&w=900&q=80',
    releasedOn: '2025-09-10',
    links: [
      { platform: 'YouTube Music', url: 'https://music.youtube.com/', accent: '#ff0000' },
      { platform: 'Deezer', url: 'https://www.deezer.com/', accent: '#a238ff' },
    ],
  },
];

export const sessionVideos: SessionVideo[] = [
  {
    title: 'TDF Sessions #21 · Sofía Márquez',
    guests: 'Sofía Márquez + TDF House Band',
    youtubeId: 'Bey4XXJAqS8',
    duration: '14:32',
    description: 'Set íntimo con cuerdas y voz en vivo, grabado en una sola toma.',
  },
  {
    title: 'TDF Sessions #20 · La Bruma',
    guests: 'La Bruma',
    youtubeId: 'ktvTqknDobU',
    duration: '12:48',
    description: 'Brass y percusión en el domo con mezcla en consola analógica.',
  },
  {
    title: 'TDF Sessions #19 · Beat Tape Live',
    guests: 'TDF House Band',
    youtubeId: 'Zi_XLOBDo_Y',
    duration: '09:17',
    description: 'MPC meets Prophet: beats, texturas y un bajo Moog que vibra.',
  },
];
