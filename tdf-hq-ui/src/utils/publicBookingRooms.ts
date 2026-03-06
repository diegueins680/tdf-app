const normalizeService = (service: string) => service.trim().toLowerCase();

const stripDiacritics = (text: string) => text.normalize('NFD').replace(/\p{Diacritic}/gu, '');

const normalizeToken = (value: string) =>
  stripDiacritics(normalizeService(value))
    .replace(/[^a-z0-9]+/g, ' ')
    .trim()
    .replace(/\s+/g, ' ');

const normalizeRoomToken = (value: string) => normalizeToken(value);

export const defaultRoomsForService = (service: string, roomOptions: string[]) => {
  const plain = normalizeToken(service);
  const hasAny = (...needles: string[]) =>
    needles.some((needle) => plain.includes(normalizeToken(needle)));
  const pick = (needle: string) =>
    roomOptions.find((room) => normalizeRoomToken(room).includes(normalizeRoomToken(needle)));
  const picks = (...needles: string[]) => {
    const seen = new Set<string>();
    const selected: string[] = [];
    for (const needle of needles) {
      const room = pick(needle);
      if (!room) continue;
      const key = normalizeRoomToken(room);
      if (seen.has(key)) continue;
      seen.add(key);
      selected.push(room);
    }
    return selected;
  };

  if (hasAny('grabacion audiovisual live', 'audiovisual live')) {
    const candidates = picks('live', 'control');
    if (candidates.length) return candidates;
  }
  if (hasAny('grabacion de banda', 'grabacion banda', 'band recording', 'banda')) {
    const candidates = picks('live', 'control');
    if (candidates.length) return candidates;
  }
  if (hasAny('grabacion de voz', 'grabacion voz', 'grabacion vocal', 'vocal recording', 'voz')) {
    const candidates = picks('live', 'vocal');
    if (candidates.length) return candidates;
  }
  if (hasAny('mezcla', 'mix')) {
    const candidates = picks('control');
    if (candidates.length) return candidates;
  }
  // Treat mastering as a whole-word concept so labels like "masterclass" don't misclassify.
  if (/\bmaster(?:ing|izacion)?\b/.test(plain)) {
    const candidates = picks('control');
    if (candidates.length) return candidates;
  }
  if (hasAny('podcast')) {
    const candidates = picks('control', 'vocal');
    if (candidates.length) return candidates;
  }
  if (hasAny('ensayo', 'rehearsal')) {
    const candidates = picks('live');
    if (candidates.length) return candidates;
  }
  if (hasAny('dj')) {
    const candidates = picks('dj');
    if (candidates.length) return candidates;
  }
  const fallback = roomOptions.slice(0, 2).filter(Boolean);
  return fallback;
};

export const sameRooms = (a: string[], b: string[]) => {
  if (a.length !== b.length) return false;
  const sort = (list: string[]) => [...list].map(normalizeRoomToken).sort();
  const [as, bs] = [sort(a), sort(b)];
  return as.every((val, idx) => val === bs[idx]);
};
