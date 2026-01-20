const normalizeService = (service: string) => service.trim().toLowerCase();

const stripDiacritics = (text: string) => text.normalize('NFD').replace(/\p{Diacritic}/gu, '');

export const defaultRoomsForService = (service: string, roomOptions: string[]) => {
  const norm = normalizeService(service);
  const plain = stripDiacritics(norm);
  const hasAny = (...needles: string[]) => needles.some((needle) => plain.includes(needle));
  const pick = (needle: string) =>
    roomOptions.find((room) => room.toLowerCase().includes(needle.toLowerCase()));
  const picks = (...needles: string[]) => needles.map(pick).filter(Boolean) as string[];

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
  if (hasAny('master')) {
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
  const sort = (list: string[]) => [...list].map(normalizeService).sort();
  const [as, bs] = [sort(a), sort(b)];
  return as.every((val, idx) => val === bs[idx]);
};
