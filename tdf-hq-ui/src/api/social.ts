import { del, get, post } from './client';
import type { PartyFollowDTO, SocialPartyProfileDTO, SuggestedFriendDTO } from './types';

const requirePositivePartyId = (partyId: number, action: string): number => {
  if (!Number.isSafeInteger(partyId) || partyId <= 0) {
    throw new Error(`Party ID inválido para ${action}.`);
  }
  return partyId;
};

const normalizeProfileIds = (partyIds: readonly number[]): number[] => {
  const uniqueIds = new Set<number>();
  partyIds.forEach((partyId) => {
    uniqueIds.add(requirePositivePartyId(partyId, 'listar perfiles sociales'));
  });
  return Array.from(uniqueIds);
};

export const SocialAPI = {
  listFollowers: () => get<PartyFollowDTO[]>('/social/followers'),
  listFollowing: () => get<PartyFollowDTO[]>('/social/following'),
  listFriends: () => get<PartyFollowDTO[]>('/social/friends'),
  addFriend: (partyId: number) =>
    post<PartyFollowDTO[]>(`/social/friends/${requirePositivePartyId(partyId, 'agregar amistad')}`, {}),
  exchangeVCard: (partyId: number) =>
    post<PartyFollowDTO[]>('/social/vcard-exchange', { vcerPartyId: requirePositivePartyId(partyId, 'intercambio de vCard') }),
  removeFriend: (partyId: number) =>
    del<void>(`/social/friends/${requirePositivePartyId(partyId, 'eliminar amistad')}`),
  listProfiles: (partyIds: readonly number[]) => {
    const normalizedIds = normalizeProfileIds(partyIds);
    if (normalizedIds.length === 0) {
      return Promise.resolve([] as SocialPartyProfileDTO[]);
    }
    const params = new URLSearchParams();
    normalizedIds.forEach((partyId) => params.append('partyId', String(partyId)));
    return get<SocialPartyProfileDTO[]>(`/social/profiles?${params.toString()}`);
  },
  getProfile: (partyId: number) =>
    get<SocialPartyProfileDTO>(`/social/profiles/${requirePositivePartyId(partyId, 'obtener perfil social')}`),
  listSuggestions: () => get<SuggestedFriendDTO[]>('/social/suggestions'),
};
