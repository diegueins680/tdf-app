import { del, get, post } from './client';
import type { PartyFollowDTO, SuggestedFriendDTO } from './types';

const requirePositivePartyId = (partyId: number, action: string): number => {
  if (!Number.isSafeInteger(partyId) || partyId <= 0) {
    throw new Error(`Party ID inválido para ${action}.`);
  }
  return partyId;
};

export const SocialAPI = {
  listFollowers: () => get<PartyFollowDTO[]>('/social/followers'),
  listFollowing: () => get<PartyFollowDTO[]>('/social/following'),
  listFriends: () => get<PartyFollowDTO[]>('/social/friends'),
  listSuggestions: () => get<SuggestedFriendDTO[]>('/social/suggestions'),
  addFriend: (partyId: number) =>
    post<PartyFollowDTO[]>(`/social/friends/${requirePositivePartyId(partyId, 'agregar amistad')}`, {}),
  exchangeVCard: (partyId: number) =>
    post<PartyFollowDTO[]>('/social/vcard-exchange', { vcerPartyId: requirePositivePartyId(partyId, 'intercambio de vCard') }),
  removeFriend: (partyId: number) =>
    del<void>(`/social/friends/${requirePositivePartyId(partyId, 'eliminar amistad')}`),
};
