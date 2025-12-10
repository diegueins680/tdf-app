import { del, get, post } from './client';
import type { PartyFollowDTO, SuggestedFriendDTO } from './types';

export const SocialAPI = {
  listFollowers: () => get<PartyFollowDTO[]>('/social/followers'),
  listFollowing: () => get<PartyFollowDTO[]>('/social/following'),
  listFriends: () => get<PartyFollowDTO[]>('/social/friends'),
  listSuggestions: () => get<SuggestedFriendDTO[]>('/social/suggestions'),
  addFriend: (partyId: number) => post<PartyFollowDTO[]>(`/social/friends/${partyId}`, {}),
  exchangeVCard: (partyId: number) => post<PartyFollowDTO[]>('/social/vcard-exchange', { vcerPartyId: partyId }),
  removeFriend: (partyId: number) => del<void>(`/social/friends/${partyId}`),
};
