import { del, get, post } from './client';
import type { PartyFollowDTO } from './types';

export const SocialAPI = {
  listFollowers: () => get<PartyFollowDTO[]>('/social/followers'),
  listFollowing: () => get<PartyFollowDTO[]>('/social/following'),
  listFriends: () => get<PartyFollowDTO[]>('/social/friends'),
  addFriend: (partyId: number) => post<PartyFollowDTO[]>(`/social/friends/${partyId}`, {}),
  removeFriend: (partyId: number) => del<void>(`/social/friends/${partyId}`),
};
