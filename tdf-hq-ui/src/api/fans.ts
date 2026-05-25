import { get, post, put, del } from './client';
import type {
  ArtistProfileDTO,
  ArtistReleaseDTO,
  ArtistProfileUpsert,
  FanProfileDTO,
  FanProfileUpdate,
  FanFollowDTO,
  ArtistFansResponse,
  FanClubDTO,
  FanClubPostDTO,
  FanClubEventDTO,
  FanClubElectionDTO,
  FanClubCandidacyDTO,
  FanClubCreatePostReq,
  FanClubCreateEventReq,
  FanClubCreateElectionReq,
  FanClubCreateCandidacyReq,
  FanClubVoteReq,
  FanClubFeedItemDTO,
  FanClubMemoryDTO,
  FanClubMemoryReportDTO,
  FanClubCreateMemoryReq,
  FanClubMemoryReportReq,
  FanClubMemberProfileDTO,
  FanClubMemberProfileUpdate,
  FanClubInboxMessageDTO,
  FanClubInboxSendReq,
  FanClubInboxReplyReq,
  FanClubInboxStatusReq,
} from './types';

const read: (path: string) => unknown = get;
const send: (path: string, body: unknown) => unknown = post;
const update: (path: string, body: unknown) => unknown = put;
const remove: (path: string) => unknown = del;

export const Fans = {
  listArtists: async () => (await read('/fans/artists')) as ArtistProfileDTO[],
  getArtist: async (artistId: number) => (await read(`/fans/artists/${artistId}`)) as ArtistProfileDTO,
  getReleases: async (artistId: number) => (await read(`/fans/artists/${artistId}/releases`)) as ArtistReleaseDTO[],
  getArtistFans: async (artistId: number, page = 1, pageSize = 5) =>
    (await read(`/fans/artists/${artistId}/fans?page=${page}&pageSize=${pageSize}`)) as ArtistFansResponse,
  getProfile: async () => (await read('/fans/me/profile')) as FanProfileDTO,
  updateProfile: async (payload: FanProfileUpdate) => (await update('/fans/me/profile', payload)) as FanProfileDTO,
  listFollows: async () => (await read('/fans/me/follows')) as FanFollowDTO[],
  follow: async (artistId: number) => (await send(`/fans/me/follows/${artistId}`, {})) as FanFollowDTO,
  unfollow: async (artistId: number) => {
    await remove(`/fans/me/follows/${artistId}`);
  },
  getMyArtistProfile: async () => (await read('/fans/me/artist-profile')) as ArtistProfileDTO,
  updateMyArtistProfile: async (payload: ArtistProfileUpsert) =>
    (await update('/fans/me/artist-profile', payload)) as ArtistProfileDTO,

  // Fan Club
  getClub: async (artistId: number) => (await read(`/fans/clubs/${artistId}`)) as FanClubDTO,
  getClubEvents: async (artistId: number) => (await read(`/fans/clubs/${artistId}/events`)) as FanClubEventDTO[],
  listMyClubs: async () => (await read('/fans/me/clubs')) as FanClubDTO[],
  getMyClub: async (artistId: number) => (await read(`/fans/me/clubs/${artistId}`)) as FanClubDTO,
  listClubPosts: async (artistId: number) => (await read(`/fans/me/clubs/${artistId}/posts`)) as FanClubPostDTO[],
  createClubPost: async (artistId: number, payload: FanClubCreatePostReq) =>
    (await send(`/fans/me/clubs/${artistId}/posts`, payload)) as FanClubPostDTO,
  pinClubPost: async (artistId: number, postId: number) => {
    await send(`/fans/me/clubs/${artistId}/posts/${postId}/pin`, {});
  },
  unpinClubPost: async (artistId: number, postId: number) => {
    await send(`/fans/me/clubs/${artistId}/posts/${postId}/unpin`, {});
  },
  hideClubPost: async (artistId: number, postId: number) => {
    await send(`/fans/me/clubs/${artistId}/posts/${postId}/hide`, {});
  },
  unhideClubPost: async (artistId: number, postId: number) => {
    await send(`/fans/me/clubs/${artistId}/posts/${postId}/unhide`, {});
  },
  listClubEvents: async (artistId: number) => (await read(`/fans/me/clubs/${artistId}/events`)) as FanClubEventDTO[],
  createClubEvent: async (artistId: number, payload: FanClubCreateEventReq) =>
    (await send(`/fans/me/clubs/${artistId}/events`, payload)) as FanClubEventDTO,
  listClubElections: async (artistId: number) =>
    (await read(`/fans/me/clubs/${artistId}/elections`)) as FanClubElectionDTO[],
  createClubElection: async (artistId: number, payload: FanClubCreateElectionReq) =>
    (await send(`/fans/me/clubs/${artistId}/elections`, payload)) as FanClubElectionDTO,
  createCandidacy: async (artistId: number, electionId: number, payload: FanClubCreateCandidacyReq) =>
    (await send(`/fans/me/clubs/${artistId}/elections/${electionId}/candidacy`, payload)) as FanClubCandidacyDTO,
  castVote: async (artistId: number, electionId: number, payload: FanClubVoteReq) => {
    await send(`/fans/me/clubs/${artistId}/elections/${electionId}/vote`, payload);
  },

  // Fan Club Feed
  listClubFeed: async (artistId: number) => (await read(`/fans/me/clubs/${artistId}/feed`)) as FanClubFeedItemDTO[],

  // Fan Club Memories
  listClubMemories: async (artistId: number) =>
    (await read(`/fans/me/clubs/${artistId}/memories`)) as FanClubMemoryDTO[],
  createClubMemory: async (artistId: number, payload: FanClubCreateMemoryReq) =>
    (await send(`/fans/me/clubs/${artistId}/memories`, payload)) as FanClubMemoryDTO,
  hideClubMemory: async (artistId: number, memoryId: number) => {
    await send(`/fans/me/clubs/${artistId}/memories/${memoryId}/hide`, {});
  },
  unhideClubMemory: async (artistId: number, memoryId: number) => {
    await send(`/fans/me/clubs/${artistId}/memories/${memoryId}/unhide`, {});
  },
  deleteClubMemory: async (artistId: number, memoryId: number) => {
    await send(`/fans/me/clubs/${artistId}/memories/${memoryId}/delete`, {});
  },
  reportClubMemory: async (artistId: number, memoryId: number, payload: FanClubMemoryReportReq) =>
    (await send(`/fans/me/clubs/${artistId}/memories/${memoryId}/report`, payload)) as FanClubMemoryReportDTO,

  // Fan Club Member Profiles
  listClubMemberProfiles: async (artistId: number) =>
    (await read(`/fans/me/clubs/${artistId}/member-profiles`)) as FanClubMemberProfileDTO[],
  getMyClubMemberProfile: async (artistId: number) =>
    (await read(`/fans/me/clubs/${artistId}/member-profiles/me`)) as FanClubMemberProfileDTO,
  updateMyClubMemberProfile: async (artistId: number, payload: FanClubMemberProfileUpdate) =>
    (await update(`/fans/me/clubs/${artistId}/member-profiles/me`, payload)) as FanClubMemberProfileDTO,

  // Fan Club Inbox
  listClubInbox: async (artistId: number) =>
    (await read(`/fans/me/clubs/${artistId}/inbox`)) as FanClubInboxMessageDTO[],
  sendClubInboxMessage: async (artistId: number, payload: FanClubInboxSendReq) =>
    (await send(`/fans/me/clubs/${artistId}/inbox`, payload)) as FanClubInboxMessageDTO,
  getClubInboxMessage: async (artistId: number, messageId: number) =>
    (await read(`/fans/me/clubs/${artistId}/inbox/${messageId}`)) as FanClubInboxMessageDTO,
  replyClubInboxMessage: async (artistId: number, messageId: number, payload: FanClubInboxReplyReq) =>
    (await send(`/fans/me/clubs/${artistId}/inbox/${messageId}/reply`, payload)) as FanClubInboxMessageDTO,
  updateClubInboxStatus: async (artistId: number, messageId: number, payload: FanClubInboxStatusReq) =>
    (await send(`/fans/me/clubs/${artistId}/inbox/${messageId}/status`, payload)) as FanClubInboxMessageDTO,
};
