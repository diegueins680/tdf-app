import { get, post, put, del } from './client';
import type {
  ArtistProfileDTO,
  ArtistReleaseDTO,
  ArtistProfileUpsert,
  FanProfileDTO,
  FanProfileUpdate,
  FanFollowDTO,
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
  ContentReactionReq,
  ReactionSummaryDTO,
  NotificationDTO,
  NotificationCountDTO,
  LeaderboardEntryDTO,
} from './types';

export const Fans = {
  listArtists: () => get<ArtistProfileDTO[]>('/fans/artists'),
  getArtist: (artistId: number) => get<ArtistProfileDTO>(`/fans/artists/${artistId}`),
  getReleases: (artistId: number) => get<ArtistReleaseDTO[]>(`/fans/artists/${artistId}/releases`),
  getProfile: () => get<FanProfileDTO>('/fans/me/profile'),
  updateProfile: (payload: FanProfileUpdate) => put<FanProfileDTO>('/fans/me/profile', payload),
  listFollows: () => get<FanFollowDTO[]>('/fans/me/follows'),
  follow: (artistId: number) => post<FanFollowDTO>(`/fans/me/follows/${artistId}`, {}),
  unfollow: (artistId: number) => del<void>(`/fans/me/follows/${artistId}`),
  getMyArtistProfile: () => get<ArtistProfileDTO>('/fans/me/artist-profile'),
  updateMyArtistProfile: (payload: ArtistProfileUpsert) =>
    put<ArtistProfileDTO>('/fans/me/artist-profile', payload),

  // Fan Club
  getClub: (artistId: number) => get<FanClubDTO>(`/fans/clubs/${artistId}`),
  getClubEvents: (artistId: number) => get<FanClubEventDTO[]>(`/fans/clubs/${artistId}/events`),
  listMyClubs: () => get<FanClubDTO[]>('/fans/me/clubs'),
  getMyClub: (artistId: number) => get<FanClubDTO>(`/fans/me/clubs/${artistId}`),
  listClubPosts: (artistId: number) => get<FanClubPostDTO[]>(`/fans/me/clubs/${artistId}/posts`),
  createClubPost: (artistId: number, payload: FanClubCreatePostReq) =>
    post<FanClubPostDTO>(`/fans/me/clubs/${artistId}/posts`, payload),
  pinClubPost: (artistId: number, postId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/posts/${postId}/pin`, {}),
  unpinClubPost: (artistId: number, postId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/posts/${postId}/unpin`, {}),
  hideClubPost: (artistId: number, postId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/posts/${postId}/hide`, {}),
  unhideClubPost: (artistId: number, postId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/posts/${postId}/unhide`, {}),
  listClubEvents: (artistId: number) => get<FanClubEventDTO[]>(`/fans/me/clubs/${artistId}/events`),
  createClubEvent: (artistId: number, payload: FanClubCreateEventReq) =>
    post<FanClubEventDTO>(`/fans/me/clubs/${artistId}/events`, payload),
  listClubElections: (artistId: number) => get<FanClubElectionDTO[]>(`/fans/me/clubs/${artistId}/elections`),
  createClubElection: (artistId: number, payload: FanClubCreateElectionReq) =>
    post<FanClubElectionDTO>(`/fans/me/clubs/${artistId}/elections`, payload),
  createCandidacy: (artistId: number, electionId: number, payload: FanClubCreateCandidacyReq) =>
    post<FanClubCandidacyDTO>(`/fans/me/clubs/${artistId}/elections/${electionId}/candidacy`, payload),
  castVote: (artistId: number, electionId: number, payload: FanClubVoteReq) =>
    post<void>(`/fans/me/clubs/${artistId}/elections/${electionId}/vote`, payload),

  // Fan Club Feed
  listClubFeed: (artistId: number, sort?: string, period?: string) => {
    const params = new URLSearchParams();
    if (sort) params.set('sort', sort);
    if (period) params.set('period', period);
    const qs = params.toString();
    return get<FanClubFeedItemDTO[]>(`/fans/me/clubs/${artistId}/feed${qs ? `?${qs}` : ''}`);
  },

  // Fan Club Memories
  listClubMemories: (artistId: number) => get<FanClubMemoryDTO[]>(`/fans/me/clubs/${artistId}/memories`),
  createClubMemory: (artistId: number, payload: FanClubCreateMemoryReq) =>
    post<FanClubMemoryDTO>(`/fans/me/clubs/${artistId}/memories`, payload),
  hideClubMemory: (artistId: number, memoryId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/memories/${memoryId}/hide`, {}),
  unhideClubMemory: (artistId: number, memoryId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/memories/${memoryId}/unhide`, {}),
  deleteClubMemory: (artistId: number, memoryId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/memories/${memoryId}/delete`, {}),
  reportClubMemory: (artistId: number, memoryId: number, payload: FanClubMemoryReportReq) =>
    post<FanClubMemoryReportDTO>(`/fans/me/clubs/${artistId}/memories/${memoryId}/report`, payload),

  // Fan Club Member Profiles
  listClubMemberProfiles: (artistId: number) => get<FanClubMemberProfileDTO[]>(`/fans/me/clubs/${artistId}/member-profiles`),
  getMyClubMemberProfile: (artistId: number) => get<FanClubMemberProfileDTO>(`/fans/me/clubs/${artistId}/member-profiles/me`),
  updateMyClubMemberProfile: (artistId: number, payload: FanClubMemberProfileUpdate) =>
    put<FanClubMemberProfileDTO>(`/fans/me/clubs/${artistId}/member-profiles/me`, payload),

  // Fan Club Inbox
  listClubInbox: (artistId: number) => get<FanClubInboxMessageDTO[]>(`/fans/me/clubs/${artistId}/inbox`),
  sendClubInboxMessage: (artistId: number, payload: FanClubInboxSendReq) =>
    post<FanClubInboxMessageDTO>(`/fans/me/clubs/${artistId}/inbox`, payload),
  getClubInboxMessage: (artistId: number, messageId: number) =>
    get<FanClubInboxMessageDTO>(`/fans/me/clubs/${artistId}/inbox/${messageId}`),
  replyClubInboxMessage: (artistId: number, messageId: number, payload: FanClubInboxReplyReq) =>
    post<FanClubInboxMessageDTO>(`/fans/me/clubs/${artistId}/inbox/${messageId}/reply`, payload),
  updateClubInboxStatus: (artistId: number, messageId: number, payload: FanClubInboxStatusReq) =>
    post<FanClubInboxMessageDTO>(`/fans/me/clubs/${artistId}/inbox/${messageId}/status`, payload),

  // Reactions
  reactToPost: (artistId: number, postId: number, payload: ContentReactionReq) =>
    post<ReactionSummaryDTO>(`/fans/me/clubs/${artistId}/posts/${postId}/react`, payload),
  reactToMemory: (artistId: number, memoryId: number, payload: ContentReactionReq) =>
    post<ReactionSummaryDTO>(`/fans/me/clubs/${artistId}/memories/${memoryId}/react`, payload),

  // Leaderboard & Spotlight
  getLeaderboard: (artistId: number, period?: string) => {
    const qs = period ? `?period=${period}` : '';
    return get<LeaderboardEntryDTO[]>(`/fans/me/clubs/${artistId}/leaderboard${qs}`);
  },
  getSpotlight: (artistId: number) =>
    get<FanClubFeedItemDTO | null>(`/fans/me/clubs/${artistId}/spotlight`),

  // Discovery
  getDiscoveryFeed: (limit?: number) => {
    const qs = limit ? `?limit=${limit}` : '';
    return get<FanClubFeedItemDTO[]>(`/fans/discovery${qs}`);
  },

  // Notifications
  listNotifications: (unreadOnly?: boolean) => {
    const qs = unreadOnly ? '?unreadOnly=true' : '';
    return get<NotificationDTO[]>(`/fans/me/notifications${qs}`);
  },
  getNotificationCount: () => get<NotificationCountDTO>('/fans/me/notifications/count'),
  markNotificationRead: (notifId: number) =>
    post<void>(`/fans/me/notifications/${notifId}/read`, {}),
  markAllNotificationsRead: () =>
    post<void>('/fans/me/notifications/read-all', {}),
};
