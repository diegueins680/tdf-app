import type { PlayerTrack, RepeatMode } from './types';

export interface QueueState {
  tracks: PlayerTrack[];
  currentIndex: number;
  shuffle: boolean;
  repeat: RepeatMode;
}

export type QueueAction =
  | { type: 'replace'; tracks: PlayerTrack[]; currentTrackId?: string }
  | { type: 'enqueue'; tracks: PlayerTrack[] }
  | { type: 'select'; index: number }
  | { type: 'remove'; index: number }
  | { type: 'move'; from: number; to: number }
  | { type: 'clear' }
  | { type: 'shuffle'; enabled: boolean }
  | { type: 'repeat'; mode: RepeatMode };

export const EMPTY_QUEUE: QueueState = {
  tracks: [],
  currentIndex: -1,
  shuffle: false,
  repeat: 'off',
};

const clampIndex = (index: number, length: number): number => {
  if (length === 0) return -1;
  return Math.min(Math.max(index, 0), length - 1);
};

export function queueReducer(state: QueueState, action: QueueAction): QueueState {
  switch (action.type) {
    case 'replace': {
      const requestedIndex = action.currentTrackId
        ? action.tracks.findIndex((track) => track.id === action.currentTrackId)
        : 0;
      return {
        ...state,
        tracks: [...action.tracks],
        currentIndex: clampIndex(requestedIndex < 0 ? 0 : requestedIndex, action.tracks.length),
      };
    }
    case 'enqueue':
      return {
        ...state,
        tracks: [...state.tracks, ...action.tracks],
        currentIndex: state.currentIndex < 0 && action.tracks.length > 0 ? 0 : state.currentIndex,
      };
    case 'select':
      return { ...state, currentIndex: clampIndex(action.index, state.tracks.length) };
    case 'remove': {
      if (action.index < 0 || action.index >= state.tracks.length) return state;
      const tracks = state.tracks.filter((_, index) => index !== action.index);
      const currentIndex = action.index < state.currentIndex
        ? state.currentIndex - 1
        : action.index === state.currentIndex
          ? clampIndex(state.currentIndex, tracks.length)
          : state.currentIndex;
      return { ...state, tracks, currentIndex };
    }
    case 'move': {
      if (
        action.from < 0 || action.from >= state.tracks.length
        || action.to < 0 || action.to >= state.tracks.length
        || action.from === action.to
      ) return state;
      const tracks = [...state.tracks];
      const [moved] = tracks.splice(action.from, 1);
      if (!moved) return state;
      tracks.splice(action.to, 0, moved);
      const currentTrackId = state.tracks[state.currentIndex]?.id;
      return {
        ...state,
        tracks,
        currentIndex: currentTrackId ? tracks.findIndex((track) => track.id === currentTrackId) : -1,
      };
    }
    case 'clear':
      return { ...state, tracks: [], currentIndex: -1 };
    case 'shuffle':
      return { ...state, shuffle: action.enabled };
    case 'repeat':
      return { ...state, repeat: action.mode };
  }
}

const sequentialNextIndex = (state: QueueState): number => {
  const next = state.currentIndex + 1;
  if (next < state.tracks.length) return next;
  return state.repeat === 'queue' ? 0 : -1;
};

export function nextQueueIndex(state: QueueState, randomValue = Math.random()): number {
  if (state.tracks.length === 0 || state.currentIndex < 0) return -1;
  if (state.repeat === 'track') return state.currentIndex;

  if (state.repeat === 'release') {
    const releaseId = state.tracks[state.currentIndex]?.releaseId;
    const releaseIndices = state.tracks
      .map((track, index) => ({ track, index }))
      .filter(({ track }) => track.releaseId === releaseId)
      .map(({ index }) => index);
    const position = releaseIndices.indexOf(state.currentIndex);
    if (releaseIndices.length > 0 && position >= 0) {
      return releaseIndices[(position + 1) % releaseIndices.length] ?? state.currentIndex;
    }
  }

  if (state.shuffle && state.tracks.length > 1) {
    const candidates = state.tracks.map((_, index) => index).filter((index) => index !== state.currentIndex);
    const normalizedRandom = Math.min(Math.max(randomValue, 0), 0.999999999);
    return candidates[Math.floor(normalizedRandom * candidates.length)] ?? sequentialNextIndex(state);
  }

  return sequentialNextIndex(state);
}

export function previousQueueIndex(state: QueueState): number {
  if (state.tracks.length === 0 || state.currentIndex < 0) return -1;
  if (state.repeat === 'track') return state.currentIndex;
  if (state.repeat === 'release') {
    const releaseId = state.tracks[state.currentIndex]?.releaseId;
    const releaseIndices = state.tracks
      .map((track, index) => ({ track, index }))
      .filter(({ track }) => track.releaseId === releaseId)
      .map(({ index }) => index);
    const position = releaseIndices.indexOf(state.currentIndex);
    if (releaseIndices.length > 0 && position >= 0) {
      return releaseIndices[(position - 1 + releaseIndices.length) % releaseIndices.length] ?? state.currentIndex;
    }
  }
  if (state.currentIndex > 0) return state.currentIndex - 1;
  return state.repeat === 'queue' ? state.tracks.length - 1 : -1;
}
