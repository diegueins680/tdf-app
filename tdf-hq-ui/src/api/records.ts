import { get } from './client';
import type { components } from './generated/types';

export type RecordsContributorDTO = components['schemas']['RecordsContributor'];
export type RecordsResourceDTO = components['schemas']['RecordsResource'];
export type RecordsCollectionDTO = components['schemas']['RecordsCollection'];
export type RecordsReleaseDTO = components['schemas']['RecordsRelease'];
export type RecordsRecordingDTO = components['schemas']['RecordsRecording'];
export type RecordsSessionDTO = components['schemas']['RecordsSession'];
export type RecordsFeedDTO = components['schemas']['RecordsFeed'];

export const Records = {
  getFeed: (locale = 'es', signal?: AbortSignal) => {
    const path = `/records/feed?locale=${encodeURIComponent(locale)}`;
    return signal ? get<RecordsFeedDTO>(path, { signal }) : get<RecordsFeedDTO>(path);
  },
};
