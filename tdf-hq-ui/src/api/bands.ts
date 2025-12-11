import { get } from './client';
import type { BandOptionsDTO } from './types';

export const Bands = {
  options: () => get<BandOptionsDTO>('/bands/options'),
};
