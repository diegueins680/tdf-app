import { get } from './client';

export interface PublicEngineer {
  peId: number;
  peName: string;
}

export const Engineers = {
  listPublic: () => get<PublicEngineer[]>('/engineers'),
};
