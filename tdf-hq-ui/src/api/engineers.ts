import { get } from './client';

export interface PublicEngineer {
  peId: number;
  peName: string;
}

export const Engineers = {
  listPublic: async () => {
    try {
      return await get<PublicEngineer[]>('/engineers');
    } catch (error) {
      console.warn('Engineer catalog unavailable, falling back to manual entry', error);
      return [];
    }
  },
};
