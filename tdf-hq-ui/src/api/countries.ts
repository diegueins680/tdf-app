import { get } from './client';

export interface CountryDTO {
  countryCode: string;
  countryName: string;
}

export const Countries = {
  list: () => get<CountryDTO[]>('/countries'),
};
