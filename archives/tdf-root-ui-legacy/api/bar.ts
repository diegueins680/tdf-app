import { get, post } from './client';

export type MenuItem = {
  miProductId: number;
  miName: string;
  miCategory: string;
  miUnitPriceCents: number;
};

export type SaleItem = {
  productId: number;
  qty: number;
  unitPriceCents: number;
};

export type PaymentMethod = 'PayCash' | 'PayCard' | 'PayTransfer' | 'PayComp';

export type SalePayload = {
  bookingId: number;
  stationId: number;
  shiftId: number;
  staffName?: string;
  items: SaleItem[];
  discountCents: number;
  taxCents: number;
  tipCents: number;
  paymentMethod: PaymentMethod;
};

export type RegisterShift = {
  barEventId?: number;
  stationId: number;
  cashierName?: string;
  openingFloatCents: number;
  expectedCloseCents?: number;
};

export type RegisterCountPayload = {
  shiftId: number;
  counts: Array<[number, number]>;
};

export type RegisterCountResult = {
  declaredCents: number;
  expectedCents: number;
  overShortCents: number;
};

export type BarStation = {
  id: number;
  name: string;
  location?: string | null;
  isActive: boolean;
};

export const getMenu = (bookingId: number) =>
  get<MenuItem[]>(`/bar/menu?bookingId=${bookingId}`);

export const listStations = (bookingId: number) =>
  get<BarStation[]>(`/bar/events/${bookingId}/stations`);

export const createStation = (
  bookingId: number,
  station: { name: string; location?: string },
) =>
  post<BarStation>(`/bar/events/${bookingId}/stations`, {
    barEventId: bookingId,
    isActive: true,
    ...station,
  });

export const openRegister = (payload: RegisterShift) =>
  post(`/bar/register/open`, payload);

export const cashDrop = (payload: {
  shiftId: number;
  amountCents: number;
  bagNumber?: string;
  notes?: string;
}) => post(`/bar/register/cash-drops`, payload);

export const countRegister = (payload: RegisterCountPayload) =>
  post<RegisterCountResult>(`/bar/register/count`, payload);

export const closeRegister = (payload: {
  shiftIdClose: number;
  declaredCloseCents: number;
}) => post(`/bar/register/close`, payload);

export const postSale = (payload: SalePayload) =>
  post(`/bar/sales`, payload);

export const getShiftSummary = (shiftId: number) =>
  get(`/bar/register/shifts/${shiftId}/summary`);
