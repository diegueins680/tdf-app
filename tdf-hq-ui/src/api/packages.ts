import { get, post, put } from './client';
import type { PackageDTO, PackageCreate, PackageUpdate, PurchaseDTO, PurchaseCreate } from './types';

export const Packages = {
  // Package catalog management
  list: () => get<PackageDTO[]>('/packages'),
  create: (body: PackageCreate) => post<PackageDTO>('/packages', body),
  getOne: (id: number) => get<PackageDTO>(`/packages/${id}`),
  update: (id: number, body: PackageUpdate) => put<PackageDTO>(`/packages/${id}`, body),
  
  // Package purchases
  listPurchases: (partyId?: number) => 
    get<PurchaseDTO[]>(partyId ? `/purchases?partyId=${partyId}` : '/purchases'),
  createPurchase: (body: PurchaseCreate) => post<PurchaseDTO>('/purchases', body),
  getPurchase: (id: number) => get<PurchaseDTO>(`/purchases/${id}`),
  
  // Package usage tracking
  getBalance: (purchaseId: number) => get<{ remaining: number; used: number; total: number }>(`/purchases/${purchaseId}/balance`),
};
