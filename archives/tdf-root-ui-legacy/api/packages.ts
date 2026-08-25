import { get, patch, post } from './client';
import type { PackageProductCreate, PackageProductDTO, PackageProductUpdate, PackagePurchaseReq } from './types';

export const PackagesApi = {
  listProducts: () => get<PackageProductDTO[]>('/packages/products'),
  createProduct: (body: PackageProductCreate) => post<PackageProductDTO>('/packages/products', body),
  updateProduct: (id: number, body: PackageProductUpdate) => patch<PackageProductDTO>(`/packages/products/${id}`, body),
  createPurchase: (body: PackagePurchaseReq) => post<void>('/packages/purchases', body),
};
