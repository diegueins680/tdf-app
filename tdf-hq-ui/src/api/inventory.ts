import { get, post, put } from './client';
import type { InventoryItemDTO, InventoryItemCreate, InventoryItemUpdate, CheckoutDTO, CheckoutCreate } from './types';

export const Inventory = {
  // Inventory item management
  list: (location?: string) => 
    get<InventoryItemDTO[]>(location ? `/inventory?location=${location}` : '/inventory'),
  create: (body: InventoryItemCreate) => post<InventoryItemDTO>('/inventory', body),
  getOne: (id: number) => get<InventoryItemDTO>(`/inventory/${id}`),
  update: (id: number, body: InventoryItemUpdate) => put<InventoryItemDTO>(`/inventory/${id}`, body),
  
  // Check-in/Check-out
  listCheckouts: () => get<CheckoutDTO[]>('/inventory/checkouts'),
  checkout: (body: CheckoutCreate) => post<CheckoutDTO>('/inventory/checkout', body),
  checkin: (checkoutId: number) => post<void>(`/inventory/checkin/${checkoutId}`, {}),
  
  // Maintenance tracking
  getMaintenanceDue: () => get<InventoryItemDTO[]>('/inventory/maintenance-due'),
  scheduleMaintenance: (itemId: number, date: string) => 
    post<void>(`/inventory/${itemId}/maintenance`, { date }),
};
