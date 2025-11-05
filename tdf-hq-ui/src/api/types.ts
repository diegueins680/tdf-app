export type PartyDTO = {
  partyId: number;
  legalName?: string | null;
  displayName: string;
  isOrg: boolean;
  taxId?: string | null;
  primaryEmail?: string | null;
  primaryPhone?: string | null;
  whatsapp?: string | null;
  instagram?: string | null;
  emergencyContact?: string | null;
  notes?: string | null;
};

export type PartyCreate = {
  cDisplayName: string;
  cIsOrg: boolean;
  cLegalName?: string | null;
  cPrimaryEmail?: string | null;
  cPrimaryPhone?: string | null;
  cWhatsapp?: string | null;
  cInstagram?: string | null;
  cTaxId?: string | null;
  cEmergencyContact?: string | null;
  cNotes?: string | null;
};

export type PartyUpdate = Partial<{
  uDisplayName: string;
  uIsOrg: boolean;
  uLegalName: string | null;
  uPrimaryEmail: string | null;
  uPrimaryPhone: string | null;
  uWhatsapp: string | null;
  uInstagram: string | null;
  uTaxId: string | null;
  uEmergencyContact: string | null;
  uNotes: string | null;
}>;

export type BookingDTO = {
  bookingId: number;
  title: string;
  startsAt: string; // ISO
  endsAt: string;   // ISO
  status: string;
  notes?: string | null;
};

// Package Management Types
export type PackageDTO = {
  packageId: number;
  name: string;
  service: string;
  priceUsd: number;
  unitsKind: 'hours' | 'sessions' | 'songs';
  unitsQty: number;
  expiresDays: number;
  transferable: boolean;
  refundPolicy: 'credit_only' | 'full' | 'none';
  description?: string | null;
};

export type PackageCreate = {
  cName: string;
  cService: string;
  cPriceUsd: number;
  cUnitsKind: 'hours' | 'sessions' | 'songs';
  cUnitsQty: number;
  cExpiresDays: number;
  cTransferable: boolean;
  cRefundPolicy: 'credit_only' | 'full' | 'none';
  cDescription?: string | null;
};

export type PackageUpdate = Partial<{
  uName: string;
  uPriceUsd: number;
  uDescription: string | null;
}>;

export type PurchaseDTO = {
  purchaseId: number;
  partyId: number;
  packageId: number;
  packageName: string;
  purchasedAt: string;
  expiresAt: string;
  unitsTotal: number;
  unitsUsed: number;
  unitsRemaining: number;
  status: 'active' | 'expired' | 'depleted';
};

export type PurchaseCreate = {
  cPartyId: number;
  cPackageId: number;
};

// Invoice & Payment Types
export type InvoiceDTO = {
  invoiceId: number;
  partyId: number;
  partyName: string;
  invoiceNumber: string;
  issuedAt: string;
  dueAt: string;
  subtotalUsd: number;
  taxUsd: number;
  totalUsd: number;
  paidUsd: number;
  status: 'draft' | 'issued' | 'paid' | 'overdue' | 'cancelled';
  items: InvoiceItemDTO[];
};

export type InvoiceItemDTO = {
  description: string;
  quantity: number;
  unitPriceUsd: number;
  totalUsd: number;
};

export type InvoiceCreate = {
  cPartyId: number;
  cDueAt: string;
  cItems: Array<{
    description: string;
    quantity: number;
    unitPriceUsd: number;
  }>;
  cNotes?: string | null;
};

export type PaymentDTO = {
  paymentId: number;
  invoiceId: number;
  amountUsd: number;
  method: 'cash' | 'bank_transfer' | 'card_pos';
  paidAt: string;
  reference?: string | null;
  notes?: string | null;
};

export type PaymentCreate = {
  cInvoiceId: number;
  cAmountUsd: number;
  cMethod: 'cash' | 'bank_transfer' | 'card_pos';
  cReference?: string | null;
  cNotes?: string | null;
};

// Inventory Types
export type InventoryItemDTO = {
  itemId: number;
  name: string;
  category: string;
  serialNumber?: string | null;
  location: string;
  status: 'available' | 'checked_out' | 'maintenance' | 'retired';
  condition: 'excellent' | 'good' | 'fair' | 'poor';
  purchasedAt?: string | null;
  lastMaintenanceAt?: string | null;
  nextMaintenanceAt?: string | null;
  notes?: string | null;
};

export type InventoryItemCreate = {
  cName: string;
  cCategory: string;
  cSerialNumber?: string | null;
  cLocation: string;
  cCondition: 'excellent' | 'good' | 'fair' | 'poor';
  cPurchasedAt?: string | null;
  cNotes?: string | null;
};

export type InventoryItemUpdate = Partial<{
  uName: string;
  uLocation: string;
  uStatus: 'available' | 'checked_out' | 'maintenance' | 'retired';
  uCondition: 'excellent' | 'good' | 'fair' | 'poor';
  uNotes: string | null;
}>;

export type CheckoutDTO = {
  checkoutId: number;
  itemId: number;
  itemName: string;
  partyId: number;
  partyName: string;
  checkedOutAt: string;
  expectedReturnAt: string;
  returnedAt?: string | null;
  notes?: string | null;
};

export type CheckoutCreate = {
  cItemId: number;
  cPartyId: number;
  cExpectedReturnAt: string;
  cNotes?: string | null;
};
