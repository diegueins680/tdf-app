
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
