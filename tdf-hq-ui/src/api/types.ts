export interface PartyDTO {
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
}

export interface PartyCreate {
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
}

export interface PartyUpdate {
  uDisplayName?: string;
  uIsOrg?: boolean;
  uLegalName?: string | null;
  uPrimaryEmail?: string | null;
  uPrimaryPhone?: string | null;
  uWhatsapp?: string | null;
  uInstagram?: string | null;
  uTaxId?: string | null;
  uEmergencyContact?: string | null;
  uNotes?: string | null;
}

export interface BookingResourceDTO {
  brRoomId: string;
  brRoomName: string;
  brRole: string;
}

export interface BookingDTO {
  bookingId: number;
  title: string;
  startsAt: string; // ISO
  endsAt: string;   // ISO
  status: string;
  notes?: string | null;
  partyId?: number | null;
  serviceType?: string | null;
  serviceOrderId?: number | null;
  serviceOrderTitle?: string | null;
  customerName?: string | null;
  partyDisplayName?: string | null;
  resources: BookingResourceDTO[];
}

export interface VersionInfo {
  name: string;
  version: string;
  commit?: string | null;
  buildTime?: string | null;
}

type HealthState = 'ok' | 'degraded' | (string & Record<never, never>);

export interface HealthStatus {
  status: HealthState;
  version?: string | null;
}

export interface RoomDTO {
  roomId: string;
  rName: string;
  rBookable: boolean;
}

export interface RoomCreate {
  rcName: string;
}

export interface RoomUpdate {
  ruName?: string;
  ruIsBookable?: boolean;
}

export interface PipelineCardDTO {
  pcId: string;
  pcTitle: string;
  pcArtist?: string | null;
  pcType: string;
  pcStage: string;
  pcSortOrder: number;
  pcNotes?: string | null;
}

export interface PipelineCardUpdate {
  pcuTitle?: string;
  pcuArtist?: string | null;
  pcuStage?: string;
  pcuSortOrder?: number;
  pcuNotes?: string | null;
}
