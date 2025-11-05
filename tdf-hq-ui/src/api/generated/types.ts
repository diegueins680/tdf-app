// Auto-generated types from OpenAPI specification

export type PartyRole = 
  | 'Admin'
  | 'Manager'
  | 'Engineer'
  | 'Teacher'
  | 'Reception'
  | 'Accounting'
  | 'Artist'
  | 'Student'
  | 'ReadOnly';

export interface User {
  id: number;
  name: string;
  email?: string | null;
  phone?: string | null;
  roles: PartyRole[];
  createdAt: string;
}

export interface UserRoleUpdate {
  roles: PartyRole[];
}
