/**
 * Helper utilities for managing party roles and conversions
 * Avoids data duplication by using party records as the source of truth
 */

import { Parties } from '../api/parties';
import type { PartyDTO, RoleKey } from '../api/types';
import { hqClient } from '../api/hq/client';

export interface ConvertToStudentOptions {
  /** Whether to create a student record in the lessons system */
  createStudentRecord?: boolean;
  /** Additional notes to add to the party */
  notes?: string;
}

export interface ConvertToTeacherOptions {
  /** Whether to create a teacher record in the lessons system */
  createTeacherRecord?: boolean;
  /** Additional notes to add to the party */
  notes?: string;
}

/**
 * Convert an existing party to a student
 * This adds the Student role and optionally creates a student record
 */
export async function convertPartyToStudent(
  party: PartyDTO,
  options: ConvertToStudentOptions = {}
): Promise<{ partyId: number; studentId?: string }> {
  const { createStudentRecord = true, notes } = options;

  // Step 1: Add Student role to the party
  await Parties.addRole(party.partyId, 'Student');

  // Step 2: Update notes if provided
  if (notes) {
    const currentNotes = party.notes || '';
    const updatedNotes = currentNotes
      ? `${currentNotes}\n\n[Convertido a estudiante: ${new Date().toISOString()}]\n${notes}`
      : `[Convertido a estudiante: ${new Date().toISOString()}]\n${notes}`;
    
    await Parties.update(party.partyId, { uNotes: updatedNotes });
  }

  // Step 3: Optionally create a student record in the lessons system
  let studentId: string | undefined;
  if (createStudentRecord) {
    const response = await hqClient.POST('/api/students', {
      body: {
        name: party.displayName,
        email: party.primaryEmail || undefined,
        phone: party.primaryPhone || undefined,
      },
    });

    if (response.data && typeof response.data === 'object' && 'id' in response.data) {
      studentId = (response.data as { id: string }).id;
    }
  }

  return { partyId: party.partyId, studentId };
}

/**
 * Convert an existing party to a teacher
 * This adds the Teacher role and optionally creates a teacher record
 */
export async function convertPartyToTeacher(
  party: PartyDTO,
  options: ConvertToTeacherOptions = {}
): Promise<{ partyId: number; teacherId?: string }> {
  const { createTeacherRecord = true, notes } = options;

  // Step 1: Add Teacher role to the party
  await Parties.addRole(party.partyId, 'Teacher');

  // Step 2: Update notes if provided
  if (notes) {
    const currentNotes = party.notes || '';
    const updatedNotes = currentNotes
      ? `${currentNotes}\n\n[Convertido a profesor: ${new Date().toISOString()}]\n${notes}`
      : `[Convertido a profesor: ${new Date().toISOString()}]\n${notes}`;
    
    await Parties.update(party.partyId, { uNotes: updatedNotes });
  }

  // Step 3: Optionally create a teacher record in the lessons system
  let teacherId: string | undefined;
  if (createTeacherRecord) {
    const response = await hqClient.POST('/api/teachers', {
      body: {
        name: party.displayName,
        email: party.primaryEmail || undefined,
        phone: party.primaryPhone || undefined,
      },
    });

    if (response.data && typeof response.data === 'object' && 'id' in response.data) {
      teacherId = (response.data as { id: string }).id;
    }
  }

  return { partyId: party.partyId, teacherId };
}

/**
 * Check if a party has a specific role
 */
export function hasRole(party: PartyDTO, role: RoleKey): boolean {
  // This would need to be implemented based on how roles are stored
  // For now, this is a placeholder
  return false;
}

/**
 * Get all roles for a party
 */
export async function getPartyRoles(partyId: number): Promise<RoleKey[]> {
  // This would call an endpoint like /parties/{id}/roles
  // For now, this is a placeholder
  return [];
}

/**
 * Bulk convert multiple parties to students
 */
export async function bulkConvertToStudents(
  parties: PartyDTO[],
  options: ConvertToStudentOptions = {}
): Promise<Array<{ partyId: number; studentId?: string; error?: string }>> {
  const results = await Promise.allSettled(
    parties.map(party => convertPartyToStudent(party, options))
  );

  return results.map((result, index) => {
    if (result.status === 'fulfilled') {
      return result.value;
    } else {
      return {
        partyId: parties[index].partyId,
        error: result.reason?.message || 'Unknown error',
      };
    }
  });
}
