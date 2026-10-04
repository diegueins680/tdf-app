import { useMemo } from 'react';
import { Parties } from '../api/parties';
import type { PartyCreate } from '../api/types';

// One scope is one user-requested creation. Shared fields never identify a person.
// Keep the key after failures (including failures later in a multi-step form).
export function createContactCreation() {
    const requests = new Map<string, string>();
    return {
      create(body: PartyCreate, scope = 'form') {
        let key = requests.get(scope);
        if (!key) {
          key = crypto.randomUUID();
          requests.set(scope, key);
        }
        // Edits after an uncertain response retain the key. The server detects a
        // changed accepted payload instead of silently creating another person.
        return Parties.create(body, key);
      },
      reset() { requests.clear(); },
    };
}

export function useContactCreation() {
  return useMemo(createContactCreation, []);
}
