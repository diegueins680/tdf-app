import { useMemo } from 'react';
import { Parties } from '../api/parties';
import type { PartyCreate } from '../api/types';

// One scope is one user-requested creation. Shared fields never identify a person.
// Keep the key after failures (including failures later in a multi-step form).
export function createContactCreation() {
    const requests = new Map<string, { body: string; key: string }>();
    return {
      create(body: PartyCreate, scope = 'form') {
        const serialized = JSON.stringify(body);
        let request = requests.get(scope);
        if (request?.body !== serialized) {
          request = { body: serialized, key: crypto.randomUUID() };
          requests.set(scope, request);
        }
        return Parties.create(body, request.key);
      },
      reset() { requests.clear(); },
    };
}

export function useContactCreation() {
  return useMemo(createContactCreation, []);
}
