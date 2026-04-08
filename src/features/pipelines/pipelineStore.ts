import { useMemo, useSyncExternalStore } from 'react';
import type { PipelineCard } from '../../api/types';
import { buildNormalizedNames, isPipelineCardRelated } from './pipelineFilters';

export type PipelineBoardCard = PipelineCard & {
  partyId?: number;
  clientName?: string;
};

export const MIXING_STAGES = ['Brief','Prep','v1 Sent','Revisions','Approved','Delivered'] as const;
export const MASTERING_STAGES = ['Brief','v1','Revisions','Approved','DDP Delivered'] as const;

const INITIAL_CARDS: PipelineBoardCard[] = [
  { id: 'mx-1', title: 'Arkabuz - Single A', artist: 'Arkabuz', type: 'Mixing', stage: 'Brief', clientName: 'Arkabuz' },
  { id: 'mx-2', title: 'Quimika - EP', artist: 'Quimika Soul', type: 'Mixing', stage: 'Prep', clientName: 'Quimika Soul' },
  { id: 'ma-1', title: 'Skanka Fe - LP', artist: 'Skanka Fe', type: 'Mastering', stage: 'v1', clientName: 'Skanka Fe' },
  { id: 'ma-2', title: 'El Bloque - Single', artist: 'El Bloque', type: 'Mastering', stage: 'Approved', clientName: 'El Bloque' },
];

let cards: PipelineBoardCard[] = [...INITIAL_CARDS];
const listeners = new Set<() => void>();

function emit() {
  listeners.forEach(listener => listener());
}

function subscribe(listener: () => void) {
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
  };
}

function getSnapshot() {
  return cards;
}

export function usePipelineCards() {
  return useSyncExternalStore(subscribe, getSnapshot, getSnapshot);
}

export function findPipelineCard(cardId: string) {
  return cards.find(card => card.id === cardId);
}

export function updatePipelineCardStage(cardId: string, newStage: string) {
  let updated: PipelineBoardCard | undefined;
  cards = cards.map(card => {
    if (card.id === cardId) {
      updated = { ...card, stage: newStage };
      return updated;
    }
    return card;
  });
  if (updated) {
    emit();
  }
  return updated;
}

export function usePipelineCardsForParty(
  party: { partyId?: number | null; displayName?: string | null } | null,
  options?: {
    extraNames?: Array<string | null | undefined>;
    relatedPartyIds?: Array<number | null | undefined>;
  },
) {
  const allCards = usePipelineCards();
  return useMemo(() => {
    if (!party) {
      return [] as PipelineBoardCard[];
    }
    const partyIds = new Set<number>();
    if (party.partyId) {
      partyIds.add(party.partyId);
    }
    (options?.relatedPartyIds ?? []).forEach(id => {
      if (id) {
        partyIds.add(id);
      }
    });

    const nameCandidates: Array<string | null | undefined> = [party.displayName];
    if (options?.extraNames) {
      nameCandidates.push(...options.extraNames);
    }

    const context = {
      partyIds: Array.from(partyIds),
      normalizedNames: buildNormalizedNames(nameCandidates),
    };

    if (context.partyIds.length === 0 && context.normalizedNames.length === 0) {
      return [] as PipelineBoardCard[];
    }

    return allCards.filter(card => isPipelineCardRelated(card, context));
  }, [allCards, party?.partyId, party?.displayName, options?.extraNames, options?.relatedPartyIds]);
}

export function resetPipelineCards(next?: PipelineBoardCard[]) {
  cards = next ? [...next] : [...INITIAL_CARDS];
  emit();
}
