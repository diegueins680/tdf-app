import type { PipelineCard } from '../../api/types';

export type PipelineRelationContext = {
  partyIds?: number[];
  normalizedNames?: string[];
};

export function normalizePipelineName(value: string) {
  return value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .trim();
}

export function buildNormalizedNames(values: Iterable<string | null | undefined>) {
  const unique = new Set<string>();
  for (const value of values) {
    if (!value) continue;
    const normalized = normalizePipelineName(value);
    if (normalized) {
      unique.add(normalized);
    }
  }
  return Array.from(unique);
}

function matchesAnyNormalizedTarget(candidate: string | null | undefined, normalizedTargets: string[]) {
  if (!candidate || normalizedTargets.length === 0) {
    return false;
  }

  const normalizedCandidate = normalizePipelineName(candidate);
  if (!normalizedCandidate) {
    return false;
  }

  return normalizedTargets.some(target => normalizedCandidate.includes(target) || target.includes(normalizedCandidate));
}

export function isPipelineCardRelated(card: PipelineCard, context: PipelineRelationContext) {
  const partyIds = context.partyIds ?? [];
  const normalizedNames = context.normalizedNames ?? [];

  if (partyIds.length > 0) {
    if (card.partyId && partyIds.includes(card.partyId)) {
      return true;
    }
    if (card.relatedPartyIds?.some(relatedId => relatedId != null && partyIds.includes(relatedId))) {
      return true;
    }
  }

  if (normalizedNames.length > 0) {
    const candidates: Array<string | null | undefined> = [
      card.clientName,
      card.artist,
      card.title,
    ];

    if (card.relatedPartyNames) {
      candidates.push(...card.relatedPartyNames);
    }

    return candidates.some(candidate => matchesAnyNormalizedTarget(candidate, normalizedNames));
  }

  return false;
}
