
import { get, patch } from './client';
import type { PipelineCard } from './types';

const rawFlag = (import.meta.env.VITE_PIPELINES_API_ENABLED ?? '').toString().trim().toLowerCase();
const PIPELINES_API_ENABLED = ['1', 'true', 'yes', 'on'].includes(rawFlag);

let warnedDisabled = false;
let warnedUnavailable = false;

const DEMO_PIPELINE_CARDS: PipelineCard[] = [
  { id: 'mx-1', title: 'Arkabuz - Single A', artist: 'Arkabuz', clientName: 'Arkabuz', type: 'Mixing', stage: 'Brief' },
  { id: 'mx-2', title: 'Quimika - EP', artist: 'Quimika Soul', clientName: 'Quimika Soul', type: 'Mixing', stage: 'Prep' },
  { id: 'ma-1', title: 'Skanka Fe - LP', artist: 'Skanka Fe', clientName: 'Skanka Fe', type: 'Mastering', stage: 'v1' },
  { id: 'ma-2', title: 'El Bloque - Single', artist: 'El Bloque', clientName: 'El Bloque', type: 'Mastering', stage: 'Approved' },
];

function warnDisabledOnce() {
  if (!warnedDisabled) {
    console.info('Pipeline API deshabilitada; se usan datos en memoria.');
    warnedDisabled = true;
  }
}

function warnUnavailableOnce() {
  if (!warnedUnavailable) {
    console.warn('Pipeline API no disponible todavía; se usan datos en memoria.');
    warnedUnavailable = true;
  }
}

function fallbackCardsForParty(partyId: number): PipelineCard[] {
  if (!partyId || DEMO_PIPELINE_CARDS.length === 0) {
    return [];
  }

  const first = DEMO_PIPELINE_CARDS[partyId % DEMO_PIPELINE_CARDS.length];
  const second = DEMO_PIPELINE_CARDS[(partyId + 1) % DEMO_PIPELINE_CARDS.length];
  const base = [first, second].filter((card, index, arr) => arr.findIndex(c => c.id === card.id) === index);

  return base.map((card, idx) => ({
    ...card,
    id: `${card.id}-party-${partyId}-${idx}`,
    title: `${card.title} — Cliente #${partyId}`,
  }));
}

export function getDemoPipelineCards(): PipelineCard[] {
  return DEMO_PIPELINE_CARDS.map(card => ({ ...card }));
}

export async function listByParty(partyId: number): Promise<PipelineCard[]> {
  if (!partyId) {
    return [];
  }

  if (!PIPELINES_API_ENABLED) {
    warnDisabledOnce();
    return fallbackCardsForParty(partyId);
  }

  try {
    return await get<PipelineCard[]>(`/pipelines/parties/${partyId}`);
  } catch (error) {
    warnUnavailableOnce();
    return fallbackCardsForParty(partyId);
  }
}

// Hook point: when backend exposes endpoints, call them here.
// Example PATCH: /pipelines/:type/:id { stage }
export async function updateStage(card: PipelineCard, newStage: string) {
  if (!PIPELINES_API_ENABLED) {
    warnDisabledOnce();
    return;
  }

  try {
    await patch<void>(`/pipelines/${card.type.toLowerCase()}/${card.id}`, { stage: newStage });
  } catch (error) {
    warnUnavailableOnce();
  }
}
