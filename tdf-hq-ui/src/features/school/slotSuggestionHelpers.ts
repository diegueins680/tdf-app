// SCHOOL-SLOT-SUGGESTION-001: pure helpers for the lesson slot suggestion panel.

/** Session flag reported by the backend when LESSON_SLOT_SUGGESTIONS_ENABLED is on. */
export const LESSON_SLOT_SUGGESTIONS_FLAG = 'LESSON_SLOT_SUGGESTIONS_ENABLED';

export const DEFAULT_LESSON_MINUTES = 60;
const GRID_MINUTES = 30;
const MIN_LESSON_MINUTES = 30;
const MAX_LESSON_MINUTES = 180;

/** Off unless the session lists the flag; a missing or malformed list is off. */
export function lessonSlotSuggestionsEnabled(featureFlags: readonly string[] | null | undefined): boolean {
  return (featureFlags ?? []).some(
    (flag) => typeof flag === 'string' && flag.trim().toUpperCase() === LESSON_SLOT_SUGGESTIONS_FLAG,
  );
}

/**
 * The lesson length to ask for, from the form's start and end. The service
 * accepts 30 to 180 minutes in steps of 30; anything else falls back to 60.
 */
export function lessonDurationMinutes(startAt: string, endAt: string): number {
  const start = new Date(startAt).getTime();
  const end = new Date(endAt).getTime();
  if (!Number.isFinite(start) || !Number.isFinite(end)) return DEFAULT_LESSON_MINUTES;
  const minutes = (end - start) / 60000;
  const valid =
    Number.isInteger(minutes) &&
    minutes >= MIN_LESSON_MINUTES &&
    minutes <= MAX_LESSON_MINUTES &&
    minutes % GRID_MINUTES === 0;
  return valid ? minutes : DEFAULT_LESSON_MINUTES;
}

/** Why a slot ranks where it does, in the order the service applies. */
export function describeTeacherGap(teacherGapMinutes: number | null): string {
  if (teacherGapMinutes === null) return 'Único compromiso del profesor ese día';
  if (teacherGapMinutes === 0) return 'Junto a otra clase del profesor';
  return `A ${teacherGapMinutes} min de otra clase del profesor`;
}
