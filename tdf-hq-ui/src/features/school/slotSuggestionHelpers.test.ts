import { describe, expect, it } from '@jest/globals';
import {
  describeTeacherGap,
  lessonDurationMinutes,
  lessonSlotSuggestionsEnabled,
} from './slotSuggestionHelpers';

describe('lessonSlotSuggestionsEnabled', () => {
  it('is off by default', () => {
    expect(lessonSlotSuggestionsEnabled(undefined)).toBe(false);
    expect(lessonSlotSuggestionsEnabled(null)).toBe(false);
    expect(lessonSlotSuggestionsEnabled([])).toBe(false);
    expect(lessonSlotSuggestionsEnabled(['EVENT_DISCOVERY_ENABLED'])).toBe(false);
  });

  it('is on only when the session lists the flag', () => {
    expect(lessonSlotSuggestionsEnabled(['LESSON_SLOT_SUGGESTIONS_ENABLED'])).toBe(true);
    expect(lessonSlotSuggestionsEnabled([' lesson_slot_suggestions_enabled '])).toBe(true);
  });
});

describe('lessonDurationMinutes', () => {
  it('uses the form length when the service accepts it', () => {
    expect(lessonDurationMinutes('2030-12-02T10:00', '2030-12-02T11:30')).toBe(90);
    expect(lessonDurationMinutes('2030-12-02T10:00', '2030-12-02T10:30')).toBe(30);
    expect(lessonDurationMinutes('2030-12-02T10:00', '2030-12-02T13:00')).toBe(180);
  });

  it('falls back to 60 minutes otherwise', () => {
    expect(lessonDurationMinutes('', '')).toBe(60);
    expect(lessonDurationMinutes('2030-12-02T10:00', '2030-12-02T10:45')).toBe(60);
    expect(lessonDurationMinutes('2030-12-02T10:00', '2030-12-02T14:00')).toBe(60);
    expect(lessonDurationMinutes('2030-12-02T10:00', '2030-12-02T09:00')).toBe(60);
  });
});

describe('describeTeacherGap', () => {
  it('names the three cases', () => {
    expect(describeTeacherGap(null)).toBe('Único compromiso del profesor ese día');
    expect(describeTeacherGap(0)).toBe('Junto a otra clase del profesor');
    expect(describeTeacherGap(90)).toBe('A 90 min de otra clase del profesor');
  });
});
