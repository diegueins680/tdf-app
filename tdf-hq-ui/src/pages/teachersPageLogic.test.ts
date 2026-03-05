import { resolveTeacherClasses } from './teachersPageLogic';

describe('resolveTeacherClasses', () => {
  it('uses fallback rows while API data is still loading', () => {
    expect(resolveTeacherClasses(undefined, ['api-class'], ['slot-class'])).toEqual(['slot-class']);
  });

  it('keeps API rows once request resolves with empty results', () => {
    expect(resolveTeacherClasses([], [], ['slot-class'])).toEqual([]);
  });

  it('prefers API rows when request resolves with data', () => {
    expect(resolveTeacherClasses([{}], ['api-class'], ['slot-class'])).toEqual(['api-class']);
  });
});
