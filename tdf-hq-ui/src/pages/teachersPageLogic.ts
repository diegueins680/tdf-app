export const resolveTeacherClasses = <T>(
  apiData: readonly unknown[] | undefined,
  apiClasses: readonly T[],
  fallbackClasses: readonly T[],
): readonly T[] => (apiData === undefined ? fallbackClasses : apiClasses);
