/**
 * Returns the first truthy string, preserving the fallback behavior of a
 * logical-OR chain without conflating it with nullish coalescing.
 */
export function firstNonEmptyString(
  ...values: readonly (string | null | undefined)[]
): string {
  return values.find((value): value is string => Boolean(value)) ?? '';
}
