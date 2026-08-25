export const WAITLIST_MIN_QUANTITY = 1;
export const WAITLIST_MAX_QUANTITY = 10;
export const WAITLIST_DEFAULT_QUANTITY = WAITLIST_MIN_QUANTITY;

export function parseWaitlistQuantity(rawValue: string): number {
  /*
   * precondition: rawValue is raw numeric input text.
   * invariant: decimal radix is explicit.
   * postcondition: invalid or zero input returns default.
   */
  const parsedQuantity = Number.parseInt(rawValue, 10);
  if (!Number.isFinite(parsedQuantity) || parsedQuantity === 0) {
    return WAITLIST_DEFAULT_QUANTITY;
  }

  return parsedQuantity;
}
