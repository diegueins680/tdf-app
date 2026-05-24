export function parseWaitlistQuantity(rawValue: string): number {
  const parsedQuantity = Number.parseInt(rawValue, 10);
  if (!Number.isFinite(parsedQuantity) || parsedQuantity === 0) {
    return 1;
  }

  return parsedQuantity;
}
