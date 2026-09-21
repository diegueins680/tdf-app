// SYS-ESCROW-001/002. Deliberately narrow source correspondence gate, not a Haskell parser.
// Any handler refactor requires renewed review of the no-effects argument.
export const disabledEscrowWrites = [
  { id: 'SYS-ESCROW-001', handler: 'createServiceMarketplaceBooking',
    message: 'Service marketplace booking is unavailable until verified escrow is supported' },
  { id: 'SYS-ESCROW-002', handler: 'releaseServiceMarketplaceEscrow',
    message: 'Service marketplace escrow release is unavailable until verified escrow is supported' },
];

export function checkDisabledEscrowWrites(source) {
  for (const { id, handler, message } of disabledEscrowWrites) {
    const declarations = [...source.matchAll(new RegExp(`^${handler} ::[^\\n]*\\n([\\s\\S]*?)(?=^\\w+ ::|$(?![\\s\\S]))`, 'gm'))];
    if (declarations.length !== 1) throw new Error(`${id}: expected exactly one handler declaration`);
    const body = declarations[0][1].replace(/^--[^\n]*\n/gm, '').trim();
    const expected = `${handler} _ _ =\n  throwError err503 { errBody = "${message}" }`;
    if (body !== expected) throw new Error(`${id}: handler no-effects correspondence must be re-established`);
  }
  return { status: 'recognized-no-effects-handler-bodies', obligations: disabledEscrowWrites.map(({ id }) => id) };
}
