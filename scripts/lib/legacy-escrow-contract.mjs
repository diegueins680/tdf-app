// SYS-ESCROW-001/002. Deliberately narrow source correspondence gate, not a Haskell parser.
// Any handler refactor requires renewed review of the no-effects argument.
export const disabledEscrowWrites = [
  { id: 'SYS-ESCROW-001', handler: 'createServiceMarketplaceBooking',
    message: 'Service marketplace booking is unavailable until verified escrow is supported' },
  { id: 'SYS-ESCROW-002', handler: 'releaseServiceMarketplaceEscrow',
    message: 'Service marketplace escrow release is unavailable until verified escrow is supported' },
];

const normalize = source => source.replace(/^\s*--[^\n]*/gm, '').trim().replace(/\s+/g, ' ');

function definition(source, name) {
  const matches = [...source.matchAll(new RegExp(`^${name} ::[^\\n]*\\n([\\s\\S]*?)(?=^\\w+ ::|$(?![\\s\\S]))`, 'gm'))];
  if (matches.length !== 1) throw new Error(`${name}: expected exactly one handler declaration`);
  return matches[0][1].replace(/^--[^\n]*\n/gm, '').trim();
}

function apiType(source, name) {
  const matches = [...source.matchAll(new RegExp(`^type ${name} =\\n([\\s\\S]*?)(?=^\\S|$(?![\\s\\S]))`, 'gm'))];
  if (matches.length !== 1) throw new Error(`${name}: expected exactly one API declaration`);
  return normalize(matches[0][1]);
}

export function checkDisabledEscrowWrites(source, apiSource) {
  for (const { id, handler, message } of disabledEscrowWrites) {
    const body = definition(source, handler);
    const expected = `${handler} _ _ =\n  throwError err503 { errBody = "${message}" }`;
    if (body !== expected) throw new Error(`${id}: handler no-effects correspondence must be re-established`);
  }
  const dispatch = normalize(definition(source, 'serviceMarketplaceServer'));
  const expectedDispatch = `serviceMarketplaceServer user =
       listServiceAds
  :<|> createServiceAd user
  :<|> listServiceAdSlots
  :<|> createServiceAdSlot user
  :<|> createServiceMarketplaceBooking user
  :<|> completeServiceMarketplaceBooking user
  :<|> releaseServiceMarketplaceEscrow user`;
  if (dispatch !== normalize(expectedDispatch)) throw new Error('Escrow route composition changed');
  const expectedApi = `
       "service-marketplace" :> "ads" :> Get '[JSON] [ServiceAdDTO]
  :<|> "service-marketplace" :> "ads" :> ReqBody '[JSON] ServiceAdCreateReq :> Post '[JSON] ServiceAdDTO
  :<|> "service-marketplace" :> "ads" :> Capture "adId" Int64 :> "slots" :> Get '[JSON] [ServiceAdSlotDTO]
  :<|> "service-marketplace" :> "ads" :> Capture "adId" Int64 :> "slots" :> ReqBody '[JSON] ServiceAdSlotCreateReq :> Post '[JSON] ServiceAdSlotDTO
  :<|> "service-marketplace" :> "bookings" :> ReqBody '[JSON] ServiceMarketplaceBookingReq :> Post '[JSON] ServiceMarketplaceBookingDTO
  :<|> "service-marketplace" :> "bookings" :> Capture "bookingId" Int64 :> "complete" :> Post '[JSON] ServiceMarketplaceBookingDTO
  :<|> "service-marketplace" :> "bookings" :> Capture "bookingId" Int64 :> "escrow" :> "release" :> Post '[JSON] ServiceMarketplaceBookingDTO`;
  if (apiType(apiSource, 'ServiceMarketplaceAPI') !== normalize(expectedApi)) {
    throw new Error('Escrow API ordering or contract changed');
  }
  // Match the actual parent positions too; retaining an unused safe server is insufficient.
  const protectedIndex = apiType(apiSource, 'ProtectedAPI').split(':<|>').map(x => x.trim()).indexOf('ServiceMarketplaceAPI');
  const protectedBranches = normalize(definition(source, 'protectedServer')).split(':<|>').map(x => x.trim());
  if (protectedIndex < 0 || protectedBranches[protectedIndex] !== 'serviceMarketplaceServer user') {
    throw new Error('Escrow protected route binding changed');
  }
  const rootIndex = apiType(apiSource, 'API').split(':<|>').map(x => x.trim()).indexOf('AuthProtect "bearer-token" :> ProtectedAPI');
  const rootBranches = normalize(definition(source, 'server')).split(':<|>').map(x => x.trim());
  if (rootIndex < 0 || rootBranches[rootIndex] !== 'protectedServer') {
    throw new Error('Escrow authenticated root binding changed');
  }
  return { status: 'recognized-no-effects-handlers-and-routes', obligations: disabledEscrowWrites.map(({ id }) => id) };
}
