# Player proofs

MEDIA-MUSIC-CONTINUATION-001. Contract: [../system/queue-continuation.md](../system/queue-continuation.md).

## Queue continuation (`QueueContinuation.tla`)

When the web player queue ends, the client walks the artists the server recommended, in
the server's order, and appends at most one recording per artist that is not queued,
up to a limit. The server response is abstract (a served set and a strict order); the
model shows the client adds nothing to it and keeps its order. The client's "first
eligible track" is modelled as "any eligible recording", so the invariants hold for
every choice; determinism of the choice is tested in
`tdf-hq-ui/src/player/queueContinuation.test.ts`, not modelled.

`Safe` = every appended recording belongs to a served artist (`Subset`); appended
artists follow the server order, hence one per artist (`Order`); nothing queued is
appended and nothing twice (`NoRepeat`); at most `Limit` (`Bound`).

### Results (`npm run verify:inductive-proofs`)

| Obligation | Tool | Result |
|---|---|---|
| Three artists, three recordings, limit 2, every served set, order, catalogue and queue | TLC 1.7.2 | no violation (852,222 states) |
| Same, limit 3 | TLC | no violation (870,654 states) |
| No check against the queue | TLC | `NoRepeat` violated, as expected |
| `Init => IndInv` | Apalache 0.62.4 | NoError |
| `IndInv /\ Next => IndInv'` | Apalache | NoError |
| `IndInv => Safe` | Apalache | NoError |
| No check against the queue, three steps from `Init` | Apalache | `NoRepeat` violated, as expected |

The Apalache obligations range over three artists, four recordings, every limit in
0..3 and walks of any length. They are proofs about this model. The TypeScript
function is related to it by review and by random tests of the same four invariants.
