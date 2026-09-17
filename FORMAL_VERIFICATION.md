# Formal Verification

This repo uses two complementary levels of formal work:

- lightweight deterministic model checks and source audits for broad repository feedback; and
- bounded TLA+/PlusCal and Alloy analysis for high-risk event-operations concurrency,
  authorization, lifecycle, RACI, contract, and financial gates.

The bounded analyses are not universal proofs. Their exact tool versions, scopes, fairness
assumptions, results, counterexamples, and limitations are recorded in
`formal/event-operations/README.md`.

## Commands

```bash
npm run verify:formal
npm run verify:event-operations:formal
npm run audit:formal
npm run audit:formal -- --json
npm run audit:formal -- --fail-on warning
npm run test:formal
```

`npm run quality` runs the formal gate before linting, type-checking, UI tests, mobile checks, and backend tests.

The event-operations command additionally requires `JAVA_BIN` (optional), `TLA2TOOLS_JAR`, and
`ALLOY_JAR`. CI downloads the checksum-pinned official releases in the dedicated
`event-operations-formal.yml` workflow.

## Gate Policy

- `verify:formal` model-checks the continuous-improvement loop and runs the formal audit.
- `verify:event-operations:formal` runs TLC sequentially, requires a satisfiable Alloy scenario,
  and rejects any bounded Alloy counterexample.
- CI fails on `error` or `critical` findings by default.
- `warning` and `info` findings are advisory debt: visible, sorted by importance, and suitable for follow-up loop work.
- The formal audit scans tracked active source only. It intentionally skips archives, generated clients, build outputs, mobile native generated folders, and dependency directories.

## When To Add Formal Coverage

Add or update formal coverage when a change touches:

- authorization, role grants, session state, or credential handling;
- payment, ticketing, invoice, refund, webhook, or signature logic;
- data synchronization, idempotency, dedupe, or retry behavior;
- state machines, schedulers, supervisors, CI repair loops, or background workers;
- parsers, normalizers, validators, or API payload boundaries.

## Haskell Practices

- Prefer total functions returning `Either`, `Maybe`, or `ServerError` over partial functions or exceptions.
- Use explicit module export lists to keep constructors and helper functions private unless they are part of the contract.
- Encode invariants in types where possible: newtypes, bounded enums, domain-specific parsers, and smart constructors.
- Add QuickCheck properties for normalization, round trips, idempotency, authorization lattice behavior, and signature verification.
- Acquire resources with `bracket`, `finally`, or `with...` helpers.

## TypeScript Practices

- Use discriminated unions and exhaustive switches for workflow state.
- Put runtime validation at untrusted boundaries: API responses, form payloads, local storage, and webhook-like inputs.
- Avoid boolean traps in exported functions; use named options or enums.
- Document exported side effects and preconditions when types alone cannot carry them.
- Keep generated API clients out of manual formal findings; verify their source OpenAPI contract instead.

## State Machine Pattern

State-machine code should expose a pure model and a verifier:

```js
export function buildThingModel() {
  return {
    initial: 'start',
    transitions: {
      start: ['validate'],
      validate: ['commit', 'repair'],
      repair: ['validate'],
      commit: ['done'],
      done: ['start'],
    },
  };
}

export function verifyThingModel() {
  // Check reachability, dead ends, forbidden paths, and liveness.
}
```

Keep the verifier deterministic and covered by tests that prove it catches broken models, not only that the current model passes.
