# Current integration dependencies

Snapshot: 2026-10-06T04:54:15.676640+00:00

```mermaid
flowchart TD
  Rmain["tdf-app main 24d8ba45"]
  R499["tdf-app #499 ecc42d39"]
  R498["tdf-app #498 97f9f744"]
  R497["tdf-app #497 5241d979"]
  R496["tdf-app #496 4d99974d"]
  R495["tdf-app #495 f3973387"]
  R493["tdf-app #493 a849d9b8"]
  R493 -->|source ancestry or PR base| R496
  R496 -->|source ancestry or PR base| R497
  R497 -->|source ancestry or PR base| R498
  R498 -->|source ancestry or PR base| R499
  Mmain["tdf-mobile main d5e217a7"]
  M145["tdf-mobile #145 1ef15707"]
  P4cf82e2d3dd2["Mobile pin 4cf82e2d"] -.->|pinned revision| Rmain
  P4cf82e2d3dd2["Mobile pin 4cf82e2d"] -.->|pinned revision| R499
  P6f63aa4e825d["Mobile pin 6f63aa4e"] -.->|pinned revision| R498
  P6f63aa4e825d["Mobile pin 6f63aa4e"] -.->|pinned revision| R497
  P6f63aa4e825d["Mobile pin 6f63aa4e"] -.->|pinned revision| R496
  P4cf82e2d3dd2["Mobile pin 4cf82e2d"] -.->|pinned revision| R495
  P6f63aa4e825d["Mobile pin 6f63aa4e"] -.->|pinned revision| R493
```

Solid edges preserve implementation ancestry or an active PR base. Dotted edges identify exact Mobile gitlinks. Main admission is a separate protected scheduling check; an ancestry edge does not imply merge or deployment clearance.

Every retained branch, including recovery refs without PRs, is mapped in the CSV files. Removed refs retain preservation evidence and action attribution.
