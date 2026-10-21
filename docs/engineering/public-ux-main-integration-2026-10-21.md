# Public booking, discovery and chat integration — 21 October 2026

The existing parent PR #552 reconciles public UX parent
`e99978c4ab1ed9c78fce36fbb366b1d10dd96fae` with protected main
`0a37123ea1d9d6b7c6b44d73c9342e18515c06c9`. It retains source PR ancestry
from #560, #576 and #578 and the Operations/music work already on main.
Source PRs remain open until their successor merges and its resulting tree is verified.

The runtime changes remain those of the original public UX candidate: every
published service is selectable from the booking contact step; legacy DJ-booth
URLs redirect into ordinary booking; discovery uses the canonical search
projection; chat notification navigation requires fresh authorization for that
navigation before exposing messages or updating read bookkeeping. Drafts survive
same-route navigation. The notification popover closes before destination input.

## Preservation

- All 127 requirements and 72 authority decisions retain verified three-way field
  correspondence. Both parents' complete CI and formal commands remain present.
- Main's 208 migrations remain an exact prefix. The three original community
  search, genre-alias synchronization and chat-notification migrations follow in
  their original order. No introduction SHA or SQL byte is rewritten.
- Web source, feature registry and OpenAPI are byte-identical to the public UX
  parent. Generated web/Mobile clients match that contract.
- Mobile remains pinned to `3194982a01174d36017533e79400ce9209086162`, preserving
  main's earlier `4c84c60a5836f7d39f18e10d34d136996bb0f63f` as an ancestor.
  Protected Mobile PR #162 merged as `80d57fd70341e45f2a4f94dd2607f321ebfd0400`
  after exact-head independent approval, required checks, a current base and no
  unresolved conversations. The resulting tree equals the approved head; its
  post-merge validation and configured synthetics pass. This is repository
  integration, not a device or store release.

## Catalog evidence

`docs/catalog-persistence/public-ux-main-reconciliation-20261021.json` retains
72 concurrent review differences and both original manifest reviews through
immutable Git file/entry hashes. Main's source-specific active reviews and
original retirement bytes/hashes remain intact. The active 211-entry manifest
review binds the complete protected prefix and the three appended entries.

Five obsolete main fingerprints are historical evidence: exclusive DJ-booth
navigation/cards/links and web/Mobile notification switches that now include
chat. Each records its current replacement or the source reason a single-value
link list no longer forms a scanner candidate. Old approvals cannot authorize a
future reintroduced fingerprint. Existing successor mappings follow the current
public navigation and manifest while retaining both parent records.

The inherited music reconciliation regression now permits later append-only
migrations while checking the exact protected 208-entry prefix, original music
SQL hashes, original reviews and immutable retired manifest decision. It does
not relax historical migration integrity to an arbitrary new manifest.

## Qualification and limits

Fresh results belong to the integration revision reported in the PR and private
receipts. Relevant checks are full web lint/typecheck/tests/build and bundle
budget, actual production-bundle notification navigation across browser engines,
strict catalog audit, immutable migration introductions, exact Mobile generated
clients, source inventory/conformance and changed-surface admission. On a fresh
owned loopback `_test` database, apply/replay all 211 migrations, execute community
ranking/privacy, genre-alias synchronization with rollback and disabled-trigger
counterexamples, and chat notification properties. Hosted PG17, compiled backend
and complete bounded models remain required; local PG16 and browser fixtures do
not establish deployed authorization or production delivery.

The prior public UX runtime source `f56087352` and metadata successor `e99978c4a`
have their original qualification history: 294 web suites / 3,268 tests, 30 browser
cases, and bounded models with documented abstractions. These are historical
results, not a substitute for fresh current-head CI. An initial local integration
setup omitted workspace-local dependency links and failed the bundle configuration
probe before web checks; it is retained as a setup failure, not a test pass.
The current local host is macOS12, which the pinned Playwright WebKit does not
support. Chromium and Firefox install successfully; current WebKit qualification
must run on the supported hosted platform rather than reusing historical results.

The guarded Hetzner recovery executor, coordinated assets/database/private-upload
recovery, independent key custody, restricted canary and Operations capture
retention/deletion/backup policy remain separate release prerequisites. This PR
performs no production migration, deployment, provider action or feature activation.
