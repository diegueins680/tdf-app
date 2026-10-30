# Music purchase asset admission

Requirement: PAY-MUSIC-DOWNLOAD-001. Base: main
`301810ae3861ea36b1fa790beee940bfe2461a02` (227 migrations).

Purchase admission now requires a same-version asset and the exact existing
`downloadableAssetPredicate`, also used by free grants and entitled downloads.
The old offer SQL accepted all 324 synthetic combinations;304 disagree with the
download policy. The corrected query accepts 20 and rejects 304. Removing only
the predicate gives 142 mismatches; removing only version equality gives 20.

Reproduce the extracted SQL check on a fresh disposable migrated database:

```sh
sh scripts/load-payment-test-schema.sh "$TDF_MUSIC_PURCHASE_DATABASE_URL"
python3 scripts/test-music-purchase-asset-query.py
```

The runner validates its explicit loopback/CI `_test` URL, fails closed on
incomplete discovery/results or undetected mutations, and fingerprints its
inputs before and after execution. It uses temporary relation projections in
rolled-back connections. Nine new Hspec handler cases are selected by the
existing `scripts/test-music-purchase.sh` after the Stack test binary is built.
Script-only changes select backend CI; full handler execution is separate.

The matrix enumerates nine states, three roles, two immutable values, three
storage classes and two version relationships. It abstracts the full asset
graph and does not claim every combination can pass authoring/publication.
The additional real-schema SQL probe uses a same-version immutable `valid`
`cover_original`: original query returns one offer, corrected query returns zero.
It uses the complete227-migration schema and the existing synthetic published
fixture convention (publication triggers bypassed for insertion, CHECK
constraints retained, ordinary triggers restored before reads). It is not an
HTTP or publication test.

Clean source qualification and precise tools are recorded below. Complete
requirement/refinement proof remains UNPROVEN.
This check samples eligibility at offer-query time; it establishes no lock
through provider capture, future asset availability, object bytes, provider
truthfulness, historical-data repair or production correspondence. Existing
provider/transport/activation blockers remain; keep music commerce disabled.

A stronger database counterexample also constructs the complete graph from
`MusicPublicationSpec` with just two input changes: the original cover is
`valid`, and the Worldwide purchase rule for 250 USD minor units ($2.50) references that cover. Ordinary
submission checks report zero errors; approval/scheduling/publication triggers
remain enabled throughout; `music_publish_due` publishes one version and the
public view returns it. Original offer SQL accepts it; corrected SQL rejects it.
This establishes one reachable database publication, without claiming that all
324 projected combinations pass publication or that HTTP/session authorization
and provider execution have been established. The synthetic authoring fixture
is committed in a separate disposable database to advance transaction time;
the subsequent publication/read transaction is rolled back. No real provider
or object store is contacted.

## Completed qualification

Clean integrated source: `4f7cd24d03ccf121be235bdd5160ed086d832451`, normally
merging main ff39 through 34662. Native build/test source:
`c2cd0a2d8f42c15d03cb169190040ce37ac1beec`. All 454 backend input hashes match
the integrated source; this correspondence does not relabel the compiled SHA.
Main UI/E2E/SQL227/API surface/Mobile7000 and every 1351 catalog decision are
preserved. All 154 main requirement objects remain except the specified
PAY-MUSIC-DOWNLOAD-001 amendment.

Actual Stack3.7.1/GHC9.10.3/Hspec2.11.17/Aeson2.2.5.0/Text2.1.3/Time1.12.2,
Python3.13.5, Node24.13.1 and PostgreSQL16.13 on macOS12.7.6. Only Stack invokes
the compiler. Both the server executable target and test component complete
successfully. The canonical `scripts/test-music-purchase.sh` passes 32 real
handler cases with no pending cases and the 324-case SQL matrix/two controls.

The native control compiles the exact old main301 Commerce module, retains the
new tests, and requires32 examples/nine failures/no pending/exit1. Each failure
is expected404 versus actual201. After restoring the source and rebuilding, the
same seeded group requires32/0/0/exit0. Full logs and control source are archived;
the server executable is not used in that test-component mutation. Earlier
interrupted builds, the unbuilt-server package-copy failure and the10-connection
fixture failure are retained. Final builds and runner genuinely complete0; the
private database capacity is40, with original assertions/concurrency unchanged.

Integrated-source repository quality, source admission (seven mapped material
paths),19 scope checks and strict catalog1351/zero unreviewed pass. Fresh solver
and replay execution passes with 1409 matching inputs. The 11 existing conditional
SMT claims retain their assumptions; all 154 complete requirements and 308
implementation/refinement obligations remain UNPROVEN, release BLOCKED.

`artifacts.tar.gz` contains the raw native/control logs, exact source inputs,
publication SQL/Python/result, previous failures, repository checks, catalog and
fresh assurance outputs. `qualification.json` binds each entry and both artifacts
by SHA256 and records the assurance scorecard. Reproduce publication only in a
fresh explicitly disposable schema using the recorded dedicated loopback test
cluster/database; its authoring fixture is committed between transactions as
documented above. The archived scripts retain the exact recorded workspace and
loopback test paths; adapt those paths when reproducing elsewhere.

Next obligation: canonical provider admission plus durable ambiguous-outcome
reconciliation before music commerce activation; retain no-charge/activation
guards meanwhile. Independent current-head review, hosted CI and canonical FIFO
admission are required before integration. No main merge, deployment or provider
activation was performed for this repair.
