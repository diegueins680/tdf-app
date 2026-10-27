# Music purchase asset admission

Requirement: PAY-MUSIC-DOWNLOAD-001. Base: main
`301810ae3861ea36b1fa790beee940bfe2461a02` (227 migrations).

Purchase admission now requires a same-version asset and the exact existing
`downloadableAssetPredicate`, also used by free grants and entitled downloads.
The old offer SQL accepted all324 synthetic combinations;304 disagree with the
download policy. The corrected query accepts20 and rejects304. Removing only
the predicate gives142 mismatches; removing only version equality gives20.

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

Current qualification receipts and tools will be appended after execution on a
clean source commit. Complete requirement/refinement proof remains UNPROVEN.
This check samples eligibility at offer-query time; it establishes no lock
through provider capture, future asset availability, object bytes, provider
truthfulness, historical-data repair or production correspondence. Existing
provider/transport/activation blockers remain; keep music commerce disabled.

A stronger database counterexample also constructs the complete graph from
`MusicPublicationSpec` with just two input changes: the original cover is
`valid`, and the Worldwide250USD purchase rule references that cover. Ordinary
submission checks report zero errors; approval/scheduling/publication triggers
remain enabled throughout; `music_publish_due` publishes one version and the
public view returns it. Original offer SQL accepts it; corrected SQL rejects it.
This establishes one reachable database publication, without claiming that all
324 projected combinations pass publication or that HTTP/session authorization
and provider execution have been established. The synthetic authoring fixture
is committed in a separate disposable database to advance transaction time;
the subsequent publication/read transaction is rolled back. No real provider
or object store is contacted.
