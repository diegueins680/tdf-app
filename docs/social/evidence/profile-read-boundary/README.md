# Verification evidence

Actual local results for the source files fingerprinted in `sources.sha256`.
Text logs have trailing whitespace normalized. `stack-final.txt` is an explicitly
labeled summary excerpt of the full successful run; its original digest is retained.
`raw-log-digests.sha256` records original pre-normalization logs. Expected negative
TLC/SQL failures and the unsuccessful scalar benchmark remain evidence, not passes.
`bulk-http-benchmark.txt` contains the final positive SQL/HTTP/performance run.
`results.json` records observed process exits. Hosted CI is tracked separately.
