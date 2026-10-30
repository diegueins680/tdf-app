# Checkout-money execution evidence

Clean qualification source: `fe6d2eb768137c3fe8b274a98c5e53d2dd2cc8c1`, based on main `63c56aaf919a07e919a61280311be1dc809cd6fe`. No financial Haskell, original tests, SQL or223-entry migration manifest changes.

The old verifier accepted all ten variants after all three `withMaxSuccess 1000` limits in `CheckoutMoneySpec.hs` were reduced to1. Its output still claimed three1000-case properties. `counterexample.json` and `old-verifier.stdout.gz` retain that source-bound false acceptance. The financial test was restored afterward. This is a verification defect, not evidence of an actual charge or arithmetic defect on main.

The corrected verifier requires one complete61-example, zero-pending Hspec report. The intended run must show zero failures and exactly three1000-case completions. Each arithmetic mutant must reach at least one real assertion failure in the full nonpending suite. Missing, truncated, empty, pending or compiler-only output rejects.

`qualified-native.json.gz` records all61 intended examples and three1000-case properties passing, with all nine arithmetic mutants detected. A separate disposable checkout with the same three1-case limits is now rejected at the intended run; see `fixed-reduced-control.*`. Both control checkouts restore the original financial test.

All eight verifier regressions pass in normal and optimized Python. Removing only the new property-count guard makes six assertions fail in each mode. Logs and receipts are in `harness-controls.tar.gz`. Root quality and catalog audit pass; source admission maps every changed material surface. CI selection now runs the backend when either verifier file changes.

Reproduce on the recorded source/toolchain with `STACK_ROOT=/Users/macpro/.stack python3 scripts/verify-checkout-money.py`, `python3 scripts/test-checkout-money-verifier.py`, `python3 -O scripts/test-checkout-money-verifier.py`, and `npm run quality:repo`. To reproduce reduced coverage, use a separate clean checkout of recorded main or qualification source, replace all three property limits1000 with1, run the verifier, retain its output and restore the test. Never reduce the checked-in financial test.

Assurance is TESTED at this evidence-admission boundary under trusted Hspec/QuickCheck reporting. No new whole-program proof, independently replayed certificate, full database/provider qualification or deployment is claimed. The fresh report retains148 complete requirements and296 implementation/refinement obligations UNPROVEN, plus11 separate conditional SMT claims; release execution is BLOCKED. `qualification.json` lists exact versions and scope. `sha256.json` binds every retained artifact except itself.

Normal integration of protected music-authorization main `4b81ab4ab92416aa31dc816057fb8e6a736596d1` was qualified at clean source `7376617b1ba3ccf40eefbb9976930481a7fe6be2`. Fresh native arithmetic, root quality, catalog audit and source admission pass; all 1,356 report inputs match. The report now retains 149 complete requirements and 298 implementation/refinement obligations UNPROVEN, plus 11 separate conditional SMT claims. See `main626-integration.json` and `main626-artifacts.tar.gz`; earlier receipts keep their original source identities.
