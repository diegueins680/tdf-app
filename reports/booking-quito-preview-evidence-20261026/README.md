# Published Quito preview

The actual preview source75b2cd18745800b6432e5dd80870886af213319d passed
four Chromium desktop/Pixel7-emulated cases with source/hash attestation before
and after. All business/session API responses were synthetic, external traffic
blocked. No actual reservation or payment was created. This is not canonical
production, human usability or physical-device validation.

The recorded run used the private task-local config and observer, with the
unaltered committed `e2e/web/booking-timezone.spec.mjs` from source75b2cd1.
The public wrappers here adapt paths for replay; they were not the exact private
wrapper files executed. With source75b2cd1 checked out, dependencies installed and
Chromium available, copy this evidence folder into its reports directory and run:

```sh
node reports/booking-quito-preview-evidence-20261026/verify.mjs
```

The wrappers check the immutable preview source and asset SHA, and execute the
same committed browser cases. TDF_QUITO_SOURCE_ROOT and TDF_QUITO_OUTPUT may set
alternate source and output locations. Replaying creates synthetic browser state
only; provider or application APIs are always intercepted. Hosted WebKit CI
remains separate; its result must not be inferred from this Chromium preview.
