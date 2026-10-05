# Synthetic pre-repair booking regressions

These independently reproduced failures establish why the implementation must change. They are not current-candidate passes. The canonical replacement checks live in `scripts/test-booking-conformance.py`; source and binary identities must be recorded again after the repair is frozen. Initial fixture/infrastructure failures are retained in the local evidence directory and are not treated as regressions. No real-money transaction, provider call or production mutation occurred.
