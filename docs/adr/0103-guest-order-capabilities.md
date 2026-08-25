# ADR-0103: Scoped guest-order capabilities

Status: Accepted — 2026-08-13

## Decision

Guest checkout returns an opaque random lookup token once. Only a salted hash is stored. Customer
tracking requires the token, is rate-limited, returns a redacted domain-safe projection, and rotates
after account claim or suspected disclosure. Order numbers remain human references, not credentials.

Sequential IDs, short order codes, buyer email alone, and browser-embedded bearer tokens were
rejected because they enable enumeration or privileged API access.
