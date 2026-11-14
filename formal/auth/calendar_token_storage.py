#!/usr/bin/env python3
"""INTEG-GOOGLE-CALENDAR-001: a sealed calendar token is always accepted back.

Google Calendar tokens are stored sealed (TDF.Calendar.TokenCipher): the
prefix "enc:v1:" followed by base64 of nonce (12 bytes), GCM tag (16 bytes)
and ciphertext. The stored value is therefore longer than the token. Three
pinned regions are translated:

  * sealedCalendarTokenLength and calendarTokenStoredLimit
    (Calendar/TokenCipher.hs): the length of a sealed value and the length
    limit to apply to a stored value;
  * sealCalendarTokenForStorage (Server.hs): which token is sealed at all;
  * validateStoredGoogleCalendarOAuthToken (Server.hs): which stored value is
    accepted when a calendar configuration is read.

Tokens are ASCII, so characters and bytes coincide; the character-class checks
are one boolean, since sealing produces only base64 and prefix characters.

Proofs (UNSAT of the negation), for the 4096-character token limit:
  - every token that is sealed for storage yields a stored value that the
    stored-token validator accepts (no row can be written that later fails
    its own read);
  - a sealed value is always longer than its token, and its length is
    monotone in the token length;
  - the stored-value limit admits no sealed value holding more than the
    token limit plus one byte (base64 rounding), and a value that is not
    sealed keeps the plain limit;
  - a token that already looks sealed is never sealed again.

Witness: under the previous rule (the plain limit applied to the sealed
value) a 3039-character token is stored and then refused on every read; under
the current rule there is none.

Necessity: the sealed-value limit, the intake length check and the intake
refusal of a token that looks sealed are removed in turn; each must make a
proof fail.

Not covered: AES-GCM itself, the nonce source, the key derivation, and that
every write of the two token columns goes through sealCalendarTokenForStorage
(read from the three write sites in Server.hs). The sealed value carries no
associated data, so it is not bound to its row or column.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import sys
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
CIPHER = 'tdf-hq/src/TDF/Calendar/TokenCipher.hs'
SERVER = 'tdf-hq/src/TDF/Server.hs'
REGIONS = (
    (CIPHER, 'sealedCalendarTokenLength :: Int -> Int', '-- | Deterministic sealing for a caller-supplied 12-byte nonce.',
     'efef6e52b0aa724aeb8e32c04d4986c9647fbf24826d834162a6d8a2dbd74786'),
    (SERVER, 'sealCalendarTokenForStorage :: Text -> AppM Text', 'openStoredCalendarToken :: Text -> AppM Text',
     '29a2d1d65ac706e2761f6cce674b53c55d90922f7bcfa566f4a649c3fb591089'),
    (SERVER, 'validateStoredGoogleCalendarOAuthToken :: Text -> Text -> Either ServerError Text',
     'validateCalendarEventStatusQuery :: Maybe Text -> Either ServerError (Maybe Text)', '1afdada05c994e9b5b059b8e04b1e4c89b2e4d7e4aa36d6dabe9537667668214'),
)

PLAIN_LIMIT = 4096   # maxDriveOAuthTokenChars
PREFIX = 7           # T.length "enc:v1:"
GUARDS = ('sealed-value-limit', 'intake-length-check', 'intake-refuses-sealed-looking-token')


def sources_are_current():
    ok = True
    for source, start, end, pin in REGIONS:
        text = (ROOT / source).read_text()
        if text.count(start) != 1 or text.count(end) != 1:
            print(f'TDF_SMT_RESULT source {source} markers FAILED')
            ok = False
            continue
        digest = hashlib.sha256(text[text.index(start):text.index(end)].encode()).hexdigest()
        current = digest == pin
        ok = ok and current
        print(f"TDF_SMT_RESULT source {source} region-sha256={digest} {'ok' if current else 'FAILED'}")
    return ok


def sealed_length(plain_bytes):
    """sealedCalendarTokenLength."""
    return PREFIX + 4 * ((12 + 16 + plain_bytes + 2) / 3)


def stored_limit(is_sealed, dropped=None):
    """calendarTokenStoredLimit maxDriveOAuthTokenChars."""
    if dropped == 'sealed-value-limit':
        return z3.IntVal(PLAIN_LIMIT)
    return z3.If(is_sealed, sealed_length(z3.IntVal(PLAIN_LIMIT)), z3.IntVal(PLAIN_LIMIT))


def stored_accepts(length, is_sealed, clean, dropped=None):
    """validateStoredGoogleCalendarOAuthToken on a stored value."""
    return z3.And(length >= 1, length <= stored_limit(is_sealed, dropped), clean)


def sealed_for_storage(length, looks_sealed, clean, dropped=None):
    """sealCalendarTokenForStorage: the token is sealed (not answered 502)."""
    within = z3.BoolVal(True) if dropped == 'intake-length-check' else \
        stored_accepts(length, looks_sealed, clean, dropped)
    fresh = z3.BoolVal(True) if dropped == 'intake-refuses-sealed-looking-token' else z3.Not(looks_sealed)
    return z3.And(fresh, within, length >= 1, clean)


def properties(dropped=None):
    n, m = z3.Int('n'), z3.Int('m')
    looks_sealed, clean = z3.Bool('looks_sealed'), z3.Bool('clean')
    stored = sealed_length(n)
    sealed_ok = sealed_for_storage(n, looks_sealed, clean, dropped)
    return {
        'every-sealed-token-is-accepted-back': z3.Implies(
            sealed_ok, stored_accepts(stored, z3.BoolVal(True), z3.BoolVal(True), dropped)),
        'only-a-token-within-the-plain-limit-is-sealed': z3.Implies(
            sealed_ok, z3.And(n >= 1, n <= PLAIN_LIMIT, z3.Not(looks_sealed), clean)),
        'a-sealed-value-is-longer-than-its-token': z3.Implies(n >= 0, stored > n),
        'sealed-length-is-monotone': z3.Implies(z3.And(n >= 0, n <= m), sealed_length(n) <= sealed_length(m)),
        'stored-limit-admits-no-oversized-sealed-token': z3.Implies(
            z3.And(n >= 0, stored <= stored_limit(z3.BoolVal(True), dropped)), n <= PLAIN_LIMIT + 1),
        'an-unsealed-value-keeps-the-plain-limit': stored_limit(z3.BoolVal(False), dropped) == PLAIN_LIMIT,
    }


def proved(claim):
    solver = z3.Solver()
    solver.set('timeout', 60000)
    solver.add(z3.Not(claim))
    return solver.check() == z3.unsat


def satisfiable(*facts):
    solver = z3.Solver()
    solver.set('timeout', 60000)
    solver.add(*facts)
    return solver.check() == z3.sat


def main():
    ok = z3.get_version_string().startswith(PINNED)
    print(f"TDF_SMT_RESULT solver z3 {z3.get_version_string()} {'ok' if ok else 'FAILED'}", flush=True)
    ok = sources_are_current() and ok
    for name, claim in properties().items():
        result = proved(claim)
        ok = ok and result
        print(f"TDF_SMT_RESULT proof {name} {'unsat' if result else 'FAILED'}", flush=True)
    n = z3.Int('n_w')
    stranded = lambda rule: z3.And(n >= 1, n <= PLAIN_LIMIT, sealed_length(n) > stored_limit(z3.BoolVal(True), rule))
    checks = {
        'previous-rule-strands-a-3039-character-token': satisfiable(stranded('sealed-value-limit'), n == 3039),
        'previous-rule-keeps-a-3038-character-token': not satisfiable(stranded('sealed-value-limit'), n <= 3038),
        'current-rule-strands-no-token': not satisfiable(stranded(None)),
        'a-full-length-token-is-sealed-and-accepted': satisfiable(
            n == PLAIN_LIMIT, sealed_for_storage(n, z3.BoolVal(False), z3.BoolVal(True)),
            sealed_length(n) == 5507),
    }
    for name, result in checks.items():
        ok = ok and result
        print(f"TDF_SMT_RESULT witness {name} {'ok' if result else 'FAILED'}", flush=True)
    for guard in GUARDS:
        broken = [name for name, claim in properties(guard).items() if not proved(claim)]
        result = bool(broken)
        ok = ok and result
        print(f"TDF_SMT_RESULT necessity drop-{guard} "
              f"{'breaks ' + ','.join(broken) if result else 'FAILED (guard is not load-bearing)'}", flush=True)
    print(f"TDF_SMT_RESULT summary {'ok' if ok else 'FAILED'}", flush=True)
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
