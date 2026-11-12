#!/usr/bin/env python3
"""AUTH-WEBHOOK-001: what verifyMetaWebhookSignature admits.

The verifier of tdf-hq/src/TDF/API/Types.hs (verifyMetaWebhookSignature and
parseMetaWebhookSignature) is encoded as a decision over the configured
secret, the X-Hub-Signature-256 header and the raw body. HMAC-SHA256 is an
uninterpreted function of key and body: the proofs say which digest the
verifier demands, not that the digest is unforgeable. Text values are
abstract; the parser's four syntactic guards are booleans and its result is
the lower-cased digest.

Proofs (UNSAT of the negation):
  - admission needs a present, nonblank secret, a present header that passes
    every syntactic guard, and a digest equal to HMAC(secret, exact body);
  - an absent or blank secret answers 503 whatever the header says, so
    missing configuration never disables authentication;
  - with a usable secret the only refusals are 401, and the three outcomes
    (admit, 401, 503) are exhaustive and mutually exclusive;
  - a header admitted for one body is refused for any body whose HMAC under
    the same secret differs, and for any secret whose HMAC of that body
    differs.

Necessity: each guard is dropped in turn (blank-secret check, each of the
four syntactic guards, the digest comparison); each must make a proof fail.

The pinned text is the two functions themselves, not the whole module, so
unrelated edits to the module do not invalidate the translation.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import sys
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
SOURCE = 'tdf-hq/src/TDF/API/Types.hs'
START = 'verifyMetaWebhookSignature :: Maybe Text -> Maybe Text -> BL.ByteString -> Either ServerError ()'
END = 'isAsciiHexDigit :: Char -> Bool'
PINNED_TEXT_SHA256 = '4b2c4b5fad72e3e9ec1f1dc6a5ce557c0e1c1711b04a1ad524d9d9d346dbe479'

ADMIT, R401, R503 = z3.IntVal(200), z3.IntVal(401), z3.IntVal(503)
GUARDS = ('blank-secret', 'trimmed', 'prefix', 'length', 'hex', 'digest')


def source_is_current():
    text = (ROOT / SOURCE).read_text()
    if text.count(START) != 1 or text.count(END) != 1:
        print(f'TDF_SMT_RESULT source {SOURCE} markers FAILED')
        return False
    region = text[text.index(START):text.index(END)]
    digest = hashlib.sha256(region.encode()).hexdigest()
    current = digest == PINNED_TEXT_SHA256
    print(f"TDF_SMT_RESULT source {SOURCE} verifier-sha256={digest} {'ok' if current else 'FAILED'}")
    return current


HMAC = z3.Function('hmac', z3.IntSort(), z3.IntSort(), z3.IntSort())


class Request:
    def __init__(self, tag):
        b = lambda n: z3.Bool(f'{n}_{tag}')
        i = lambda n: z3.Int(f'{n}_{tag}')
        self.secret_present, self.secret_blank = b('secret_present'), b('secret_blank')
        self.secret, self.body = i('secret'), i('body')
        self.header_present = b('header_present')
        self.trimmed, self.prefix, self.length, self.hex = b('trimmed'), b('prefix'), b('length'), b('hex')
        self.digest = i('digest')  # lower-cased digest carried by the header


def outcome(r, dropped=None):
    """verifyMetaWebhookSignature, with one guard optionally removed."""
    keep = lambda name, cond: z3.BoolVal(True) if dropped == name else cond
    usable = z3.And(r.secret_present, keep('blank-secret', z3.Not(r.secret_blank)))
    parsed = z3.And(keep('trimmed', r.trimmed), keep('prefix', r.prefix),
                    keep('length', r.length), keep('hex', r.hex))
    matches = keep('digest', r.digest == HMAC(r.secret, r.body))
    return z3.If(z3.Not(usable), R503,
                 z3.If(z3.And(r.header_present, parsed, matches), ADMIT, R401))


def properties(dropped=None):
    r, s = Request('a'), Request('b')
    out, out_s = outcome(r, dropped), outcome(s, dropped)
    wellformed = z3.And(r.trimmed, r.prefix, r.length, r.hex)
    same_header = z3.And(s.header_present == r.header_present, s.trimmed == r.trimmed, s.prefix == r.prefix,
                         s.length == r.length, s.hex == r.hex, s.digest == r.digest)
    return {
        'admit-needs-secret-header-syntax-and-exact-hmac': z3.Implies(
            out == ADMIT,
            z3.And(r.secret_present, z3.Not(r.secret_blank), r.header_present, wellformed,
                   r.digest == HMAC(r.secret, r.body))),
        'missing-or-blank-secret-is-503-never-open': z3.Implies(
            z3.Or(z3.Not(r.secret_present), r.secret_blank), out == R503),
        'usable-secret-refuses-only-with-401': z3.Implies(
            z3.And(r.secret_present, z3.Not(r.secret_blank)), z3.Or(out == ADMIT, out == R401)),
        'outcomes-exhaustive': z3.Or(out == ADMIT, out == R401, out == R503),
        'admission-is-exactly-the-conjunction': (out == ADMIT) == z3.And(
            r.secret_present, z3.Not(r.secret_blank), r.header_present, wellformed,
            r.digest == HMAC(r.secret, r.body)),
        'header-does-not-transfer-to-another-body': z3.Implies(
            z3.And(out == ADMIT, same_header, s.secret_present == r.secret_present,
                   s.secret_blank == r.secret_blank, s.secret == r.secret,
                   HMAC(s.secret, s.body) != HMAC(r.secret, r.body)),
            out_s == R401),
        'header-does-not-transfer-to-another-secret': z3.Implies(
            z3.And(out == ADMIT, same_header, s.body == r.body, s.secret_present, z3.Not(s.secret_blank),
                   HMAC(s.secret, s.body) != HMAC(r.secret, r.body)),
            out_s == R401),
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
    ok = source_is_current() and ok
    for name, claim in properties().items():
        result = proved(claim)
        ok = ok and result
        print(f"TDF_SMT_RESULT proof {name} {'unsat' if result else 'FAILED'}", flush=True)
    r = Request('w')
    for name, value in (('admit', ADMIT), ('refuse-401', R401), ('unavailable-503', R503)):
        result = satisfiable(outcome(r) == value)
        ok = ok and result
        print(f"TDF_SMT_RESULT witness {name} {'sat' if result else 'FAILED'}", flush=True)
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
