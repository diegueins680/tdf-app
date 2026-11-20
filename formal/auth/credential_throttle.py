#!/usr/bin/env python3
"""AUTH-CREDENTIAL-INTAKE-001 / AUTH-PASSWORD-RECOVERY-001: the credential throttle.

Two pinned regions are translated:

  * tdf-hq/src/TDF/Auth/Throttle.hs, the pure policy: limits, window lengths,
    authThrottleWindowStart, authThrottleRetryAfterSeconds,
    authThrottleLockedOut and authThrottleOverLimit;
    and the three login counters: loginCounterLimit and
    loginCountersLockedOut;
  * tdf-hq/src/TDF/ServerAuth.hs, the login handler's transaction: lock the
    three counters (identifier at the client address, client address,
    identifier), read their failure counts, refuse when any is used up,
    otherwise check the password and count a failure on all three.

Time is a real number of POSIX seconds; the window start is
floor(t) - floor(t) mod W, as in the source. One attempt's counters in one
window are a sequence of steps over the stored counters: the advisory locks
make attempts that share a counter run one after another, which the model
assumes and does not prove.

Proofs (UNSAT of the negation), for every scope:
  - the window start is a multiple of the window, start <= t < start + W, and
    two instants share a start exactly when they share floor(floor(t) / W);
  - Retry-After is between 1 and W whole seconds and reaches the next window:
    start + W <= t + retryAfter;
  - login: each of the three counters never exceeds its limit (10, 50, 100;
    inductive invariant), a locked-out attempt checks no password and changes
    nothing, an attempt with every counter below its limit is checked, and
    only a failed check moves the counters, all three by one;
  - login: the owner, at an address with no failures of its own, is refused
    only once the identifier counter reached 100, and that takes failures
    from at least ten addresses (each pair counter stays at most 10 and the
    identifier counter is their sum);
  - signup and reset: a request is admitted exactly when fewer than the limit
    were counted before it, and the admitted requests of a window never
    exceed the limit (inductive invariant).

Witnesses: a tenth failed check, a lockout of the pair, a lockout of the
address, a lockout of the identifier from a fresh address with ten
addresses, a fifth admitted signup and a refused sixth are reachable.

Necessity: the lockout check, the failure count, the window alignment, the
strict comparison of authThrottleOverLimit, and each of the three counters in
the lockout check are each removed in turn; each must make a proof fail. The `max 1` of authThrottleRetryAfterSeconds is reported as
not load-bearing: the ceiling is already at least one.

Not covered: that the advisory lock serializes (PostgreSQL behaviour), the
subject hash, the SQL upsert, a timestamp taken before the lock is granted
(the attempt is counted in the window of its own timestamp), and how the
client address is derived (TDF.Auth.ClientAddress, unit- and HTTP-tested:
the last X-Forwarded-For entry behind a private peer, else the peer).

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import sys
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
REGIONS = (
    ('tdf-hq/src/TDF/Auth/Throttle.hs',
     'authThrottleLimit :: AuthThrottleScope -> Int',
     '-- | Create the counter table when the backend migrates its own schema',
     'PIN_POLICY'),
    ('tdf-hq/src/TDF/ServerAuth.hs',
     'login :: Maybe Text -> SockAddr -> LoginRequest -> AppM (Api.SessionCookieHeaders LoginResponse)',
     '-- | 429 with the seconds left in the current window.',
     'PIN_LOGIN'),
)
PINS = {
    'PIN_POLICY': 'e0372e05c5867eb572ecf814fd4927dcd7a7aa2ec0e7b79dbd7f638872a6b85e',
    'PIN_LOGIN': '804a0fb26c6520c58fab8084bc31b7c3e5c7df9c412d7aa1c8241939703cd140',
}

# authThrottleLimit / authThrottleWindowSeconds (the login limit is the pair's)
SCOPES = {'login-failure': (10, 15 * 60), 'signup': (5, 60 * 60), 'password-reset': (5, 60 * 60)}
# loginCounterLimit, in the order of LoginCounter
LOGIN_COUNTERS = (('pair', 10), ('address', 50), ('identifier', 100))
GUARDS = ('lockout-check', 'failure-count', 'window-alignment', 'strict-over-limit',
          'pair-in-lockout', 'address-in-lockout', 'identifier-in-lockout')


def sources_are_current():
    ok = True
    for source, start, end, pin in REGIONS:
        text = (ROOT / source).read_text()
        if text.count(start) != 1 or text.count(end) != 1:
            print(f'TDF_SMT_RESULT source {source} markers FAILED')
            ok = False
            continue
        digest = hashlib.sha256(text[text.index(start):text.index(end)].encode()).hexdigest()
        current = digest == PINS[pin]
        ok = ok and current
        print(f"TDF_SMT_RESULT source {source} region-sha256={digest} {'ok' if current else 'FAILED'}")
    return ok


def floor(t):
    return z3.ToInt(t)


def ceiling(x):
    return -z3.ToInt(-x)


def window_start(t, window, dropped=None):
    seconds = floor(t)
    return seconds if dropped == 'window-alignment' else seconds - seconds % window


def retry_after(t, window, dropped=None, keep_max=True):
    remaining = ceiling(z3.ToReal(window_start(t, window, dropped) + window) - t)
    return z3.If(remaining >= 1, remaining, z3.IntVal(1)) if keep_max else remaining


def locked_out(limit, recorded):
    return recorded >= limit


def over_limit(limit, consumed, dropped=None):
    return consumed >= limit if dropped == 'strict-over-limit' else consumed > limit


def login_step(counts, invalid, dropped=None):
    """One serialized login attempt over the three counters:
    (refused, password checked, next counts)."""
    used_up = [locked_out(limit, count) for (name, limit), count in zip(LOGIN_COUNTERS, counts)
               if dropped != f'{name}-in-lockout']
    refused = z3.BoolVal(False) if dropped == 'lockout-check' else z3.Or(used_up)
    checked = z3.Not(refused)
    counted = z3.And(checked, invalid)
    bumped = [count if dropped == 'failure-count' else count + 1 for count in counts]
    return refused, checked, [z3.If(counted, b, count) for b, count in zip(bumped, counts)]


def consume_step(limit, count, dropped=None):
    """Signup / reset: count first, then decide. (admitted, next count)."""
    consumed = count + 1
    return z3.Not(over_limit(limit, consumed, dropped)), consumed


def properties(scope, dropped=None):
    limit, window = SCOPES[scope]
    t, u = z3.Real('t'), z3.Real('u')
    start, start_u = window_start(t, window, dropped), window_start(u, window, dropped)
    retry = retry_after(t, window, dropped)
    claims = {
        'window-start-aligned-and-contains-now': z3.And(
            start % window == 0, z3.ToReal(start) <= t, t < z3.ToReal(start + window)),
        'same-window-iff-same-quotient': (start == start_u) == (floor(t) / window == floor(u) / window),
        'retry-after-between-one-and-window': z3.And(retry >= 1, retry <= window),
        'retry-after-reaches-the-next-window': z3.ToReal(start + window) <= t + z3.ToReal(retry),
    }
    count, failed, admitted = z3.Int('count'), z3.Int('failed'), z3.Int('admitted')
    invalid = z3.Bool('invalid')
    if scope == 'login-failure':
        counts = [z3.Int(f'count_{name}') for name, _ in LOGIN_COUNTERS]
        refused, checked, after = login_step(counts, invalid, dropped)
        inv = lambda cs: z3.And([z3.And(c >= 0, c <= lim) for c, (_, lim) in zip(cs, LOGIN_COUNTERS)])
        unchanged = z3.And([a == c for a, c in zip(after, counts)])
        all_bumped = z3.And([a == c + 1 for a, c in zip(after, counts)])
        any_used_up = z3.Or([c >= lim for c, (_, lim) in zip(counts, LOGIN_COUNTERS)])
        # The owner's case: identifier counter = sum of its pair counters over
        # `addresses` addresses, each pair counter within the invariant.
        addresses, identifier_total = z3.Int('addresses'), counts[2]
        fresh = z3.And(counts[0] == 0, counts[1] == 0)
        claims.update({
            'login-invariant-holds-initially': inv([z3.IntVal(0)] * 3),
            'login-invariant-is-inductive': z3.Implies(inv(counts), inv(after)),
            'locked-out-attempt-checks-nothing-and-changes-nothing': z3.Implies(
                z3.And(inv(counts), any_used_up), z3.And(refused, z3.Not(checked), unchanged)),
            'below-every-limit-the-password-is-checked': z3.Implies(
                z3.And(inv(counts), z3.Not(any_used_up)), z3.And(checked, z3.Not(refused))),
            'only-a-failed-check-moves-the-counters-all-by-one': z3.Implies(
                inv(counts), z3.If(z3.And(checked, invalid), all_bumped, unchanged)),
            'owner-at-a-fresh-address-is-refused-only-by-the-identifier-counter': z3.Implies(
                z3.And(inv(counts), fresh, refused), identifier_total >= LOGIN_COUNTERS[2][1]),
            'the-identifier-lockout-needs-ten-addresses': z3.Implies(
                z3.And(addresses >= 0, identifier_total <= LOGIN_COUNTERS[0][1] * addresses,
                       identifier_total >= LOGIN_COUNTERS[2][1]),
                addresses >= LOGIN_COUNTERS[2][1] // LOGIN_COUNTERS[0][1]),
        })
    else:
        ok, after = consume_step(limit, count, dropped)
        admitted_after = z3.If(ok, admitted + 1, admitted)
        inv = lambda c, a: z3.And(c >= 0, a == z3.If(c <= limit, c, limit))
        claims.update({
            'consume-invariant-holds-initially': inv(z3.IntVal(0), z3.IntVal(0)),
            'consume-invariant-is-inductive': z3.Implies(inv(count, admitted), inv(after, admitted_after)),
            'admitted-exactly-below-the-limit': z3.Implies(count >= 0, ok == (count < limit)),
            'every-request-is-counted': after == count + 1,
        })
    return claims


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


def witnesses():
    count, invalid = z3.Int('count_w'), z3.Bool('invalid_w')
    pair, address, identifier = counts = [z3.Int(f'w_{name}') for name, _ in LOGIN_COUNTERS]
    refused, checked, after = login_step(counts, invalid)
    signup_limit, _ = SCOPES['signup']
    ok, _ = consume_step(signup_limit, count)
    low = z3.And(address == 0, identifier == 9)
    return {
        'tenth-failed-check-is-counted': (pair == 9, low, checked, invalid, after[0] == 10),
        'eleventh-attempt-of-the-pair-is-refused': (pair == 10, address == 10, identifier == 10, refused),
        'address-used-up-across-identifiers-is-refused': (pair == 0, address == 50, identifier == 0, refused),
        'identifier-used-up-from-ten-addresses-is-refused-at-a-fresh-one': (
            pair == 0, address == 0, identifier == 100, refused),
        'fifth-signup-is-admitted': (count == signup_limit - 1, ok),
        'sixth-signup-is-refused': (count == signup_limit, z3.Not(ok)),
    }


def main():
    ok = z3.get_version_string().startswith(PINNED)
    print(f"TDF_SMT_RESULT solver z3 {z3.get_version_string()} {'ok' if ok else 'FAILED'}", flush=True)
    ok = sources_are_current() and ok
    for scope in SCOPES:
        for name, claim in properties(scope).items():
            result = proved(claim)
            ok = ok and result
            print(f"TDF_SMT_RESULT proof {scope} {name} {'unsat' if result else 'FAILED'}", flush=True)
    for name, facts in witnesses().items():
        result = satisfiable(*facts)
        ok = ok and result
        print(f"TDF_SMT_RESULT witness {name} {'sat' if result else 'FAILED'}", flush=True)
    for guard in GUARDS:
        broken = [f'{scope}:{name}' for scope in SCOPES
                  for name, claim in properties(scope, guard).items() if not proved(claim)]
        result = bool(broken)
        ok = ok and result
        print(f"TDF_SMT_RESULT necessity drop-{guard} "
              f"{'breaks ' + ','.join(broken) if result else 'FAILED (guard is not load-bearing)'}", flush=True)
    t = z3.Real('t')
    redundant = all(proved(retry_after(t, window, keep_max=False) >= 1) for _, window in SCOPES.values())
    ok = ok and redundant
    print(f"TDF_SMT_RESULT note retry-after-max-1 {'redundant' if redundant else 'FAILED (load-bearing)'}", flush=True)
    print(f"TDF_SMT_RESULT summary {'ok' if ok else 'FAILED'}", flush=True)
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
