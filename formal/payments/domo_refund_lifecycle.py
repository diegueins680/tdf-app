#!/usr/bin/env python3
"""Bounded exploration of the Domo refund protocol; no I/O or provider calls.

Two actors (requester/independent reviewer), one cancellation, two execution
contenders. Covers request, approve, claim, accepted/ambiguous/rejected POST,
verified completion, manual evidence and review, and retries. This is a protocol
model, not a proof of Haskell/SQL refinement. Runtime tests cover that boundary.
"""
from dataclasses import dataclass, replace
from collections import deque
import unittest


@dataclass(frozen=True)
class State:
    phase: str = 'requested'
    approved: bool = False
    permits: int = 0
    outcome: str = 'none'
    evidence: bool = False
    verified: bool = False
    refunded: int = 0
    closed: bool = False


def successors(s, amount, unsafe_retry=False, unsafe_close=False):
    out = []
    if s.phase == 'requested':
        out.append(replace(s, phase='approved', approved=True))
    if s.phase == 'approved' or (unsafe_retry and s.phase == 'processing' and s.permits < 2):
        out.append(replace(s, phase='processing', permits=s.permits + 1))
    if s.phase == 'processing' and s.outcome == 'none':
        for outcome in ('accepted', 'pending', 'ambiguous', 'rejected', 'manual'):
            out.append(replace(s, outcome=outcome, closed=unsafe_close))
    if s.phase == 'processing':
        if s.outcome in ('accepted', 'pending'):
            out.append(replace(s, phase='succeeded', verified=True, refunded=amount, closed=True))
        if s.outcome == 'manual' and not s.evidence:
            out.append(replace(s, evidence=True))
        if s.outcome == 'manual' and s.evidence:
            out.append(replace(s, phase='succeeded', verified=True, refunded=amount, closed=True))
    # Denied self approval, duplicate claim, pending readback, rejection, transport
    # error and replays leave the state unchanged. They do not create a permit.
    return out


def violations(s, amount):
    return (s.permits > 1 or (s.permits > 0 and not s.approved)
            or s.refunded not in (0, amount) or s.closed != s.verified
            or (s.refunded > 0) != s.verified)


def explore(amount, **unsafe):
    queue = deque([State()])
    visited = set()
    while queue:
        current = queue.popleft()
        if current in visited:
            continue
        visited.add(current)
        if violations(current, amount):
            return visited, current
        queue.extend(successors(current, amount, **unsafe))
    return visited, None


class Protocol(unittest.TestCase):
    def test_partial_and_full_refund(self):
        for amount in (1, 100352, 200704, 2**63 - 1):
            states, bad = explore(amount)
            self.assertIsNone(bad)
            self.assertTrue(any(s.closed for s in states), 'completion must be reachable')
            self.assertTrue(any(s.outcome == 'rejected' and not s.closed for s in states))
            self.assertTrue(any(s.outcome == 'manual' and s.evidence and not s.closed for s in states))

    def test_detects_repeated_execution_after_ambiguous_outcome(self):
        _, bad = explore(200704, unsafe_retry=True)
        self.assertIsNotNone(bad)
        self.assertGreater(bad.permits, 1)

    def test_detects_closure_at_provider_acceptance(self):
        _, bad = explore(200704, unsafe_close=True)
        self.assertIsNotNone(bad)
        self.assertTrue(bad.closed and not bad.verified)


if __name__ == '__main__':
    unittest.main(verbosity=2)
