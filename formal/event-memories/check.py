#!/usr/bin/env python3
"""Bounded campaign policy exploration, not a proof of SQL/HTTP implementation."""
from collections import deque
from dataclasses import dataclass, replace

@dataclass(frozen=True)
class State:
    enabled: bool = False
    consent: bool = False
    finished: bool = False
    eligible: bool = True
    published: bool = False
    messages: int = 0
    age: int = 7

def successors(s, defect=None):
    for field in ('enabled', 'consent', 'finished', 'eligible'):
        yield 'toggle_' + field, replace(s, **{field: not getattr(s, field)})
    yield 'publish', replace(s, published=True)
    yield 'wait', replace(s, age=7)
    ready = s.enabled and (s.consent or defect == 'ignore_consent') and s.finished and s.eligible
    ready = ready and (not s.published or defect == 'ignore_publication') and s.age >= 7
    if ready and (s.messages < 2 or defect == 'third_message'):
        yield 'deliver', replace(s, messages=s.messages+1, age=0)

def check(defect=None):
    seen = {State()}
    queue = deque([(State(), [])])
    while queue:
        s, path = queue.popleft()
        for action, nxt in successors(s, defect):
            if action == 'deliver':
                assert s.enabled and s.consent and s.finished and s.eligible, path + [action]
                assert not s.published and s.age >= 7, path + [action]
                assert nxt.messages <= 2, path + [action]
            if nxt not in seen:
                seen.add(nxt)
                queue.append((nxt, path + [action]))
    return len(seen)

if __name__ == '__main__':
    count = check()
    for defect in ('ignore_consent', 'ignore_publication', 'third_message'):
        try:
            check(defect)
        except AssertionError:
            continue
        raise SystemExit('Negative control escaped: ' + defect)
    print(f'{count} bounded states; 3 negative controls detected; SQL refinement remains unproven')
