"""Bounded transport recovery for the isolated abuse-budget acceptance probe.

Only the interaction server's exact empty transaction-conflict response is
retryable. A503 is never a successful authorization/rate-limit observation.
The same Request object, actor credentials, payload and idempotency key are reused.
"""
import time
import urllib.error
import urllib.request


def request_after_conflict(request):
    deadline = time.monotonic()+5
    for attempt in range(3):
        remaining = deadline-time.monotonic()
        assert remaining > 0, 'Interaction conflict recovery deadline exhausted'
        try:
            response = urllib.request.urlopen(request, timeout=remaining)
        except urllib.error.HTTPError as error:
            response = error
        with response:
            code, headers = response.code, response.headers
            raw = response.read(8193)
        assert len(raw) <= 8192, 'Unexpected conflict-probe response size'
        body = raw.decode()
        if code != 503:
            return code, body
        assert body == '' and headers.get('Retry-After') == '1' and headers.get('Cache-Control') == 'no-store', \
            'Unexpected503 contract; no recovery admitted'
        assert attempt < 2 and time.monotonic()+1 < deadline, \
            'Interaction transaction conflict did not recover within the bounded probe'
        time.sleep(1)
    raise AssertionError('Unreachable conflict recovery state')
