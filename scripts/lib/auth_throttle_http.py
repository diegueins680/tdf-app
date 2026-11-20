"""HTTP assertions for scripts/test-auth-throttle-http.sh."""
import json
import os
import secrets
import subprocess
import urllib.error
import urllib.request

BASE = os.environ['TDF_AUTH_THROTTLE_BASE']
DATABASE = os.environ['TDF_AUTH_THROTTLE_DATABASE']
PASSWORD = secrets.token_urlsafe(24)
WRONG = secrets.token_urlsafe(24)


def post(path, body, address=None):
    headers = {'Content-Type': 'application/json'}
    if address:
        # The test reaches the backend over loopback, the position of the
        # reverse proxy, so the last forwarded entry is taken as the client.
        headers['X-Forwarded-For'] = '203.0.113.250, ' + address
    request = urllib.request.Request(BASE + path, data=json.dumps(body).encode(),
                                     headers=headers, method='POST')
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    try:
        with opener.open(request, timeout=30) as response:
            return response.status, dict(response.headers), response.read().decode('utf-8', 'replace')
    except urllib.error.HTTPError as error:
        return error.code, dict(error.headers), error.read().decode('utf-8', 'replace')


def sql(statement):
    return subprocess.run(['psql', '-X', '-v', 'ON_ERROR_STOP=1', '-d', DATABASE, '-Atqc', statement],
                          check=True, capture_output=True, text=True).stdout.strip()


def signup(email):
    return post('/signup', {'firstName': 'Synthetic', 'lastName': 'Throttle', 'email': email,
                            'password': PASSWORD, 'termsAccepted': True, 'termsVersion': TERMS})


def login(email, password, address=None):
    return post('/login', {'username': email, 'password': password}, address)


def counts(scope='login-failure'):
    return sorted(int(v) for v in sql(f"SELECT request_count FROM auth_rate_limit WHERE scope='{scope}'").split())


TERMS = 'tdf-account-terms-v1'  # ServerAuth.supportedAccountTermsVersion

# The counters use fixed windows; start well inside one so no assertion spans
# the 15-minute login boundary (which is also an hourly boundary).
import time  # noqa: E402
remaining = 900 - int(time.time()) % 900
if remaining < 300:  # about 300 logins follow
    time.sleep(remaining + 2)

# Signup limit: five requests per email per hour, the sixth is refused before anything is created.
owner = 'owner@throttle.test'
statuses = [signup(owner)[0] for _ in range(6)]
assert statuses[0] == 200, statuses
assert statuses[1:5] == [409] * 4, statuses
assert statuses[5] == 429, statuses
assert sql("SELECT request_count FROM auth_rate_limit WHERE scope='signup'").splitlines().count('6') == 1

# Login: a right password works, a wrong one and an unknown account answer alike.
assert login(owner, PASSWORD)[0] == 200
wrong = login(owner, WRONG)
unknown = login('nobody@throttle.test', WRONG)
assert (wrong[0], wrong[2]) == (401, 'Invalid username or password'), wrong
assert (unknown[0], unknown[2]) == (wrong[0], wrong[2]), unknown

# A disabled account answers exactly like a wrong password, with the right password too.
disabled = 'disabled@throttle.test'
assert signup(disabled)[0] == 200
sql("UPDATE user_credential SET active = FALSE WHERE username = 'disabled@throttle.test'")
refused = login(disabled, PASSWORD)
assert (refused[0], refused[2]) == (wrong[0], wrong[2]), refused

# Lockout: ten failures for one identifier from one address, then nothing is
# checked from that address, the right password included.
victim = 'victim@throttle.test'
attacker = '198.51.100.10'
assert signup(victim)[0] == 200
assert [login(victim, WRONG, attacker)[0] for _ in range(10)] == [401] * 10
locked = login(victim, PASSWORD, attacker)
assert locked[0] == 429, locked
assert int(locked[1].get('Retry-After', '0')) >= 1, locked[1]
assert login(' Victim@Throttle.test ', PASSWORD, attacker)[0] in (400, 429)
# The owner, at another address, still signs in: the attacker locked only
# their own address out of this account.
assert login(victim, PASSWORD, '198.51.100.11')[0] == 200
# Another identifier from the attacker's address is unaffected.
assert login(owner, PASSWORD, attacker)[0] == 200

# One address across identifiers: fifty failures, then that address is
# refused for every identifier, while another address is not.
sprayer = '198.51.100.20'
sprayed = [login(f'spray{n}@throttle.test', WRONG, sprayer)[0] for n in range(50)]
assert sprayed == [401] * 50, sprayed
assert login(owner, PASSWORD, sprayer)[0] == 429
assert login(owner, PASSWORD, '198.51.100.21')[0] == 200

# Many addresses on one identifier: a hundred failures from ten addresses,
# then the account is refused from every address. This is the remaining
# lockout, and it needs at least ten addresses.
target = 'target@throttle.test'
assert signup(target)[0] == 200
for n in range(10):
    assert [login(target, WRONG, f'198.51.100.{100 + n}')[0] for _ in range(10)] == [401] * 10
assert login(target, PASSWORD, '198.51.100.200')[0] == 429
assert max(counts()) == 100, counts()

# Only the last forwarded entry counts (the peer here is loopback, the
# proxy's position): two requests that differ only in an earlier entry share
# all three counters, so three counters reach two.
spoof = 'spoof@throttle.test'
twos = counts().count(2)
for earlier in ('192.0.2.1', '192.0.2.2'):
    request = urllib.request.Request(
        BASE + '/login', data=json.dumps({'username': spoof, 'password': WRONG}).encode(),
        headers={'Content-Type': 'application/json', 'X-Forwarded-For': earlier + ', 198.51.100.30'},
        method='POST')
    try:
        urllib.request.build_opener(urllib.request.ProxyHandler({})).open(request, timeout=30)
        raise AssertionError('wrong password accepted')
    except urllib.error.HTTPError as error:
        assert error.code == 401, error.code
assert counts().count(2) == twos + 3, counts()

# Concurrency: forty simultaneous wrong passwords on one identifier. The count
# check, the password check and the count are serialized per identifier, so at
# most ten are checked and the rest are refused.
from concurrent.futures import ThreadPoolExecutor  # noqa: E402
racer = 'racer@throttle.test'
assert signup(racer)[0] == 200
with ThreadPoolExecutor(max_workers=20) as pool:
    raced = list(pool.map(lambda _: login(racer, WRONG, '198.51.100.40')[0], range(40)))
assert set(raced) <= {401, 429}, raced
assert raced.count(401) == 10, raced
assert login(racer, PASSWORD, '198.51.100.40')[0] == 429

# Reset requests: always the same answer; past five nothing more is issued.
for _ in range(7):
    status, _, _ = post('/v1/password-reset', {'email': owner})
    assert status in (200, 204), status
issued = int(sql("SELECT count(*) FROM auth_recovery_challenge"))
assert 1 <= issued <= 5, issued
assert sql("SELECT request_count FROM auth_rate_limit WHERE scope='password-reset'") == '7'

print(json.dumps({'signupStatuses': statuses, 'lockedStatus': locked[0], 'concurrentChecked': raced.count(401),
                  'retryAfterSeconds': int(locked[1]['Retry-After']), 'resetChallengesIssued': issued}))
print('Auth throttle HTTP checks passed')
