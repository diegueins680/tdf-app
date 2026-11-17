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


def post(path, body):
    request = urllib.request.Request(BASE + path, data=json.dumps(body).encode(),
                                     headers={'Content-Type': 'application/json'}, method='POST')
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


def login(email, password):
    return post('/login', {'username': email, 'password': password})


TERMS = 'tdf-account-terms-v1'  # ServerAuth.supportedAccountTermsVersion

# The counters use fixed windows; start well inside one so no assertion spans
# the 15-minute login boundary (which is also an hourly boundary).
import time  # noqa: E402
remaining = 900 - int(time.time()) % 900
if remaining < 90:
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

# Lockout: ten failures in the window, then nothing is checked, the right password included.
victim = 'victim@throttle.test'
assert signup(victim)[0] == 200
assert [login(victim, WRONG)[0] for _ in range(10)] == [401] * 10
locked = login(victim, PASSWORD)
assert locked[0] == 429, locked
assert int(locked[1].get('Retry-After', '0')) >= 1, locked[1]
assert login(' Victim@Throttle.test ', PASSWORD)[0] in (400, 429)
assert sql("SELECT request_count FROM auth_rate_limit WHERE scope='login-failure' ORDER BY request_count DESC LIMIT 1") == '10'
# Another identifier is unaffected.
assert login(owner, PASSWORD)[0] == 200

# Concurrency: forty simultaneous wrong passwords on one identifier. The count
# check, the password check and the count are serialized per identifier, so at
# most ten are checked and the rest are refused.
from concurrent.futures import ThreadPoolExecutor  # noqa: E402
racer = 'racer@throttle.test'
assert signup(racer)[0] == 200
with ThreadPoolExecutor(max_workers=20) as pool:
    raced = list(pool.map(lambda _: login(racer, WRONG)[0], range(40)))
assert set(raced) <= {401, 429}, raced
assert raced.count(401) == 10, raced
assert login(racer, PASSWORD)[0] == 429

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
