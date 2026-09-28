#!/usr/bin/env python3
"""Run installed native journeys against a private, disposable HTTP fixture.

Requires the matching bundled test app already installed on the selected device.
Android additionally needs `adb reverse tcp:18128 tcp:18128`.
Never accepts a production host or database. Credentials and device logs remain
in the private fixture directory, outside the repository.
"""
import argparse
import json
import os
import pathlib
import secrets
import string
import subprocess
import urllib.parse
import urllib.request
import uuid

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--fixture', required=True, type=pathlib.Path)
parser.add_argument('--mobile-root', required=True, type=pathlib.Path)
parser.add_argument('--device', required=True)
parser.add_argument('--app-id', default='com.tdfrecords.app')
parser.add_argument('--maestro', default='maestro')
parser.add_argument('--resume', action='store_true', help='Verify an already-created nativeBody and continue the notification journey')
args = parser.parse_args()
fixture = json.loads(args.fixture.read_text())
base, database = fixture['base'], fixture['database']
parsed = urllib.parse.urlparse(base)
assert parsed.scheme == 'http' and parsed.hostname in ('localhost', '127.0.0.1')
assert database.startswith('tdf_interaction_') and all(c.isalnum() or c == '_' for c in database)
assert args.app_id in ('com.tdfrecords.app', 'com.tdf.records')
owner, _, respondent = fixture['actors']
assert all(918000001 <= actor <= 918999999 for actor in fixture['actors'])
target = str(uuid.UUID(fixture['targetId']))
private = args.fixture.resolve().parent
assert private != args.mobile_root.resolve() and private != pathlib.Path.cwd().resolve()
private.chmod(0o700)


def sql(statement):
    return subprocess.check_output(['psql', '-X', '-v', 'ON_ERROR_STOP=1', '-d', database, '-Atq'], input=statement, text=True).strip()


def request(actor, path, payload=None):
    headers = {'Content-Type': 'application/json', 'Authorization': 'Bearer ' + fixture['tokens'][str(actor)]}
    req = urllib.request.Request(base + path, headers=headers, data=None if payload is None else json.dumps(payload).encode())
    with urllib.request.urlopen(req, timeout=30) as response:
        return json.load(response)


def flow(name, variables):
    command = [args.maestro, '--device', args.device, 'test']
    for key, value in {'APP_ID': args.app_id, **variables}.items():
        command += ['-e', key + '=' + str(value)]
    command += [str(args.mobile_root / 'e2e/interactions' / name)]
    log_path = private / (args.app_id + '-' + name + '.log')
    with log_path.open('w') as log:
        log_path.chmod(0o600)
        result = subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, env=os.environ)
    if result.returncode:
        raise SystemExit(f'Native flow {name} failed; private evidence: {log_path}')


if not args.resume:
    password = ''.join(secrets.choice(string.ascii_lowercase + string.digits) for _ in range(24))
    username = f'interaction-api-{owner}'
    # All interpolated values are generated alphanumeric test values or validated IDs.
    sql(f"UPDATE user_credential SET password_hash=crypt('{password}',gen_salt('bf')),active=true WHERE party_id={owner};")
    fixture.update(nativeUsername=username, nativePassword=password, nativeBody='Native discussion ' + uuid.uuid4().hex)
    args.fixture.write_text(json.dumps(fixture))
    args.fixture.chmod(0o600)
    request(owner, f'/interactions/targets/{target}/commands', {
        'requestKey': str(uuid.uuid4()), 'command': {'operation': 'reaction.set', 'reactionTypeId': None},
    })
    flow('create.yaml', {'TDF_USERNAME': username, 'TDF_PASSWORD': password, 'TDF_TARGET_ID': target, 'TDF_COMMENT_BODY': fixture['nativeBody']})

identity = f"/interactions/targets/club_post/{fixture['postId']}"
page = request(owner, identity + '/comments?sort=newest&limit=20')
root = next(comment for comment in page['items'] if comment['body'] == fixture['nativeBody'])
assert root['state'] == 'visible' and root['author']['id'] == owner and root['parentId'] is None
summary = request(owner, identity)
assert any(reaction['code'] == 'like' and reaction['count'] > 0 for reaction in summary['reactions'])
reply_body = 'Native reply ' + uuid.uuid4().hex
reply = request(respondent, f'/interactions/targets/{target}/commands', {
    'requestKey': str(uuid.uuid4()), 'command': {'operation': 'comment.create', 'body': reply_body, 'parentId': root['id'], 'mentions': []},
})
sql('SELECT interaction_dispatch_events(20);')
notification = next(row for row in request(owner, '/fans/me/notifications') if row.get('nTargetKey') == reply['id'])
assert notification['nTargetType'] == 'interaction_comment'
fixture.update(nativeRootId=root['id'], nativeReplyId=reply['id'], nativeReplyBody=reply_body, nativeNotificationId=notification['nId'])
args.fixture.write_text(json.dumps(fixture))
args.fixture.chmod(0o600)
flow('notification-edit-delete.yaml', {
    'TDF_NOTIFICATION_ID': notification['nId'], 'TDF_COMMENT_BODY': fixture['nativeBody'], 'TDF_REPLY_BODY': reply_body,
})
context = request(owner, identity + '/comments/' + reply['id'])
assert context['root']['id'] == root['id'] and context['root']['state'] == 'deleted'
assert not context['root']['body'] and context['comment']['body'] == reply_body
assert context['comment']['parentId'] == root['id'] and context['comment']['state'] == 'visible'
print('PASS installed native reaction, comment, reply notification/deep link, edit, parent deletion and retained reply')
