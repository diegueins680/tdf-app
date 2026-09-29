#!/usr/bin/env python3
"""Execute interleavings against real PostgreSQL functions, never a production DB."""
import concurrent.futures
import json
import subprocess
import sys
import time
import uuid

DB = sys.argv[1]
if not DB.startswith('tdf_interaction_'):
    raise SystemExit('Refusing non-test database')

def sql(statement, retries=4):
    for attempt in range(retries):
        result = subprocess.run(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', DB], input=statement,
                                text=True, capture_output=True)
        if result.returncode == 0:
            return result.stdout.strip()
        if ('deadlock detected' in result.stderr or 'could not serialize' in result.stderr) and attempt + 1 < retries:
            time.sleep(.02 * (attempt + 1))
            continue
        raise AssertionError(result.stderr)
    raise AssertionError('Retry budget exhausted')

def literal(value):
    return "'" + str(value).replace("'", "''") + "'"

def command(actor, target, payload, key=None):
    key = key or str(uuid.uuid4())
    result = json.loads(sql(f'SELECT interaction_command({actor},{literal(target)},{literal(key)},{literal(json.dumps(payload))});'))
    assert 'error' not in result, result
    return result

sql('''
INSERT INTO party(id,display_name,is_org,created_at)
SELECT n,'Concurrency synthetic '||n,false,now() FROM generate_series(930000001,930000004) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
SELECT n,'interaction-concurrency-'||n,'not-a-login-hash',true FROM generate_series(930000001,930000004) n;
INSERT INTO fan_club(id,artist_party_id,name) VALUES(930000001,930000001,'Concurrency synthetic');
INSERT INTO fan_club_post(id,club_id,fan_party_id,content,created_at)
VALUES(930000001,930000001,930000001,'Concurrency post',now());
INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at)
SELECT n,930000001,now() FROM generate_series(930000002,930000004) n;
UPDATE interaction_runtime SET enabled=true;
''')
target = sql("SELECT interaction_register('club_post','930000001',930000001);")
choices = [None, '50900000-0000-4000-8000-000000000001', '50900000-0000-4000-8000-000000000002']

def reactions(actor):
    for sequence in range(20):
        command(actor, target, {'operation': 'reaction.set', 'reactionTypeId': choices[(actor + sequence) % len(choices)]})

with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
    list(pool.map(reactions, range(930000001, 930000005)))
assert sql('''SELECT NOT EXISTS(
 SELECT 1 FROM interaction_reaction_total t FULL JOIN
 (SELECT target_id,reaction_type_id,count(*) n FROM interaction_reaction GROUP BY target_id,reaction_type_id) r USING(target_id,reaction_type_id)
 WHERE coalesce(t.total,0)<>coalesce(r.n,0));''') == 't'
print('PASS concurrent reaction changes/removals: counters equal authoritative rows')
sql("UPDATE interaction_request SET created_at=now()-interval '2 minutes';")
key = str(uuid.uuid4())
with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
    results = list(pool.map(lambda _: command(930000002, target, {'operation': 'comment.create', 'body': 'One logical comment'}, key), range(5)))
assert len({result['id'] for result in results}) == 1
assert sql('SELECT count(*) FROM interaction_comment;') == '1'
print('PASS simultaneous duplicate comment requests: one identity and one record')
root = results[0]['id']
with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
    list(pool.map(lambda actor: [command(actor, target, {'operation': 'comment.create', 'body': 'Reply', 'parentId': root}) for _ in range(8)], range(930000001,930000005)))
assert sql(f"SELECT sum(comments)=33 FROM interaction_target_comment_total WHERE target_id={literal(target)};") == 't'
command(930000002, target, {'operation': 'comment.delete', 'commentId': root, 'expectedVersion': 1})
assert sql(f"SELECT sum(comments)=32 FROM interaction_target_comment_total WHERE target_id={literal(target)};") == 't'
assert sql(f"SELECT count(*)=32 FROM interaction_comment WHERE root_id={literal(root)} AND state='visible';") == 't'
print('PASS concurrent replies and parent deletion: counts and structure preserved')

# Hold the same account/source locks as a protected command. Block must wait;
# once it commits, a later write must fail. Marker is read from psql stdout so the
# ordering is observed rather than assumed from a sleep.
writer = subprocess.Popen(['psql','-X','-qAt','-v','ON_ERROR_STOP=1','-d',DB], stdin=subprocess.PIPE,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE,text=True)
writer.stdin.write(f'''BEGIN;
SELECT id FROM party WHERE id IN (930000001,930000003) ORDER BY id FOR SHARE;
SELECT interaction_lock_source({literal(target)});
SELECT 'writer-locked';
'''); writer.stdin.flush()
while writer.stdout.readline().strip() != 'writer-locked':
    assert writer.poll() is None, 'Writer failed before lock marker'
with concurrent.futures.ThreadPoolExecutor(max_workers=1) as pool:
    blocker = pool.submit(sql, f"SELECT interaction_block(930000001,930000003,true,0,'{uuid.uuid4()}');")
    time.sleep(.2)
    assert not blocker.done(), 'Block bypassed held account lock'
    writer.stdin.write('COMMIT;\n\\q\n'); writer.stdin.flush(); writer.wait(timeout=10)
    result = json.loads(blocker.result(timeout=10)); assert result['blocked'] is True
result = json.loads(sql(f"SELECT interaction_command(930000003,{literal(target)},'{uuid.uuid4()}','{{\"operation\":\"comment.create\",\"body\":\"Blocked\"}}');"))
assert result['error'] == 'unavailable', result
print('PASS block/write serialization and immediate permission revocation')

# Reply aliases have their own reaction slots, but share deletion's parent lock.
# Either serialized order must end with no active reaction or count on the erased reply.
for sequence in range(6):
    payload = {'operation': 'legacy.comment', 'body': f'Alias race {sequence}', 'artistId': 930000001}
    created = json.loads(sql(f"SELECT interaction_legacy_command(930000002,'club_post','930000001',{literal(json.dumps(payload))});"))
    assert 'error' not in created, created
    alias = str(created['fcpId'])
    alias_target = sql(f"SELECT interaction_register('club_post',{literal(alias)},930000004);")
    comment_id = sql(f"SELECT comment_id FROM interaction_legacy_mapping WHERE legacy_kind='club_reply' AND legacy_id={literal(alias)};")
    reaction = {'operation': 'legacy.reaction', 'reactionTypeId': choices[1], 'artistId': 930000001}
    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
        reacting = pool.submit(sql, f"SELECT interaction_legacy_command(930000004,'club_post',{literal(alias)},{literal(json.dumps(reaction))});")
        deleting = pool.submit(command, 930000002, target, {'operation': 'comment.delete', 'commentId': comment_id, 'expectedVersion': 1})
        result = json.loads(reacting.result())
        assert 'error' not in result or result['error'] == 'unavailable', result
        assert deleting.result()['state'] == 'deleted'
    assert sql(f"SELECT NOT EXISTS(SELECT 1 FROM interaction_reaction WHERE target_id={literal(alias_target)});") == 't'
    assert sql(f"SELECT coalesce(sum(total),0)=0 FROM interaction_reaction_total WHERE target_id={literal(alias_target)};") == 't'
print('PASS reply-alias reaction/deletion interleavings: independent slots retire without stale counts')

# The actual owner-allowlist command must lock submitted recipients. A privacy
# opt-out waits for that transaction, then revokes new writes and queued delivery.
sql("INSERT INTO social_v2_preference(party_id,discoverable) VALUES(930000004,true) ON CONFLICT(party_id) DO UPDATE SET discoverable=true;")
mention_payload = {'operation': 'comment.create', 'body': '@Recipient', 'mentions': [{'partyId': 930000004, 'start': 0, 'end': 10}]}
mention = command(930000001, target, mention_payload)
settings = {'operation': 'settings.update', 'commentPolicy': 'mentioned', 'expectedVersion': int(sql(f"SELECT version FROM interaction_target WHERE id={literal(target)};")), 'mentionedPartyIds': [930000004]}
writer = subprocess.Popen(['psql','-X','-qAt','-v','ON_ERROR_STOP=1','-d',DB], stdin=subprocess.PIPE,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE,text=True)
writer.stdin.write(f"BEGIN;\nSELECT interaction_command(930000001,{literal(target)},'{uuid.uuid4()}',{literal(json.dumps(settings))});\nSELECT 'mention-policy-locked';\n")
writer.stdin.flush()
settings_result = json.loads(writer.stdout.readline())
assert settings_result.get('commentPolicy') == 'mentioned', settings_result
assert writer.stdout.readline().strip() == 'mention-policy-locked'
previous_social_gate = sql('SELECT enabled FROM social_v2_runtime;')
sql('UPDATE social_v2_runtime SET enabled=true;')
revision = int(sql('SELECT revision FROM social_v2_preference WHERE party_id=930000004;'))
with concurrent.futures.ThreadPoolExecutor(max_workers=1) as pool:
    privacy = pool.submit(sql, f'SELECT social_v2_preferences(930000004,false,false,{revision});')
    time.sleep(.2)
    assert not privacy.done(), 'Owner allowlist skipped the recipient privacy lock'
    writer.stdin.write('COMMIT;\n\\q\n'); writer.stdin.flush(); writer.wait(timeout=10)
    result = json.loads(privacy.result(timeout=10)); assert result['discoverable'] is False, result
sql(f"UPDATE social_v2_runtime SET enabled={'true' if previous_social_gate == 't' else 'false'};")
result = json.loads(sql(f"SELECT interaction_command(930000001,{literal(target)},'{uuid.uuid4()}',{literal(json.dumps(mention_payload))});"))
assert result.get('error') == 'invalid', result
for _ in range(20):
    if sql('SELECT NOT EXISTS(SELECT 1 FROM interaction_event WHERE completed_at IS NULL);') == 't':
        break
    sql('SELECT interaction_dispatch_events(50);')
else:
    raise AssertionError('Synthetic notification drain exceeded its bound')
assert sql(f"SELECT NOT EXISTS(SELECT 1 FROM interaction_notification WHERE comment_id={literal(mention['id'])} AND recipient_id=930000004 AND event_kind='mention');") == 't'
print('PASS mention-policy/privacy serialization and queued notification revocation')

# Current catalog capabilities, including global role permissions, serialize with
# institutional discussion policy writes. Creator metadata grants no fallback.
sql("INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version) SELECT 930000001,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';")
record_key = sql("SELECT id FROM recording WHERE active ORDER BY id LIMIT 1;")
record_target = sql(f"SELECT interaction_register('recording',{literal(record_key)},930000001);")
grant_id = sql("SELECT rp.id FROM role_permission rp JOIN security_role r ON r.id=rp.role_id JOIN security_permission p ON p.id=rp.permission_id WHERE r.code='admin' AND p.code='catalog.update';")
settings = {'operation': 'settings.update', 'commentPolicy': 'off', 'expectedVersion': int(sql(f"SELECT version FROM interaction_target WHERE id={literal(record_target)};")), 'mentionedPartyIds': []}
writer = subprocess.Popen(['psql','-X','-qAt','-v','ON_ERROR_STOP=1','-d',DB], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,text=True)
writer.stdin.write(f"BEGIN;\nSELECT interaction_command(930000001,{literal(record_target)},'{uuid.uuid4()}',{literal(json.dumps(settings))});\nSELECT 'catalog-policy-locked';\n")
writer.stdin.flush()
result = json.loads(writer.stdout.readline()); assert result.get('commentPolicy') == 'off', result
assert writer.stdout.readline().strip() == 'catalog-policy-locked'
with concurrent.futures.ThreadPoolExecutor(max_workers=1) as pool:
    revocation = pool.submit(sql, f"UPDATE role_permission SET active=false WHERE id={literal(grant_id)};")
    time.sleep(.2)
    assert not revocation.done(), 'Capability revocation bypassed the active policy transaction'
    writer.stdin.write('COMMIT;\n\\q\n'); writer.stdin.flush(); writer.wait(timeout=10)
    revocation.result(timeout=10)
settings['expectedVersion'] = int(sql(f"SELECT version FROM interaction_target WHERE id={literal(record_target)};"))
settings['commentPolicy'] = 'everyone'
result = json.loads(sql(f"SELECT interaction_command(930000001,{literal(record_target)},'{uuid.uuid4()}',{literal(json.dumps(settings))});"))
assert result.get('error') == 'forbidden', result
sql(f"UPDATE role_permission SET active=true WHERE id={literal(grant_id)};")
print('PASS catalog capability revocation serializes and immediately denies later policy writes')

# Concurrent fresh re-reports reopen once, without overwriting open evidence.
command(930000001, record_target, settings)
reported = command(930000002, record_target, {'operation': 'comment.create', 'body': 'Re-report concurrency'})
report = {'operation': 'comment.report', 'commentId': reported['id'], 'reason': 'Original evidence'}
command(930000004, record_target, report)
command(930000001, record_target, {'operation': 'comment.report.resolve', 'commentId': reported['id'], 'expectedVersion': 1, 'decision': 'dismissed', 'reason': 'Reviewed'})
report['reason'] = 'New evidence'
with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
    results = list(pool.map(lambda _: command(930000004, record_target, report), range(4)))
assert all(result['reported'] for result in results)
assert sql(f"SELECT count(*)=1 FROM interaction_report WHERE comment_id={literal(reported['id'])} AND state='open' AND reason='New evidence';") == 't'
assert sql(f"SELECT count(*)=1 FROM interaction_audit WHERE comment_id={literal(reported['id'])} AND operation='comment.report.reopen' AND reason='Original evidence';") == 't'
print('PASS concurrent re-reports: one open report and one preserved prior-evidence audit')
