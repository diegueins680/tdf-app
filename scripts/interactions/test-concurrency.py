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
