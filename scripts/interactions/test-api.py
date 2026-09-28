#!/usr/bin/env python3
"""Real session/HTTP/database checks; accepts only an isolated local test server."""
import json, os, pathlib, subprocess, time, urllib.error, urllib.parse, urllib.request, uuid
base = os.environ.get('TDF_INTERACTION_TEST_BASE', 'http://127.0.0.1:18128')
database = os.environ['TDF_INTERACTION_TEST_DATABASE']
assert urllib.parse.urlparse(base).hostname in ('127.0.0.1', 'localhost') and database.startswith('tdf_interaction_')
actor_base = int(os.environ.get('TDF_INTERACTION_TEST_ACTOR_BASE', '918000001'))
assert 918000001 <= actor_base <= 918999990
actors = [actor_base + i for i in range(3)]
tokens = {actor: str(uuid.uuid4()) + str(uuid.uuid4()) for actor in actors}
def sql(statement):
    return subprocess.check_output(['psql','-X','-v','ON_ERROR_STOP=1','-d',database,'-Atq'], input=statement, text=True).strip()
def request(actor, path, data=None, method=None, status=200):
    headers = {'Content-Type':'application/json'}
    if actor: headers['Authorization'] = 'Bearer ' + tokens[actor]
    req = urllib.request.Request(base+path, data=None if data is None else json.dumps(data).encode(), headers=headers, method=method)
    try:
        with urllib.request.urlopen(req, timeout=30) as response:
            code, body = response.status, response.read().decode()
    except urllib.error.HTTPError as error:
        code, body = error.code, error.read().decode()
    assert code in (status if isinstance(status, tuple) else (status,)), f'{req.get_method()} {path}: expected {status}, got {code}: {body[:300]}'
    if isinstance(status, tuple): return code
    return json.loads(body) if body and code < 400 else body
for actor in actors:
    sql(f"INSERT INTO party(id,display_name,is_org,created_at) VALUES({actor},'API actor {actor}',false,now()); INSERT INTO user_credential(party_id,username,password_hash,active) VALUES({actor},'interaction-api-{actor}','not-a-login-hash',true); INSERT INTO api_token(token,party_id,label,active) VALUES('{tokens[actor]}',{actor},'interaction-local-test',true); INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version) SELECT {actor},id,'bootstrap',true,now(),1 FROM security_role WHERE code='fan';")
sql(f"INSERT INTO fan_club(id,artist_party_id,name) VALUES({actors[0]},{actors[0]},'API test club'); INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES({actors[1]},{actors[0]},now()),({actors[2]},{actors[0]},now()); UPDATE interaction_runtime SET enabled=true WHERE singleton;")
post=request(actors[0],f'/fans/me/clubs/{actors[0]}/posts',{'fcpReqTitle':'API publication','fcpReqContent':'A real HTTP test post','fcpReqMediaUrls':[],'fcpReqParentId':None})
identity=f"/interactions/targets/club_post/{post['fcpId']}"
summary=request(actors[1],identity); target=summary['id']
def command(actor, payload, key=None, status=200):
    return request(actor,f'/interactions/targets/{target}/commands',{'requestKey':key or str(uuid.uuid4()),'command':payload},status=status)
# Reuse the actual scoped party selector and stable party IDs, with live privacy.
sql(f"INSERT INTO social_v2_preference(party_id,discoverable) VALUES({actors[2]},true) ON CONFLICT(party_id) DO UPDATE SET discoverable=true;")
search=f'/parties/search?context=interaction_mention&scopeId={target}&q=API&limit=20'
assert actors[2] in [item['partyId'] for item in request(actors[0],search)['items']]
sql(f"UPDATE social_v2_preference SET discoverable=false WHERE party_id={actors[2]};")
assert actors[2] not in [item['partyId'] for item in request(actors[0],search)['items']]
sql(f"UPDATE social_v2_preference SET discoverable=true WHERE party_id={actors[2]};")
settings={'reactions':False,'comments':True,'replies':True,'mentions':False}
assert request(actors[2],'/interactions/preferences',settings,method='PUT')==settings
assert request(actors[2],'/interactions/preferences')==settings
request(actors[2],'/interactions/preferences',{**settings,'ownerId':actors[0]},method='PUT',status=400)
request(None,identity,status=401)
request(None,'/public'+identity,status=404)
like=next(r['id'] for r in summary['reactions'] if r['code']=='like')
command(actors[1],{'operation':'reaction.set','reactionTypeId':like})
assert next(r['count'] for r in request(actors[0],identity)['reactions'] if r['id']==like)==1
key=str(uuid.uuid4()); payload={'operation':'comment.create','body':'Root discussion','mentions':[]}
root=command(actors[1],payload,key); assert command(actors[1],payload,key)['id']==root['id']
command(actors[1],{**payload,'body':'Different retry'},key,status=409)
reply=command(actors[2],{'operation':'comment.create','body':'Reply discussion','parentId':root['id'],'mentions':[]})
command(actors[2],{'operation':'comment.edit','commentId':root['id'],'expectedVersion':1,'body':'Unauthorized edit','mentions':[]},status=403)
edited=command(actors[1],{'operation':'comment.edit','commentId':root['id'],'expectedVersion':1,'body':'Edited root','mentions':[]}); assert edited['version']==2
# Use the real durable worker function, independent of the ten-second timer.
sql('SELECT interaction_dispatch_events(20);')
notifications=request(actors[1],'/fans/me/notifications')
notification=next(n for n in notifications if n.get('nTargetKey')==reply['id'])
assert notification['nTargetType']=='interaction_comment'
destination=request(actors[1],f"/interactions/resolve/comment/{reply['id']}")
assert destination['context']['root']['id']==root['id'] and destination['context']['comment']['id']==reply['id']
command(actors[1],{'operation':'comment.delete','commentId':root['id'],'expectedVersion':2})
context=request(actors[2],identity+'/comments/'+reply['id']); assert context['root']['state']=='deleted' and context['comment']['body']=='Reply discussion'
legacy=request(actors[2],f'/fans/me/clubs/{actors[0]}/posts',{'fcpReqTitle':None,'fcpReqContent':'Legacy client reply','fcpReqMediaUrls':[],'fcpReqParentId':post['fcpId']})
assert legacy['fcpContent']=='Legacy client reply'
assert sql(f"SELECT count(*) FROM fan_club_post WHERE id={legacy['fcpId']}")=='0'
# Owner policies apply to stale clients and compose with current target access.
version=request(actors[0],identity)['version']
command(actors[0],{'operation':'settings.update','commentPolicy':'mentioned','expectedVersion':version,'mentionedPartyIds':[actors[2]]})
command(actors[1],{'operation':'comment.create','body':'Not mentioned by content owner','mentions':[]},status=403)
mention=command(actors[2],{'operation':'comment.create','body':'@Owner hello','mentions':[{'partyId':actors[0],'start':0,'end':6}]})
assert mention['mentions'][0]['partyId']==actors[0]
version=request(actors[0],identity)['version']
command(actors[0],{'operation':'settings.update','commentPolicy':'off','expectedVersion':version,'mentionedPartyIds':[]})
command(actors[2],{'operation':'comment.create','body':'Disabled discussion','mentions':[]},status=403)
version=request(actors[0],identity)['version']
command(actors[0],{'operation':'settings.update','commentPolicy':'everyone','expectedVersion':version,'mentionedPartyIds':[]})
# Dense pages are seeded in the isolated DB, avoiding artificial rate-limit bypass
# in the HTTP client and proving that reads never serialize a full thread.
sql(f"INSERT INTO interaction_comment(id,target_id,author_id,root_id,body,created_at) SELECT id,'{target}',{actors[2]},id,'Synthetic page row',now()+n*interval '1 microsecond' FROM (SELECT gen_random_uuid() id,n FROM generate_series(1,125) n) x;")
page=request(actors[0],identity+'/comments?limit=20&sort=newest'); assert len(page['items'])==20 and page['nextCursor']
next_page=request(actors[0],identity+'/comments?limit=20&sort=newest&cursor='+page['nextCursor'])
assert not ({c['id'] for c in page['items']} & {c['id'] for c in next_page['items']})
request(actors[0],identity+'/comments?limit=5000',status=400)
state=request(actors[0],f'/interactions/blocks/{actors[1]}')
request(actors[0],f'/interactions/blocks/{actors[1]}',{'blockRequestKey':str(uuid.uuid4()),'blocked':True,'expectedVersion':state['version']},method='PUT')
request(actors[1],identity,status=404)
command(actors[1],{'operation':'comment.create','body':'Blocked write','mentions':[]},status=404)
assert not any(n.get('nTargetKey')==reply['id'] for n in request(actors[1],'/fans/me/notifications'))
# Unblock and prove a revoked bearer cannot replay a previously accepted request.
state=request(actors[0],f'/interactions/blocks/{actors[1]}')
request(actors[0],f'/interactions/blocks/{actors[1]}',{'blockRequestKey':str(uuid.uuid4()),'blocked':False,'expectedVersion':state['version']},method='PUT')
# Rejected moderation attempts must consume the same per-account write budget.
limited=False
for attempt in range(91):
    result=command(actors[1],{'operation':'comment.remove','commentId':reply['id'],'expectedVersion':1,'reason':'Synthetic unauthorized attempt'},status=(403,429))
    if result==429:
        limited=True
        break
assert limited, 'Rejected requests escaped the HTTP abuse budget'
sql(f"UPDATE api_token SET active=false WHERE party_id={actors[1]};")
command(actors[1],payload,key,status=401)
# Leave actor 2 revoked. The UI E2E uses the current owner and third account.
fixture=os.environ.get('TDF_INTERACTION_TEST_FIXTURE')
if fixture:
    output=pathlib.Path(fixture); output.parent.mkdir(parents=True,exist_ok=True); output.parent.chmod(0o700)
    output.write_text(json.dumps({'base':base,'database':database,'actors':actors,'tokens':tokens,'postId':post['fcpId'],'targetId':target,'rootId':root['id'],'replyId':reply['id']})); output.chmod(0o600)
print('PASS HTTP sessions, publication, reactions, idempotency, comments/replies, edits, tombstones, notifications/deep links, legacy adapter, scoped mention privacy, notification preferences, owner policies, pagination, blocks, rejected-write throttling and bearer revocation')
