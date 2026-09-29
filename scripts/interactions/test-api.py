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
sql(f"INSERT INTO social_v2_preference(party_id,discoverable) VALUES({actors[0]},true),({actors[2]},true) ON CONFLICT(party_id) DO UPDATE SET discoverable=true;")
search=f'/parties/search?context=interaction_mention&scopeId={target}&q=API&limit=20'
assert actors[2] in [item['partyId'] for item in request(actors[0],search)['items']]
sql(f"UPDATE social_v2_preference SET discoverable=false WHERE party_id={actors[2]};")
assert actors[2] not in [item['partyId'] for item in request(actors[0],search)['items']]
private_mentions=[{'partyId':actors[2],'start':0,'end':8}]
private_draft=command(actors[0],{'operation':'comment.create','body':'Unchanged private-mention draft','mentions':[]})
private_count=request(actors[0],identity)['commentCount']
command(actors[0],{'operation':'comment.create','body':'@Private','mentions':private_mentions},status=400)
command(actors[0],{'operation':'comment.edit','commentId':private_draft['id'],'expectedVersion':1,'body':'@Private','mentions':private_mentions},status=400)
assert request(actors[0],identity)['commentCount']==private_count
assert request(actors[0],identity+'/comments/'+private_draft['id'])['comment']['body']=='Unchanged private-mention draft'
command(actors[0],{'operation':'settings.update','commentPolicy':'mentioned','expectedVersion':request(actors[0],identity)['version'],'mentionedPartyIds':[actors[2]]},status=400)
sql(f"INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b) VALUES({actors[0]},{actors[2]},true,true) ON CONFLICT(party_a,party_b) DO UPDATE SET consent_a=true,consent_b=true;")
assert actors[2] in [item['partyId'] for item in request(actors[0],search)['items']]
private_mention=command(actors[0],{'operation':'comment.create','body':'@Private','mentions':private_mentions})
command(actors[0],{'operation':'settings.update','commentPolicy':'mentioned','expectedVersion':request(actors[0],identity)['version'],'mentionedPartyIds':[actors[2]]})
sql(f"UPDATE social_v2_pair SET consent_a=false,consent_b=false WHERE party_a={actors[0]} AND party_b={actors[2]}; SELECT interaction_dispatch_events(50);")
assert not any(row.get('nTargetKey')==private_mention['id'] for row in request(actors[2],'/fans/me/notifications'))
for non_mention_policy in ('off','followers','everyone'):
    changed=command(actors[0],{'operation':'settings.update','commentPolicy':non_mention_policy,'expectedVersion':request(actors[0],identity)['version'],'mentionedPartyIds':[actors[2]]})
    assert changed['commentPolicy']==non_mention_policy
    assert request(actors[0],identity)['mentionedPeople']==[]


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
sql(f"DELETE FROM fan_follow WHERE fan_party_id={actors[1]} AND artist_party_id={actors[0]};")
withdrawal=request(actors[1],identity)
assert withdrawal['canReact'] and withdrawal['myReactionTypeId']==like
assert not any(item['selectable'] for item in withdrawal['reactions'])
command(actors[1],{'operation':'reaction.set','reactionTypeId':like},status=400)
command(actors[1],{'operation':'reaction.set','reactionTypeId':None})
withdrawn=request(actors[1],identity)
assert not withdrawn['canReact'] and withdrawn['myReactionTypeId'] is None
assert sum(item['count'] for item in withdrawn['reactions'])==0
sql(f"INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES({actors[1]},{actors[0]},now());")
assert request(actors[1],identity)['myReactionTypeId'] is None
command(actors[1],{'operation':'reaction.set','reactionTypeId':like})
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
nested=request(actors[2],f'/fans/me/clubs/{actors[0]}/posts',{'fcpReqTitle':None,'fcpReqContent':'Nested legacy client reply','fcpReqMediaUrls':[],'fcpReqParentId':legacy['fcpId']})
assert nested['fcpParentId']==legacy['fcpId']
root_total=sum(r['count'] for r in request(actors[0],identity)['reactions'])
legacy_reaction=request(actors[2],f"/fans/me/clubs/{actors[0]}/posts/{legacy['fcpId']}/react",{'crrReactionTypeId':like})
assert legacy_reaction['rsTotal']==1 and legacy_reaction['rsMyReactionTypeId']==like
assert sum(r['count'] for r in request(actors[0],identity)['reactions'])==root_total
request(actors[2],f"/fans/me/clubs/{actors[2]}/posts/{legacy['fcpId']}/react",{'crrReactionTypeId':like},status=404)
legacy_reaction=request(actors[2],f"/fans/me/clubs/{actors[0]}/posts/{legacy['fcpId']}/react",{'crrReactionTypeId':like})
assert legacy_reaction['rsTotal']==0 and legacy_reaction['rsMyReactionTypeId'] is None

# Old memory clients must be able to remove their own reaction after losing
# write access, and after a once-valid choice is retired. Source/path checks stay.
sql(f"INSERT INTO fan_club_member_profile(id,party_id,club_id) VALUES({actors[0]},{actors[0]},{actors[0]}); INSERT INTO fan_club_memory(id,member_profile_id,title) VALUES({actors[0]},{actors[0]},'Memory withdrawal fixture');")
memory_path=f'/fans/me/clubs/{actors[0]}/memories/{actors[0]}/react'
memory_identity=f'/interactions/targets/club_memory/{actors[0]}'
assert request(actors[1],memory_path,{'crrReactionTypeId':like})['rsTotal']==1
sql(f"DELETE FROM fan_follow WHERE fan_party_id={actors[1]} AND artist_party_id={actors[0]};")
assert request(actors[1],memory_path,{'crrReactionTypeId':like})['rsTotal']==0
request(actors[1],memory_path,{'crrReactionTypeId':like},status=400)
sql(f"INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES({actors[1]},{actors[0]},now());")
assert request(actors[1],memory_path,{'crrReactionTypeId':like})['rsTotal']==1
sql(f"UPDATE catalog_definition SET active=false WHERE id=(SELECT catalog_id FROM content_reaction_type WHERE id='{like}');")
try:
    removed=request(actors[1],memory_path,{'crrReactionTypeId':like})
    assert removed['rsTotal']==0 and removed['rsMyReactionTypeId'] is None
    request(actors[1],memory_path,{'crrReactionTypeId':like},status=400)
    assert sum(row['count'] for row in request(actors[0],memory_identity)['reactions'])==0
finally:
    sql(f"UPDATE catalog_definition SET active=true WHERE id=(SELECT catalog_id FROM content_reaction_type WHERE id='{like}');")
request(actors[1],f'/fans/me/clubs/{actors[2]}/memories/{actors[0]}/react',{'crrReactionTypeId':like},status=404)

# Imported artist updates do not carry public publication authority.
sql(f"INSERT INTO artist_profile(artist_party_id,created_at) VALUES({actors[0]},now()); INSERT INTO social_sync_post(id,platform,external_post_id,artist_party_id,caption,fetched_at,ingest_source,created_at,updated_at) VALUES({actors[0]},'instagram','synthetic-api-private-update-{actors[0]}',{actors[0]},'Private ingestion caption',now(),'manual',now(),now());")
request(None,f'/public/interactions/targets/artist_update/{actors[0]}',status=404)
request(actors[0],f'/interactions/targets/artist_update/{actors[0]}',status=404)
# The legacy event-moment array has no cursor; activation must not truncate it.
sql(f"INSERT INTO social_event(id,organizer_party_id,title,start_time,event_type_id,workflow_state_id) SELECT {actors[0]},'{actors[0]}','Moment compatibility event',now(),id,'00000000-0000-4000-8000-000000000232' FROM event_type WHERE code='concert'; INSERT INTO event_moment(event_id,author_party_id,author_name,media_url,media_type) SELECT {actors[0]},'{actors[0]}','API fixture','https://example.test/photo.jpg','image' FROM generate_series(1,60);")
moments=request(actors[0],f'/social-events/events/{actors[0]}/moments')
assert len(moments)==60 and len({m['emId'] for m in moments})==60, 'Canonical compatibility must retain older moments'
# Private publication links must resolve to the existing authenticated event page.
event_summary=request(actors[0],f'/interactions/targets/event/{actors[0]}')
assert event_summary['route']==f'/social/eventos/{actors[0]}'
moment_summary=request(actors[0],f"/interactions/targets/event_moment/{moments[0]['emId']}")
assert moment_summary['route']==f"/social/eventos/{actors[0]}?moment={moments[0]['emId']}"
request(actors[0],f'/social-events/events/{actors[0]}')
request(None,f'/public/interactions/targets/event/{actors[0]}',status=404)
sql(f"UPDATE social_event SET metadata='{{\"isPublic\":true}}',workflow_state_id='00000000-0000-4000-8000-000000000239' WHERE id={actors[0]};")
assert request(None,f'/public/interactions/targets/event/{actors[0]}')['route']==f'/eventos/{actors[0]}'
assert request(None,f"/public/interactions/targets/event_moment/{moments[0]['emId']}")['route']==f"/eventos/{actors[0]}?moment={moments[0]['emId']}"


# Event legacy catalog validation must not strand historical reactions.
moment_key=moments[0]['emId']
moment_path=f'/social-events/events/{actors[0]}/moments/{moment_key}/reactions'
moment_identity=f'/interactions/targets/event_moment/{moment_key}'
moment_reaction=next(r['id'] for r in request(actors[0],moment_identity)['reactions'] if r['code']=='fire')
legacy_fire=sql("SELECT id FROM reaction_type WHERE code='fire' LIMIT 1;")
assert legacy_fire
request(actors[0],moment_path,{'emrrReactionTypeId':legacy_fire,'emrrActive':True})
assert request(actors[0],moment_identity)['myReactionTypeId']==moment_reaction
sql(f"UPDATE catalog_definition SET active=false WHERE id IN (SELECT catalog_id FROM reaction_type WHERE id='{legacy_fire}' UNION SELECT catalog_id FROM content_reaction_type WHERE id='{moment_reaction}');")
try:
    request(actors[0],moment_path,{'emrrReactionTypeId':legacy_fire,'emrrActive':False})
    withdrawn_moment=request(actors[0],moment_identity)
    assert withdrawn_moment['myReactionTypeId'] is None and sum(row['count'] for row in withdrawn_moment['reactions'])==0
    request(actors[0],moment_path,{'emrrReactionTypeId':legacy_fire,'emrrActive':True},status=400)
finally:
    sql(f"UPDATE catalog_definition SET active=true WHERE id IN (SELECT catalog_id FROM reaction_type WHERE id='{legacy_fire}' UNION SELECT catalog_id FROM content_reaction_type WHERE id='{moment_reaction}');")

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
# Moderation survives author blocks without reopening ordinary social access.
moderator=actor_base+3
tokens[moderator]=str(uuid.uuid4())+str(uuid.uuid4())
sql(f"INSERT INTO party(id,display_name,is_org,created_at) VALUES({moderator},'API moderation fixture',false,now()); INSERT INTO user_credential(party_id,username,password_hash,active) VALUES({moderator},'interaction-api-{moderator}','not-a-login-hash',true); INSERT INTO api_token(token,party_id,label,active) VALUES('{tokens[moderator]}',{moderator},'interaction-local-test',true); INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version) SELECT {moderator},id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';")
evidence=command(actors[2],{'operation':'comment.create','body':'Moderation block fixture','mentions':[]})
command(actors[0],{'operation':'comment.report','commentId':evidence['id'],'reason':'Review blocked-author content'})
for peer in [actors[0],moderator]:
    state=request(actors[2],f'/interactions/blocks/{peer}')
    request(actors[2],f'/interactions/blocks/{peer}',{'blockRequestKey':str(uuid.uuid4()),'blocked':True,'expectedVersion':state['version']},method='PUT')
owner_block=request(actors[0],f'/interactions/blocks/{moderator}')
request(actors[0],f'/interactions/blocks/{moderator}',{'blockRequestKey':str(uuid.uuid4()),'blocked':True,'expectedVersion':owner_block['version']},method='PUT')
moderator_summary=request(moderator,identity)
assert moderator_summary['canModerate'] and not moderator_summary['canComment'] and not moderator_summary['canReact']
command(moderator,{'operation':'comment.create','body':'Blocked social contact','mentions':[]},status=404)
request(moderator,identity+'/comments?limit=20',status=404)
queue=request(actors[0],f'/interactions/moderation/{target}?limit=20')
assert any(c['id']==evidence['id'] and c['state']=='visible' and c['moderationBody']=='Moderation block fixture' for c in queue['items'])
reports=request(moderator,'/interactions/reports?limit=20')
assert any(c['id']==evidence['id'] and c['reportReasons']==['Review blocked-author content'] for c in reports['items'])
linked=request(moderator,f"/interactions/resolve/comment/{evidence['id']}")
assert linked['context']['comment']['body']=='' and linked['context']['comment']['author'] is None
assert not any(c['id']==evidence['id'] for c in request(actors[0],identity+'/comments?limit=20')['items'])
command(actors[0],{'operation':'comment.remove','commentId':evidence['id'],'expectedVersion':1,'reason':'Owner is not a moderator'},status=404)
command(actors[0],{'operation':'comment.hide','commentId':evidence['id'],'expectedVersion':1,'reason':'Scoped owner hide'})
command(actors[0],{'operation':'comment.restore','commentId':evidence['id'],'expectedVersion':2,'reason':'Scoped owner restore'})
command(moderator,{'operation':'comment.report.resolve','commentId':evidence['id'],'expectedVersion':3,'reason':'Reviewed report','decision':'reviewed'})
removed=command(moderator,{'operation':'comment.remove','commentId':evidence['id'],'expectedVersion':3,'reason':'Administrative removal'})
assert removed['state']=='removed' and removed['body']==''
for peer in [actors[0],moderator]:
    state=request(actors[2],f'/interactions/blocks/{peer}')
    request(actors[2],f'/interactions/blocks/{peer}',{'blockRequestKey':str(uuid.uuid4()),'blocked':False,'expectedVersion':state['version']},method='PUT')
owner_block=request(actors[0],f'/interactions/blocks/{moderator}')
request(actors[0],f'/interactions/blocks/{moderator}',{'blockRequestKey':str(uuid.uuid4()),'blocked':False,'expectedVersion':owner_block['version']},method='PUT')
# Explicitly restore this synthetic fixture's follow for subsequent browser/native flows.
sql(f"INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES({actors[2]},{actors[0]},now());")
# Leave actor 2 revoked. The UI E2E uses the current owner and third account.
fixture=os.environ.get('TDF_INTERACTION_TEST_FIXTURE')
if fixture:
    output=pathlib.Path(fixture); output.parent.mkdir(parents=True,exist_ok=True); output.parent.chmod(0o700)
    output.write_text(json.dumps({'base':base,'database':database,'actors':actors,'tokens':tokens,'postId':post['fcpId'],'targetId':target,'rootId':root['id'],'replyId':reply['id']})); output.chmod(0o600)
print('PASS HTTP sessions, publication, reactions, idempotency, comments/replies, edits, tombstones, notifications/deep links, legacy adapter, scoped mention privacy, notification preferences, owner policies, pagination, blocks, rejected-write throttling and bearer revocation')
