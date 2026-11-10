import assert from 'node:assert/strict';
import { createHash,randomUUID } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { writeFileSync } from 'node:fs';
import path from 'node:path';

const port=Number(process.argv[2]);assert(Number.isInteger(port)&&port>0&&port<65536);
assert.equal(process.env.TDF_EVENT_CRM_DISPOSABLE_HTTP,'1');
const base=`http://127.0.0.1:${port}`;
const db=process.env.TDF_EVENT_CRM_TEST_DB;
assert(/^tdf_event_crm_http_[a-f0-9]+_test$/.test(db));
const env=Object.fromEntries(Object.entries(process.env).filter(([key])=>!key.startsWith('PG')));
env.PGPASSWORD=process.env.TDF_EVENT_CRM_TEST_PASSWORD??'';
function sql(query){return execFileSync(process.env.TDF_EVENT_CRM_PSQL??'psql',[
  '-h','127.0.0.1','-p',process.env.TDF_EVENT_CRM_TEST_PORT,'-U',process.env.TDF_EVENT_CRM_TEST_USER,'-d',db,'-X','-qAt','-v','ON_ERROR_STOP=1','-c',query
],{encoding:'utf8',env}).trim();}
const token=id=>createHash('sha256').update(`event-crm-synthetic-token-${id}`).digest('hex');
const checks=[];
function check(name,condition){assert(condition,name);checks.push(name);console.log(`PASS ${name}`);}
async function request(url,{actor,body,method=body?'POST':'GET',key,lookup}={}){
  const headers={};if(actor)headers.Authorization=`Bearer ${token(actor)}`;
  if(body)headers['Content-Type']='application/json';if(key)headers['Idempotency-Key']=key;
  if(lookup)headers['X-Order-Lookup-Token']=lookup;
  const response=await fetch(base+url,{method,headers,body:body?JSON.stringify(body):undefined});
  const raw=await response.text();let data;try{data=JSON.parse(raw);}catch{data=null;}
  return {status:response.status,data,headers:response.headers};
}
const cmd=(actor,body,event=7001,key=randomUUID())=>request(`/event-crm/events/${event}/commands`,{actor,body,key});
const invite=(actor,secret,accept=false)=>request('/event-crm/invitations',{actor,body:{token:secret,accept}});

check('missing SMTP leaves email verification unavailable without claiming delivery',(await request('/event-crm/email/request',{actor:7101,method:'POST'})).status===503);
check('unauthenticated CRM refuses access',(await request('/event-crm/events/7001')).status===401);
const owner=await request('/event-crm/events/7001',{actor:7101});
check('actual session witness and event owner authorize private read',owner.status===200&&owner.data.role==='owner'&&owner.headers.get('cache-control').includes('no-store'));
check('another authenticated account cannot read or export',(await request('/event-crm/events/7001/export',{actor:7102})).status===404);
const emailTarget='invited-new-synthetic@example.invalid';
const byEmail=await cmd(7101,{operation:'recipient.lookup',query:emailTarget});
check('email recipient lookup returns no raw address',byEmail.status===200&&byEmail.data.candidates.length===0&&!JSON.stringify(byEmail.data).includes(emailTarget));
const emailInvite=await cmd(7101,{operation:'invitation.create',recipientEmail:emailTarget,role:'sales'});
check('email invitation persists only fingerprints',emailInvite.status===200&&sql(`SELECT recipient_email_hash=encode(digest('${emailTarget}','sha256'),'hex') FROM event_crm_invitation WHERE id='${emailInvite.data.invitationId}'::uuid`)==='t'&&sql(`SELECT outcome::text FROM event_crm_command_receipt WHERE command_id='${emailInvite.data.invitationId}'::uuid`).includes(emailTarget)===false);
await cmd(7101,{operation:'invitation.revoke',invitationId:emailInvite.data.invitationId});
let issued=await cmd(7101,{operation:'invitation.create',recipientPartyId:7104,role:'coordinator'});
check('owner issues a secret once without storing it',issued.status===200&&issued.data.token.length===43&&sql("SELECT count(*) FROM event_crm_invitation WHERE token_hash ~ '^[a-f0-9]{64}$'")==='2');
check('wrong account cannot preview',(await invite(7102,issued.data.token)).status===403);
const preview=await invite(7104,issued.data.token);
check('preview shows canonical event role and permissions before acceptance',preview.status===200&&preview.data.role==='coordinator'&&preview.data.permissions.includes('crm.all'));
const race=await Promise.all([invite(7104,issued.data.token,true),invite(7104,issued.data.token,true)]);
check('concurrent HTTP acceptance grants one membership',race.filter(r=>r.status===200).length===1&&sql('SELECT count(*) FROM event_crm_membership WHERE party_id=7104')==='1');
check('coordinator cannot issue verifier or higher role',(await cmd(7104,{operation:'invitation.create',recipientPartyId:7105,role:'payment_verifier'})).status===403);
check('coordinator cannot alter prices through the existing route',(await request('/social-events/events/7001/ticket-tiers/7001',{actor:7104,method:'PUT',body:{tierEventId:'7001',tierCode:'general',tierName:'Unapproved',tierPriceCents:1,tierCurrency:'USD',tierQuantityTotal:50,tierQuantitySold:0,tierIsActive:true}})).status!==200);

for(const kind of ['expired','revoked']){
 const r=await cmd(7101,{operation:'invitation.create',recipientPartyId:7105,role:'sales'});
 if(kind==='expired')sql(`UPDATE event_crm_invitation SET issued_at=issued_at-interval '8 days',expires_at=expires_at-interval '8 days' WHERE id='${r.data.invitationId}'::uuid`);
 else await cmd(7101,{operation:'invitation.revoke',invitationId:r.data.invitationId});
 check(`${kind} HTTP invitation refuses acceptance`,(await invite(7105,r.data.token,true)).status===404);
}
const importRows=[{name:'Contacto sintético HTTP',email:'crm-http-only@example.invalid',phone:'0991112223',stage:'paid',quantity:2},
  {name:'Contacto sintético HTTP',email:'crm-http-only@example.invalid',phone:'+593991112223',stage:'reserved',quantity:2}];
const plan=await cmd(7104,{operation:'import.preview',rows:importRows});
check('preview detects file duplicates and unpaid claims',plan.status===200&&plan.data.preview[1].duplicateOf===0&&plan.data.preview[0].paymentReconciliationRequired);
await cmd(7104,{operation:'import.confirm',importId:plan.data.importId});
await cmd(7104,{operation:'import.confirm',importId:plan.data.importId});
check('HTTP repeated import produces one contact/link and zero sales',sql("SELECT count(*) FROM party WHERE primary_email='crm-http-only@example.invalid'")==='1'&&sql('SELECT count(*) FROM event_crm_lead')==='1'&&sql('SELECT count(*) FROM event_ticket_order')==='0');

const verifier=await cmd(7101,{operation:'invitation.create',recipientPartyId:7107,role:'payment_verifier'});
await invite(7107,verifier.data.token,true);
check('explicit verifier membership confers no global admin role',sql("SELECT count(*) FROM party_role WHERE party_id=7107 AND role::text='Admin' AND active")==='0');
const orders=[];
for(const provider of ['cash','bank_transfer']){
 const created=await request('/public/events/7001/ticket-orders',{key:`crm-http-checkout-${randomUUID()}`,body:{tierId:7001,quantity:1,buyerName:'Comprador sintético',buyerEmail:`crm-${provider}@example.invalid`,termsAccepted:true}});
 if(created.status!==201){console.log(JSON.stringify({failure:'canonical checkout refused',status:created.status,code:created.data?.code??null}));}
 assert.equal(created.status,201,'Canonical public checkout must create the order');
 const orderId=created.data.orderId;assert(Number.isSafeInteger(orderId));orders.push(orderId);
 // The existing checkout assigned the canonical buyer identity; link that same contact.
 const buyerId=Number(sql(`SELECT buyer_party_id FROM event_ticket_order WHERE id=${orderId}`));
 const leadResult=await cmd(7104,{operation:'lead.create',row:{name:'Comprador sintético',email:`crm-${provider}@example.invalid`,...(buyerId?{contactId:buyerId}:{}),orderId,quantity:1,stage:'reserved',source:'checkout de ensayo'}});
 assert.equal(leadResult.status,200,'Canonical order/lead link must be valid');
 if(provider==='bank_transfer'){
  const selection=await request(`/public/events/7001/ticket-orders/${orderId}/bank-transfer`,{method:'POST',lookup:created.data.lookupToken});
  assert.equal(selection.status,200,'Existing bank-transfer selection must remain required');
  check('reserved lead follows the extended bank-transfer hold expiry',sql(`SELECT l.stage='reserved' AND l.reservation_expires_at=greatest(r.hold_expires_at,r.manual_hold_expires_at) AND r.manual_hold_expires_at>r.hold_expires_at FROM event_crm_lead l JOIN event_ticket_checkout_runtime r ON r.order_id=l.order_id WHERE l.order_id=${orderId}`)==='t');
 }
 const claim={operation:'manual.submit',orderId,provider,reference:`TEST-${provider}-${orderId}`,occurredAt:new Date().toISOString(),amountMinor:2000};
 const submitted=await cmd(7104,claim);
 check(`${provider} submission uses existing evidence without settling`,submitted.status===200&&submitted.data.paid===false&&sql(`SELECT status FROM event_ticket_order WHERE id=${orderId}`)==='pending');
 check(`${provider} coordinator cannot confirm payment`,(await cmd(7104,{operation:'manual.review',orderId,provider,action:'approve',notes:'Not authorized'})).status===403);
 const review={operation:'manual.review',orderId,provider,action:'approve',notes:'Matched synthetic independent evidence'};
 const key=randomUUID();const approved=await cmd(7107,review,7001,key);
 check(`${provider} designated verifier settles and issues once`,approved.status===200&&approved.data.paid===true&&sql(`SELECT count(*) FROM event_ticket WHERE order_ref_id=${orderId}`)==='1');
 const retry=await cmd(7107,review,7001,key);
 check(`${provider} confirmation retry does not duplicate payment tickets or inventory`,retry.status===200&&retry.data.replayed===true&&sql(`SELECT count(*) FROM event_ticket WHERE order_ref_id=${orderId}`)==='1'&&sql(`SELECT count(*) FROM commerce_receipt x JOIN event_ticket_checkout_runtime r ON r.checkout_id=x.checkout_id WHERE r.order_id=${orderId} AND x.kind='payment_receipt'`)==='1');
 check(`${provider} linked CRM stage follows canonical paid order`,sql(`SELECT stage FROM event_crm_lead WHERE order_id=${orderId}`)==='paid');
}
const before=sql('SELECT quantity_sold FROM event_ticket_tier WHERE id=7001');
check('only two canonical holds/sales occupy inventory, independent of list duplicates',before==='2');
const refundRequest=await request(`/social-events/events/7001/ticket-orders/${orders[1]}/refund`,{actor:7101,body:{refundRequestReason:'Synthetic verified bank refund',refundRequestAmountCents:2000}});
assert.equal(refundRequest.status,200,'Existing canonical refund request must succeed');
const refundId=refundRequest.data.refundId;
check('coordinator and payment verifier cannot execute refunds',
  (await request(`/social-events/events/7001/refunds/${refundId}/bank-transfer-complete`,{actor:7104,body:{brcReference:'TEST-BANK-REFUND'}})).status===403&&
  (await request(`/social-events/events/7001/refunds/${refundId}/bank-transfer-complete`,{actor:7107,body:{brcReference:'TEST-BANK-REFUND'}})).status===403);
check('existing refund separation of duties stays enforced',(await request(`/social-events/events/7001/refunds/${refundId}/bank-transfer-complete`,{actor:7101,body:{brcReference:'TEST-BANK-REFUND'}})).status===409);
const existingAdmin=Number(sql("SELECT id FROM party WHERE display_name='Catalog recovery fixture A'"));
const refunded=await request(`/social-events/events/7001/refunds/${refundId}/bank-transfer-complete`,{actor:existingAdmin,body:{brcReference:'TEST-BANK-REFUND'}});
assert.equal(refunded.status,200,'Existing independent refund controller must complete the refund');
check('refund projects net sales CRM and inventory from existing order',sql(`SELECT stage FROM event_crm_lead WHERE order_id=${orders[1]}`)==='closed_lost'&&sql('SELECT quantity_sold FROM event_ticket_tier WHERE id=7001')==='1'&&sql("SELECT sum(c.paid_minor-c.refunded_minor) FROM commerce_checkout_session c JOIN event_ticket_checkout_runtime r ON r.checkout_id=c.id WHERE r.event_id=7001")==='2000');
const refundedAgain=await request(`/social-events/events/7001/refunds/${refundId}/bank-transfer-complete`,{actor:existingAdmin,body:{brcReference:'TEST-BANK-REFUND'}});
check('refund retry does not release inventory twice',refundedAgain.status===200&&sql('SELECT quantity_sold FROM event_ticket_tier WHERE id=7001')==='1');
const abandoned=await request('/public/events/7001/ticket-orders',{key:`crm-cancel-${randomUUID()}`,body:{tierId:7001,quantity:1,buyerName:'Reserva sintética',buyerEmail:'abandoned@example.invalid',termsAccepted:true}});
assert.equal(abandoned.status,201);
sql("SELECT event_ticket_checkout_expire_holds(clock_timestamp()+interval '31 hours',NULL,7001)");
check('existing canonical hold expiry releases inventory once',sql('SELECT quantity_sold FROM event_ticket_tier WHERE id=7001')==='1');
sql("SELECT event_ticket_checkout_expire_holds(clock_timestamp()+interval '31 hours',NULL,7001)");
check('repeated expiry preserves paid inventory',sql('SELECT quantity_sold FROM event_ticket_tier WHERE id=7001')==='1');
sql('UPDATE event_ticket_tier SET quantity_total=quantity_sold+1 WHERE id=7001');
const lastSeat=await Promise.all([0,1].map(n=>request('/public/events/7001/ticket-orders',{key:`crm-last-seat-${n}-${randomUUID()}`,body:{tierId:7001,quantity:1,buyerName:'Última entrada sintética',buyerEmail:`last-seat-${n}@example.invalid`,termsAccepted:true}})));
check('two real checkouts competing for the last ticket retain exactly one place',lastSeat.filter(r=>r.status===201).length===1&&lastSeat.filter(r=>r.status===409).length===1&&sql('SELECT quantity_sold FROM event_ticket_tier WHERE id=7001')==='2');
sql("SELECT event_ticket_checkout_expire_holds(clock_timestamp()+interval '31 hours',NULL,7001)");
check('expiring the competing last-ticket hold returns only its own place',sql('SELECT quantity_sold FROM event_ticket_tier WHERE id=7001')==='1');
// A refused review must keep the reconciliation exception it recorded.
const lateOrder=await request('/public/events/7001/ticket-orders',{key:`crm-late-${randomUUID()}`,body:{tierId:7001,quantity:1,buyerName:'Pago tardío sintético',buyerEmail:'late-payment@example.invalid',termsAccepted:true}});
assert.equal(lateOrder.status,201,'Canonical public checkout must create the late-payment order');
const lateId=lateOrder.data.orderId;assert(Number.isSafeInteger(lateId));
const lateClaim=await cmd(7104,{operation:'manual.submit',orderId:lateId,provider:'cash',reference:`TEST-late-${lateId}`,occurredAt:new Date().toISOString(),amountMinor:2000});
assert.equal(lateClaim.status,200,'Late cash claim must be submitted while the hold is live');
sql(`UPDATE event_ticket_checkout_runtime SET hold_expires_at=clock_timestamp()+interval '2 seconds' WHERE order_id=${lateId}`);
await new Promise(resolve=>setTimeout(resolve,3000));
const lateReview=await cmd(7107,{operation:'manual.review',orderId:lateId,provider:'cash',action:'approve',notes:'Synthetic approval after hold expiry'});
check('refused review after hold expiry keeps its reconciliation exception and issues nothing',
  lateReview.status===409&&lateReview.data.code==='manual_review_refused'&&
  sql(`SELECT count(*) FROM commerce_reconciliation_exception WHERE exception_type='manual_payment_after_ticket_hold_expiry' AND internal_reference='${lateId}'`)==='1'&&
  sql(`SELECT count(*) FROM event_ticket WHERE order_ref_id=${lateId}`)==='0');
const member=race.find(r=>r.status===200).data.membershipId;
await cmd(7101,{operation:'membership.revoke',membershipId:member});
check('removing coordinator blocks subsequent reads exports and writes',
  (await request('/event-crm/events/7001',{actor:7104})).status===404&&
  (await request('/event-crm/events/7001/export',{actor:7104})).status===404&&
  (await cmd(7104,{operation:'import.preview',rows:importRows})).status===404);
const verifierMember=sql('SELECT id FROM event_crm_membership WHERE party_id=7107 AND revoked_at IS NULL');
await cmd(7101,{operation:'membership.revoke',membershipId:verifierMember});
check('withdrawing verifier access blocks financial confirmation retries',(await cmd(7107,{operation:'manual.review',orderId:orders[0],provider:'cash',action:'approve',notes:'Retry after revoked'})).status===404);
check('pilot identity is private and transport sales remain disabled',(await request('/event-crm/events/7002',{actor:7101})).data.combinationSalesEnabled===false);
check('private draft keeps price windows and unknown trip capacities without selling',sql("SELECT count(*) FROM event_ticket_tier WHERE event_id=7002 AND NOT is_active AND ((price_cents=2000 AND sales_end='2026-12-02T04:59:59.999999Z'::timestamptz) OR (price_cents=3000 AND sales_start='2026-12-05T05:00:00Z'::timestamptz))")==='2'&&sql('SELECT count(*) FROM event_crm_transport_slot WHERE event_id=7002 AND capacity IS NULL')==='5');

// Optional real-browser phase uses the same live authenticated HTTP server.
if(process.env.TDF_EVENT_CRM_BROWSER==='1'){
 const {runCrmBrowser}=await import('./event-crm-browser.mjs');await runCrmBrowser({base,token,cmd,sql,request,invite,checks});
}
writeFileSync(path.join(process.env.TDF_EVENT_CRM_ARTIFACTS,'http-results.json'),JSON.stringify({passed:checks.length,checks,synthetic:true,externalProviderCalls:0,realInvitations:0},null,2)+'\n');
console.log(JSON.stringify({passed:checks.length,failed:0,synthetic:true}));
