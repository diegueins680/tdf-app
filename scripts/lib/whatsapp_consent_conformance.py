"""PRIV-WHATSAPP-001 over the real backend and PostgreSQL.

Simultaneous first public requests and the signed Meta webhook confirmation
path, which must answer 200 and record the confirmation notice. The fixture
backend has no WhatsApp provider, so every claimed request is marked
undelivered; the delivered pending request for the reply checks is prepared
directly in the consent row. The unit and PostgreSQL suites cover the claim,
window and withdrawal rules themselves.
"""
import concurrent.futures
import json
import time


def verify_whatsapp_consent(sql, request, check):
    phone = '+593990007321'
    row = lambda: sql("SELECT consent||','||coalesce(note,'')||','||(confirmation_requested_at IS NOT NULL)||','||(revoked_at IS NOT NULL)"
                      " FROM whats_app_consent WHERE phone_e164='" + phone + "'")
    notices = lambda: int(sql("SELECT count(*) FROM whats_app_message WHERE direction='outgoing' AND source='consent_confirmation'"
                              " AND phone_e164='" + phone + "'"))
    body = {'phone': phone, 'name': 'Consent Fixture', 'consent': True}

    # Simultaneous first requests for one number: the row is created once and
    # every submitter is answered (creation is ON CONFLICT DO NOTHING; the
    # claim is one conditional update).
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
        statuses = sorted(status for status, _ in executor.map(
            lambda _: request('/public/whatsapp/consent', body, token=None, method='POST'), range(4)))
    check('simultaneous first consent requests are all answered ' + str(statuses), statuses == [200, 200, 200, 200])
    check('simultaneous first consent requests create one unconfirmed row',
          sql("SELECT count(*)||','||bool_and(NOT consent) FROM whats_app_consent WHERE phone_e164='" + phone + "'") == '1,true')
    check('public consent request discloses nothing',
          json.loads(request('/public/whatsapp/consent', body, token=None, method='POST')[1])['status']['consent'] is False)

    def payload_for(sender, message_id, text, sent_at):
        sender_digits = sender[1:]
        return {'object': 'whatsapp_business_account', 'entry': [{'changes': [{'value': {
            'messaging_product': 'whatsapp',
            'contacts': [{'wa_id': sender_digits, 'profile': {'name': 'Consent Fixture'}}],
            'messages': [{'id': message_id, 'from': sender_digits, 'timestamp': str(int(sent_at)),
                          'type': 'text', 'text': {'body': text}}]}}]}]}

    def reply(message_id, text, sent_at, sender=phone):
        return request('/webhooks/whatsapp', payload_for(sender, message_id, text, sent_at), token=None, method='POST', signed=True)[0]

    # A delivered request one hour old (the fixture's own claims are undelivered).
    now = time.time()
    sql("UPDATE whats_app_consent SET note='pending_confirmation', consent=false, consented_at=NULL, revoked_at=NULL,"
        " confirmation_requested_at=to_timestamp(" + str(int(now) - 3600) + ") WHERE phone_e164='" + phone + "'")
    check('affirmative reply sent before the request cannot confirm it', reply('wamid.consent.stale', 'SI', now - 7200) == 200
          and row() == 'false,pending_confirmation,true,false')
    check('non-affirmative reply does not confirm', reply('wamid.consent.other', 'si quiero', now) == 200
          and row() == 'false,pending_confirmation,true,false')
    # The notice row shares the number with the inbound reply row, and
    # whats_app_message is unique on (phone_e164, created_at); the handler
    # stamps the notice separately so the webhook answers 200 and the notice
    # is recorded.
    check('affirmative reply activates consent, answers 200 and records the notice', reply('wamid.consent.ok', 'Sí', now) == 200
          and row() == 'true,confirmed_by_reply,true,false' and notices() == 1)
    check('redelivered confirmation changes nothing', reply('wamid.consent.ok', 'Sí', now) == 200
          and row() == 'true,confirmed_by_reply,true,false' and notices() == 1)
    check('affirmative reply from an unknown number creates no consent', reply('wamid.consent.unknown', 'SI', now, '+593990007322') == 200
          and sql("SELECT count(*) FROM whats_app_consent WHERE phone_e164='+593990007322'") == '0')

    # The collision itself, pinned as a schema fact: one message per number per
    # timestamp. The harness sql helper fails on a non-zero psql exit, so the
    # colliding insert runs inside a DO block that records the outcome.
    pin = '+593990007399'
    values = lambda second: ("('pin-in','" + pin + "','incoming','" + pin + "','2026-10-12T21:00:00.000001Z'),"
                             "('pin-out','" + pin + "','outgoing','" + pin + "'," + second + ")")
    collided = sql("DO $$ BEGIN INSERT INTO whats_app_message(external_id,sender_id,direction,phone_e164,created_at) VALUES "
                   + values("'2026-10-12T21:00:00.000001Z'") + "; PERFORM set_config('tdf.pin','no collision',false);"
                   " EXCEPTION WHEN unique_violation THEN PERFORM set_config('tdf.pin',SQLERRM,false); END $$; SELECT current_setting('tdf.pin',true)")
    check('one WhatsApp message per number per timestamp', 'index_whats_app_message_phone' in collided
          and sql("INSERT INTO whats_app_message(external_id,sender_id,direction,phone_e164,created_at) VALUES "
                  + values("'2026-10-12T21:00:00.000002Z'") + " RETURNING 1") == '1\n1')
    sql("DELETE FROM whats_app_message WHERE phone_e164='" + pin + "'")
