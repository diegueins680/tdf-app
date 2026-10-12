"""PRIV-WHATSAPP-001 over the real backend and PostgreSQL.

The fixture backend has no WhatsApp provider, so every confirmation request is
attempted, refused and its claim released; the pending window for the reply
checks is prepared directly in the consent row. Replies arrive through the
signed Meta webhook with their own message timestamp.
"""
import concurrent.futures
import json
import time


def verify_whatsapp_consent(sql, request, check):
    phone = '+593990007321'
    digits = phone[1:]
    rows = lambda: sql("SELECT count(*)||','||coalesce(bool_and(consent)::text,'')||','||coalesce(string_agg(note,''),'')"
                       "||','||coalesce(bool_and(confirmation_requested_at IS NOT NULL)::text,'')||','||coalesce(bool_and(revoked_at IS NOT NULL)::text,'')"
                       " FROM whats_app_consent WHERE phone_e164='" + phone + "'")
    attempts = lambda: int(sql("SELECT count(*) FROM whats_app_message WHERE direction='outgoing' AND source='consent_confirmation_request'"
                               " AND phone_e164='" + phone + "'"))
    body = {'phone': phone, 'name': 'Consent Fixture', 'consent': True}

    # Concurrent first requests for one number: the row is created once and
    # every submitter is answered; exactly one confirmation message is attempted.
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
        statuses = sorted(status for status, _ in executor.map(
            lambda _: request('/public/whatsapp/consent', body, token=None, method='POST'), range(4)))
    check('concurrent first consent requests are all answered ' + str(statuses), statuses == [200, 200, 200, 200])
    check('concurrent first consent requests create one pending row', rows() == '1,false,pending_confirmation,false,false')
    # Without a provider each claimed request is refused and released at once,
    # so a later submitter may claim again: one attempt per claim, never more
    # than one per request.
    check('concurrent first consent requests attempt at most one message each', 1 <= attempts() <= 4)
    check('public consent request discloses nothing',
          json.loads(request('/public/whatsapp/consent', body, token=None, method='POST')[1])['status']['consent'] is False)

    def payload_for(sender, message_id='wamid.consent.unknown', text='SI', sent_at=None):
        sender_digits = sender[1:]
        return {'object': 'whatsapp_business_account', 'entry': [{'changes': [{'value': {
            'messaging_product': 'whatsapp',
            'contacts': [{'wa_id': sender_digits, 'profile': {'name': 'Consent Fixture'}}],
            'messages': [{'id': message_id, 'from': sender_digits, 'timestamp': str(int(sent_at if sent_at is not None else time.time())),
                          'type': 'text', 'text': {'body': text}}]}}]}]}

    def reply(message_id, text, sent_at):
        return request('/webhooks/whatsapp', payload_for(phone, message_id, text, sent_at), token=None, method='POST', signed=True)[0]

    now = time.time()
    sql("UPDATE whats_app_consent SET confirmation_requested_at = to_timestamp(" + str(int(now) - 3600) + ") WHERE phone_e164='" + phone + "'")
    check('affirmative reply sent before the request cannot confirm it', reply('wamid.consent.stale', 'SI', now - 7200) == 200
          and rows() == '1,false,pending_confirmation,true,false')
    check('non-affirmative reply does not confirm', reply('wamid.consent.other', 'si quiero', now) == 200
          and rows() == '1,false,pending_confirmation,true,false')
    notices = lambda: int(sql("SELECT count(*) FROM whats_app_message WHERE direction='outgoing' AND source='consent_confirmation'"
                              " AND phone_e164='" + phone + "'"))
    # The webhook must answer 200 and record the confirmation notice: the
    # notice row shares the number with the inbound reply row and
    # whats_app_message is unique on (phone_e164, created_at).
    check('affirmative reply after the request activates consent once and records the notice', reply('wamid.consent.ok', 'Sí', now) == 200
          and rows() == '1,true,confirmed_by_reply,false,false' and notices() == 1)
    check('redelivered confirmation changes nothing', reply('wamid.consent.ok', 'Sí', now) == 200
          and rows() == '1,true,confirmed_by_reply,false,false' and notices() == 1)
    before = attempts()
    check('active consent is not re-requested', request('/public/whatsapp/consent', body, token=None, method='POST')[0] == 200
          and attempts() == before and rows() == '1,true,confirmed_by_reply,false,false')
    check('opt-out keyword withdraws consent and clears any request', reply('wamid.consent.stop', 'STOP', now + 1) == 200
          and rows() == '1,false,Inbound WhatsApp opt-out keyword,false,true')
    check('late affirmative reply cannot revive a withdrawn request', reply('wamid.consent.late', 'SI', now + 2) == 200
          and rows() == '1,false,Inbound WhatsApp opt-out keyword,false,true')
    before = attempts()
    check('a new request after withdrawal is claimed again', request('/public/whatsapp/consent', body, token=None, method='POST')[0] == 200
          and attempts() == before + 1 and rows() == '1,false,pending_confirmation,false,true')
    sql("UPDATE whats_app_consent SET confirmation_requested_at = now() - interval '8 days' WHERE phone_e164='" + phone + "'")
    check('expired request cannot be confirmed', reply('wamid.consent.expired', 'SI', now + 3) == 200
          and rows() == '1,false,pending_confirmation,true,true')
    unknown = {**payload_for('+593990007322')}
    check('affirmative reply from an unknown number creates no consent',
          request('/webhooks/whatsapp', unknown, token=None, method='POST', signed=True)[0] == 200
          and sql("SELECT count(*) FROM whats_app_consent WHERE phone_e164='+593990007322'") == '0')
