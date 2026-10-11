-- The inbound capture triggers of the Operations control center returned early
-- unless the message direction was 'inbound' or 'received'. The application
-- stores inbound WhatsApp, Instagram and Facebook messages with the direction
-- 'incoming', so no inbound message has ever produced an Operations event.
-- Both functions now accept 'incoming' as well, and messages stored before
-- this migration are not captured retroactively.
--
-- Two kinds of Instagram and Facebook rows are stored as 'incoming' without
-- being a new request, and are not captured: the record of a deletion that
-- arrived before its message, and a message imported from history (stored
-- more than a day after it was sent, which only the backfill does).
--
-- The capture now runs for every inbound message, inside the transaction that
-- stores it. Storing the message matters more than recording it for
-- Operations (an opt-out is applied right after storage), so a failure of the
-- capture is reported as a warning and the message is stored regardless.

BEGIN;

CREATE OR REPLACE FUNCTION operations_whatsapp_inbound_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF lower(NEW.direction) NOT IN ('incoming', 'inbound', 'received') THEN RETURN NEW; END IF;
  BEGIN
    PERFORM operations_record_event(
      'communication.whatsapp.received',
      CASE WHEN NEW.party_id IS NULL THEN 'uncorrelated_inbound' ELSE 'party' END,
      COALESCE(NEW.party_id::text, NEW.sender_id),
      'whatsapp:' || NEW.sender_id, 'whatsapp', 'high',
      'Mensaje de WhatsApp requiere respuesta', 'WhatsApp message needs a response',
      'Revise la conversación y responda por un canal autorizado.',
      'Review the conversation and respond through an approved channel.',
      jsonb_build_object('partyId', NEW.party_id, 'replyStatus', NEW.reply_status,
        'uncorrelatedIdentity', NEW.party_id IS NULL, 'terminal', false),
      NEW.created_at, NEW.external_id, false
    );
  EXCEPTION WHEN OTHERS THEN
    RAISE WARNING 'inbound message % was stored without an Operations event (%)', NEW.external_id, SQLSTATE;
  END;
  RETURN NEW;
END;
$$;

CREATE OR REPLACE FUNCTION operations_social_inbound_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE provider_name TEXT := CASE WHEN TG_TABLE_NAME = 'instagram_message' THEN 'instagram' ELSE 'facebook' END;
BEGIN
  IF lower(NEW.direction) NOT IN ('incoming', 'inbound', 'received') THEN RETURN NEW; END IF;
  IF NEW.deleted_at IS NOT NULL OR NEW.created_at < now() - interval '1 day' THEN RETURN NEW; END IF;
  BEGIN
    PERFORM operations_record_event(
      'communication.' || provider_name || '.received', 'uncorrelated_inbound', NEW.sender_id,
      provider_name || ':' || NEW.sender_id, provider_name, 'high',
      'Mensaje social requiere respuesta', 'Social message needs a response',
      'Correlacione la identidad si es posible y responda por el canal autorizado.',
      'Correlate the identity when possible and respond through the approved channel.',
      jsonb_build_object('provider', provider_name, 'replyStatus', NEW.reply_status,
        'uncorrelatedIdentity', true, 'terminal', false),
      NEW.created_at, NEW.external_id, false
    );
  EXCEPTION WHEN OTHERS THEN
    RAISE WARNING 'inbound message % was stored without an Operations event (%)', NEW.external_id, SQLSTATE;
  END;
  RETURN NEW;
END;
$$;

COMMIT;
