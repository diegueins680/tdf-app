-- Restores the capture functions as installed by 2026-08-09_admin_operations_control_center.sql.
-- Events already recorded stay; the triggers stop capturing messages stored as 'incoming'.

CREATE OR REPLACE FUNCTION operations_whatsapp_inbound_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF lower(NEW.direction) NOT IN ('inbound', 'received') THEN RETURN NEW; END IF;
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
  RETURN NEW;
END;
$$;

CREATE OR REPLACE FUNCTION operations_social_inbound_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE provider_name TEXT := CASE WHEN TG_TABLE_NAME = 'instagram_message' THEN 'instagram' ELSE 'facebook' END;
BEGIN
  IF lower(NEW.direction) NOT IN ('inbound', 'received') THEN RETURN NEW; END IF;
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
  RETURN NEW;
END;
$$;
