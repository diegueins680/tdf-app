-- Explicit product decision 2026-10-13: inbound capture is opt-in with Operations.
-- Additive successor: do not rewrite the historical direction-repair migration.
-- This guard does not approve retention or authorize production rollout.
BEGIN;

CREATE OR REPLACE FUNCTION operations_inbound_capture_enabled()
RETURNS boolean LANGUAGE plpgsql AS $$
BEGIN
  -- Same default organization/branch used by operations_record_event. SHARE
  -- locks persist through message commit, serializing capture with disablement.
  PERFORM 1
  FROM public.operations_organization organization
  JOIN public.operations_branch branch ON branch.organization_id = organization.id
  WHERE organization.id = '00000000-0000-4000-8000-000000000001'::uuid
    AND branch.id = '00000000-0000-4000-8000-000000000002'::uuid
    AND organization.operations_enabled AND branch.active
  FOR SHARE OF organization, branch;
  RETURN FOUND;
END;
$$;


CREATE OR REPLACE FUNCTION operations_whatsapp_inbound_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF lower(NEW.direction) NOT IN ('incoming', 'inbound', 'received') THEN RETURN NEW; END IF;
  BEGIN
    IF NOT operations_inbound_capture_enabled() THEN RETURN NEW; END IF;
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

-- The backfill writes its origin into the row's metadata; anything that is
-- not a JSON object carrying "backfilled": true is not a backfill.
CREATE OR REPLACE FUNCTION operations_message_is_backfill(raw_metadata TEXT)
RETURNS boolean LANGUAGE sql IMMUTABLE AS $$
  SELECT CASE
    WHEN raw_metadata IS NULL OR NOT pg_input_is_valid(raw_metadata, 'jsonb') THEN FALSE
    WHEN jsonb_typeof(raw_metadata::jsonb) <> 'object' THEN FALSE
    ELSE COALESCE((raw_metadata::jsonb -> 'backfilled') = 'true'::jsonb, FALSE)
  END;
$$;

CREATE OR REPLACE FUNCTION operations_social_inbound_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE provider_name TEXT := CASE WHEN TG_TABLE_NAME = 'instagram_message' THEN 'instagram' ELSE 'facebook' END;
BEGIN
  IF lower(NEW.direction) NOT IN ('incoming', 'inbound', 'received') THEN RETURN NEW; END IF;
  IF NEW.deleted_at IS NOT NULL OR NEW.created_at < now() - interval '1 day' THEN RETURN NEW; END IF;
  IF operations_message_is_backfill(NEW.metadata) THEN RETURN NEW; END IF;
  BEGIN
    IF NOT operations_inbound_capture_enabled() THEN RETURN NEW; END IF;
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
