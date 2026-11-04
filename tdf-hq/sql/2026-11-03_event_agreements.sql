-- EVT-AGREEMENT-001: private event plans and version-bound account acceptance.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='30s';

DO $$ DECLARE previous TEXT; BEGIN
 SELECT pg_get_expr(conbin,conrelid) INTO previous FROM pg_constraint
  WHERE conrelid='notification'::regclass AND conname='notification_notif_type_check';
 IF previous IS NOT NULL THEN
  ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
  EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK ((%s) OR notif_type = ''event.agreement.updated'')',previous);
 END IF;
END $$;

CREATE TABLE event_workspace_plan (
 event_id BIGINT NOT NULL REFERENCES social_event(id), revision INTEGER NOT NULL CHECK(revision>0),
 recipient_party_id BIGINT NOT NULL REFERENCES party(id), content JSONB NOT NULL,
 document_hash TEXT NOT NULL, created_by BIGINT NOT NULL REFERENCES party(id),
 created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(), PRIMARY KEY(event_id,revision)
);
CREATE TABLE event_agreement (
 id UUID PRIMARY KEY DEFAULT gen_random_uuid(), event_id BIGINT NOT NULL REFERENCES social_event(id),
 recipient_party_id BIGINT NOT NULL REFERENCES party(id), current_version INTEGER NOT NULL CHECK(current_version>0),
 state TEXT NOT NULL CHECK(state IN ('draft','sent','changes_requested','accepted','rejected','withdrawn')),
 parent_id UUID REFERENCES event_agreement(id), created_by BIGINT NOT NULL REFERENCES party(id),
 created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()
);
CREATE INDEX event_agreement_recipient ON event_agreement(recipient_party_id,event_id);
CREATE FUNCTION event_agreement_header_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
 IF TG_OP='DELETE' OR ROW(NEW.id,NEW.event_id,NEW.recipient_party_id,NEW.parent_id,NEW.created_by,NEW.created_at)
  IS DISTINCT FROM ROW(OLD.id,OLD.event_id,OLD.recipient_party_id,OLD.parent_id,OLD.created_by,OLD.created_at)
  OR (OLD.state='accepted' AND ROW(NEW.current_version,NEW.state) IS DISTINCT FROM ROW(OLD.current_version,OLD.state)) THEN
  RAISE EXCEPTION 'Agreement identity and accepted versions are immutable' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER event_agreement_header_immutable BEFORE UPDATE OR DELETE ON event_agreement
 FOR EACH ROW EXECUTE FUNCTION event_agreement_header_guard();
CREATE TABLE event_agreement_version (
 agreement_id UUID NOT NULL REFERENCES event_agreement(id), version INTEGER NOT NULL CHECK(version>0),
 content JSONB NOT NULL, document_hash TEXT NOT NULL,
 approved_by BIGINT REFERENCES party(id), approved_at TIMESTAMPTZ,
 sent_at TIMESTAMPTZ, created_by BIGINT NOT NULL REFERENCES party(id),
 created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(), PRIMARY KEY(agreement_id,version),
 CHECK((approved_by IS NULL)=(approved_at IS NULL)), CHECK(sent_at IS NULL OR approved_at IS NOT NULL)
);
ALTER TABLE event_agreement ADD CONSTRAINT event_agreement_current_version_fk
 FOREIGN KEY(id,current_version) REFERENCES event_agreement_version(agreement_id,version)
 DEFERRABLE INITIALLY DEFERRED;
CREATE TABLE event_agreement_acceptance (
 agreement_id UUID PRIMARY KEY REFERENCES event_agreement(id), version INTEGER NOT NULL,
 accepted_by BIGINT NOT NULL REFERENCES party(id), accepted_name TEXT NOT NULL,
 accepted_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(), document_hash TEXT NOT NULL,
 FOREIGN KEY(agreement_id,version) REFERENCES event_agreement_version(agreement_id,version)
);
CREATE TABLE event_workspace_action (
 id BIGSERIAL PRIMARY KEY, event_id BIGINT NOT NULL REFERENCES social_event(id),
 agreement_id UUID REFERENCES event_agreement(id), version INTEGER NOT NULL,
 actor_party_id BIGINT NOT NULL REFERENCES party(id), action TEXT NOT NULL,
 comment TEXT, created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()
);
CREATE TABLE event_workspace_command (
 event_id BIGINT NOT NULL REFERENCES social_event(id), actor_party_id BIGINT NOT NULL REFERENCES party(id),
 request_id UUID NOT NULL, request_hash TEXT NOT NULL, response JSONB NOT NULL,
 created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(), PRIMARY KEY(event_id,actor_party_id,request_id)
);

CREATE FUNCTION event_workspace_hash(value JSONB) RETURNS TEXT LANGUAGE SQL IMMUTABLE STRICT
AS $$ SELECT encode(sha256(convert_to(value::text,'UTF8')),'hex') $$;

CREATE FUNCTION event_workspace_immutable() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
 RAISE EXCEPTION 'Versioned event documents and acceptance evidence are immutable' USING ERRCODE='23514';
END $$;
CREATE TRIGGER event_plan_immutable BEFORE UPDATE OR DELETE ON event_workspace_plan
 FOR EACH ROW EXECUTE FUNCTION event_workspace_immutable();
CREATE TRIGGER event_acceptance_immutable BEFORE UPDATE OR DELETE ON event_agreement_acceptance
 FOR EACH ROW EXECUTE FUNCTION event_workspace_immutable();
CREATE TRIGGER event_action_immutable BEFORE UPDATE OR DELETE ON event_workspace_action
 FOR EACH ROW EXECUTE FUNCTION event_workspace_immutable();
CREATE FUNCTION event_agreement_version_guard() RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
 IF TG_OP='DELETE' OR ROW(NEW.agreement_id,NEW.version,NEW.content,NEW.document_hash,NEW.created_by,NEW.created_at)
  IS DISTINCT FROM ROW(OLD.agreement_id,OLD.version,OLD.content,OLD.document_hash,OLD.created_by,OLD.created_at)
  OR (OLD.approved_at IS NOT NULL AND ROW(NEW.approved_by,NEW.approved_at) IS DISTINCT FROM ROW(OLD.approved_by,OLD.approved_at))
  OR (OLD.sent_at IS NOT NULL AND NEW.sent_at IS DISTINCT FROM OLD.sent_at) THEN
  RAISE EXCEPTION 'Agreement version content and recorded approvals are immutable' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER event_agreement_version_immutable BEFORE UPDATE OR DELETE ON event_agreement_version
 FOR EACH ROW EXECUTE FUNCTION event_agreement_version_guard();

-- JSON is the document boundary, never executable markup or client-written audit evidence.
CREATE FUNCTION event_workspace_plan_error(p JSONB) RETURNS TEXT LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE item JSONB; field TEXT; amount NUMERIC;
BEGIN
 IF jsonb_typeof(p) IS DISTINCT FROM 'object' OR octet_length(p::text)>196608 OR
  p - ARRAY['title','overview','activities','budget','budgetCapCents','currency','deliverables','pending'] <> '{}'::jsonb THEN RETURN 'Revisa los campos y el tamaño del plan'; END IF;
 IF jsonb_typeof(p->'title') IS DISTINCT FROM 'string' OR length(btrim(p->>'title')) NOT BETWEEN 1 AND 200
  OR jsonb_typeof(p->'overview') IS DISTINCT FROM 'string' OR length(p->>'overview')>50000
  OR (p->>'currency') !~ '^[A-Z]{3}$' OR p->>'currency' IS NULL THEN RETURN 'Completa el título, la descripción y la moneda del plan'; END IF;
 FOREACH field IN ARRAY ARRAY['activities','budget','deliverables','pending'] LOOP
  IF jsonb_typeof(p->field) IS DISTINCT FROM 'array' OR jsonb_array_length(p->field)>200 THEN RETURN 'Revisa las secciones del plan y su tamaño'; END IF;
 END LOOP;
 FOREACH field IN ARRAY ARRAY['deliverables','pending'] LOOP
  FOR item IN SELECT value FROM jsonb_array_elements(p->field) LOOP
   IF jsonb_typeof(item)<>'string' OR length(btrim(item#>>'{}')) NOT BETWEEN 1 AND 2000 THEN RETURN 'Invalid plan text'; END IF;
  END LOOP;
 END LOOP;
 FOR item IN SELECT value FROM jsonb_array_elements(p->'activities') LOOP
  IF jsonb_typeof(item)<>'object' OR item - ARRAY['title','start','end','responsible','notes'] <> '{}'::jsonb THEN RETURN 'Revisa los campos de la actividad'; END IF;
  FOREACH field IN ARRAY ARRAY['title','start','end','responsible','notes'] LOOP
   IF jsonb_typeof(item->field) IS DISTINCT FROM 'string' OR length(item->>field)>4000 THEN RETURN 'Revisa el texto de la actividad'; END IF;
  END LOOP;
  IF length(btrim(item->>'title'))=0 THEN RETURN 'La actividad necesita un título'; END IF;
 END LOOP;
 FOR item IN SELECT value FROM jsonb_array_elements(p->'budget') LOOP
  IF jsonb_typeof(item)<>'object' OR item - ARRAY['label','amountCents','basis'] <> '{}'::jsonb
   OR jsonb_typeof(item->'label') IS DISTINCT FROM 'string' OR length(btrim(item->>'label')) NOT BETWEEN 1 AND 200
   OR jsonb_typeof(item->'basis') IS DISTINCT FROM 'string' OR length(item->>'basis')>2000 THEN RETURN 'Revisa el rubro del presupuesto'; END IF;
  IF item->'amountCents' IS DISTINCT FROM 'null'::jsonb THEN
   IF jsonb_typeof(item->'amountCents') IS DISTINCT FROM 'number' THEN RETURN 'Los importes deben expresarse en centavos enteros o quedar pendientes'; END IF;
   amount:=(item->>'amountCents')::numeric;
   IF amount<0 OR amount>9007199254740991 OR trunc(amount)<>amount THEN RETURN 'El importe está fuera del rango permitido'; END IF;
  END IF;
 END LOOP;
 IF p->'budgetCapCents' IS DISTINCT FROM 'null'::jsonb THEN
  IF jsonb_typeof(p->'budgetCapCents') IS DISTINCT FROM 'number' THEN RETURN 'El tope debe expresarse en centavos enteros o quedar pendiente'; END IF;
  amount:=(p->>'budgetCapCents')::numeric;
  IF amount<0 OR amount>9007199254740991 OR trunc(amount)<>amount THEN RETURN 'El tope está fuera del rango permitido'; END IF;
 END IF;
 RETURN NULL;
END $$;

CREATE FUNCTION event_workspace_read(actor BIGINT, ev BIGINT) RETURNS JSONB LANGUAGE plpgsql STABLE AS $$
DECLARE owner TEXT; plan event_workspace_plan; result JSONB;
BEGIN
 SELECT organizer_party_id INTO owner FROM social_event WHERE id=ev;
 IF NOT FOUND THEN RETURN jsonb_build_object('error','No se encontró el evento','status',404); END IF;
 SELECT * INTO plan FROM event_workspace_plan WHERE event_id=ev ORDER BY revision DESC LIMIT 1;
 IF owner IS DISTINCT FROM actor::text AND plan.recipient_party_id IS DISTINCT FROM actor AND NOT EXISTS
  (SELECT 1 FROM event_agreement WHERE event_id=ev AND recipient_party_id=actor AND EXISTS
    (SELECT 1 FROM event_agreement_version WHERE agreement_id=event_agreement.id AND sent_at IS NOT NULL)) THEN
  RETURN jsonb_build_object('error','No tienes acceso a los documentos de este evento','status',403);
 END IF;
 SELECT jsonb_build_object('eventId',ev::text,'owner',COALESCE(owner=actor::text,false),
  'plan',CASE WHEN plan.event_id IS NOT NULL AND (owner=actor::text OR plan.recipient_party_id=actor) THEN to_jsonb(plan) ELSE 'null'::jsonb END,
  'agreements',COALESCE((SELECT jsonb_agg(to_jsonb(a)||jsonb_build_object(
    'versions',(SELECT jsonb_agg(to_jsonb(v) ORDER BY v.version) FROM event_agreement_version v WHERE v.agreement_id=a.id AND (owner=actor::text OR v.sent_at IS NOT NULL)),
    'acceptance',(SELECT to_jsonb(ac) FROM event_agreement_acceptance ac WHERE ac.agreement_id=a.id),
    'actions',COALESCE((SELECT jsonb_agg(to_jsonb(x) ORDER BY x.id) FROM event_workspace_action x WHERE x.agreement_id=a.id AND (owner=actor::text OR EXISTS(SELECT 1 FROM event_agreement_version z WHERE z.agreement_id=a.id AND z.version=x.version AND z.sent_at IS NOT NULL))),'[]'::jsonb)) ORDER BY a.created_at)
    FROM event_agreement a WHERE a.event_id=ev AND (owner=actor::text OR (a.recipient_party_id=actor AND EXISTS(SELECT 1 FROM event_agreement_version v WHERE v.agreement_id=a.id AND v.sent_at IS NOT NULL)))),'[]'::jsonb),
  'planComments',COALESCE((SELECT jsonb_agg(to_jsonb(x) ORDER BY x.id) FROM event_workspace_action x WHERE x.event_id=ev AND x.agreement_id IS NULL AND (owner=actor::text OR plan.recipient_party_id=actor)),'[]'::jsonb)) INTO result;
 RETURN result;
END $$;

CREATE FUNCTION event_workspace_mutate(actor BIGINT, ev BIGINT, request JSONB) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE owner TEXT; action TEXT; key UUID; reqhash TEXT; previous event_workspace_command;
 plan event_workspace_plan; agreement event_agreement; version event_agreement_version;
 acceptance event_agreement_acceptance; content JSONB; err TEXT; recipient BIGINT; parent UUID;
 agreement_key UUID; expected INTEGER; next_revision INTEGER; note TEXT; result JSONB; notify BIGINT;
BEGIN
 -- All commands lock the same event before any agreement, so plan capture and acceptance serialize.
 SELECT organizer_party_id INTO owner FROM social_event WHERE id=ev FOR UPDATE;
 IF NOT FOUND THEN RETURN jsonb_build_object('error','No se encontró el evento','status',404); END IF;
 IF owner IS NULL OR owner !~ '^[1-9][0-9]{0,17}$' THEN RETURN jsonb_build_object('error','El evento necesita un organizador asignado','status',409); END IF;
 IF jsonb_typeof(request) IS DISTINCT FROM 'object' OR octet_length(request::text)>262144 OR request - ARRAY['requestId','action','agreementId','version','hash','recipientPartyId','parentId','content','comment','acceptedName','consent']<>'{}'::jsonb
  OR jsonb_typeof(request->'action') IS DISTINCT FROM 'string' OR (request->>'requestId') !~ '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$' OR request->>'requestId' IS NULL THEN
  RETURN jsonb_build_object('error','Revisa los campos de la operación','status',400);
 END IF;
 action:=request->>'action';
 IF (request - (CASE action
  WHEN 'plan.save' THEN ARRAY['action','requestId','version','recipientPartyId','content']
  WHEN 'plan.comment' THEN ARRAY['action','requestId','version','comment']
  WHEN 'agreement.create' THEN ARRAY['action','requestId','version','content','parentId']
  WHEN 'agreement.revise' THEN ARRAY['action','requestId','version','agreementId','hash','content']
  WHEN 'agreement.accept' THEN ARRAY['action','requestId','version','agreementId','hash','acceptedName','consent']
  WHEN 'agreement.request_changes' THEN ARRAY['action','requestId','version','agreementId','hash','comment']
  WHEN 'agreement.reject' THEN ARRAY['action','requestId','version','agreementId','hash','comment']
  WHEN 'agreement.comment' THEN ARRAY['action','requestId','version','agreementId','hash','comment']
  WHEN 'agreement.approve' THEN ARRAY['action','requestId','version','agreementId','hash']
  WHEN 'agreement.send' THEN ARRAY['action','requestId','version','agreementId','hash']
  WHEN 'agreement.withdraw' THEN ARRAY['action','requestId','version','agreementId','hash']
  ELSE ARRAY[]::TEXT[] END)) <> '{}'::jsonb THEN
  RETURN jsonb_build_object('error','Campos no admitidos para esta operación','status',400);
 END IF;
 key:=(request->>'requestId')::uuid; reqhash:=event_workspace_hash(request);
 SELECT * INTO plan FROM event_workspace_plan WHERE event_id=ev ORDER BY revision DESC LIMIT 1;
 IF action IN ('plan.save','plan.comment') THEN
  IF owner IS DISTINCT FROM actor::text AND (action<>'plan.comment' OR plan.recipient_party_id IS DISTINCT FROM actor) THEN RETURN jsonb_build_object('error','No tienes permiso para modificar el plan','status',403); END IF;
 ELSIF action='agreement.create' THEN
  IF owner IS DISTINCT FROM actor::text THEN RETURN jsonb_build_object('error','Solo el organizador puede proponer un acuerdo','status',403); END IF;
 ELSE
  IF request->>'agreementId' IS NULL OR (request->>'agreementId') !~ '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$' THEN RETURN jsonb_build_object('error','Identificador de acuerdo inválido','status',400); END IF;
  SELECT * INTO agreement FROM event_agreement WHERE event_id=ev AND event_agreement.id=(request->>'agreementId')::uuid FOR UPDATE;
  IF NOT FOUND THEN RETURN jsonb_build_object('error','No se encontró el acuerdo','status',404); END IF;
  IF owner IS DISTINCT FROM actor::text AND agreement.recipient_party_id IS DISTINCT FROM actor THEN RETURN jsonb_build_object('error','No tienes acceso a este acuerdo','status',403); END IF;
  SELECT * INTO version FROM event_agreement_version WHERE agreement_id=agreement.id AND event_agreement_version.version=agreement.current_version;
  IF owner IS DISTINCT FROM actor::text AND NOT EXISTS(SELECT 1 FROM event_agreement_version v WHERE v.agreement_id=agreement.id AND v.sent_at IS NOT NULL) THEN RETURN jsonb_build_object('error','No se encontró el acuerdo','status',404); END IF;
 END IF;
 -- Authorize before replay so a reassigned/revoked plan reader cannot replay old private responses.
 SELECT * INTO previous FROM event_workspace_command WHERE event_id=ev AND actor_party_id=actor AND request_id=key;
 IF FOUND THEN
  IF previous.request_hash<>reqhash THEN RETURN jsonb_build_object('error','Este intento ya se utilizó con otro contenido; recarga antes de continuar','status',409); END IF;
  RETURN previous.response;
 END IF;
 IF jsonb_typeof(request->'version') IS DISTINCT FROM 'number' OR (request->>'version') !~ '^[0-9]{1,9}$' THEN RETURN jsonb_build_object('error','Indica la versión que estás revisando','status',400); END IF;
 expected:=(request->>'version')::integer; note:=btrim(request->>'comment');
 IF request ? 'comment' AND (jsonb_typeof(request->'comment') IS DISTINCT FROM 'string' OR length(note)>4000) THEN RETURN jsonb_build_object('error','Revisa el comentario','status',400); END IF;
 IF action LIKE 'plan.%' THEN
  IF expected<>COALESCE(plan.revision,0) THEN RETURN jsonb_build_object('error','El plan cambió; recarga antes de continuar','status',409); END IF;
  next_revision:=expected+1;
  IF action='plan.save' THEN
   content:=request->'content'; err:=event_workspace_plan_error(content);
   IF err IS NOT NULL THEN RETURN jsonb_build_object('error',err,'status',400); END IF;
   IF (request->>'recipientPartyId') !~ '^[1-9][0-9]{0,17}$' OR request->>'recipientPartyId' IS NULL THEN RETURN jsonb_build_object('error','Selecciona la cuenta destinataria','status',400); END IF;
   recipient:=(request->>'recipientPartyId')::bigint;
   IF recipient::text=owner OR NOT EXISTS(SELECT 1 FROM user_credential WHERE party_id=recipient AND active) THEN RETURN jsonb_build_object('error','Selecciona una cuenta destinataria activa','status',400); END IF;
   INSERT INTO event_workspace_plan VALUES(ev,next_revision,recipient,content,event_workspace_hash(content),actor,clock_timestamp());
  ELSIF action='plan.comment' THEN
   IF length(note) NOT BETWEEN 1 AND 4000 OR note IS NULL OR plan.event_id IS NULL THEN RETURN jsonb_build_object('error','Escribe un comentario sobre un plan guardado','status',400); END IF;
   next_revision:=expected; notify:=CASE WHEN owner=actor::text THEN plan.recipient_party_id ELSE owner::bigint END;
  ELSE RETURN jsonb_build_object('error','Operación del plan desconocida','status',400);
  END IF;
  INSERT INTO event_workspace_action(event_id,version,actor_party_id,action,comment) VALUES(ev,next_revision,actor,action,note);
 ELSE
  IF action IN ('agreement.create','agreement.revise','agreement.approve','agreement.send','agreement.withdraw') AND owner IS DISTINCT FROM actor::text THEN RETURN jsonb_build_object('error','Esta operación requiere al organizador','status',403); END IF;
  IF action<>'agreement.create' AND (expected<>agreement.current_version OR request->>'hash' IS DISTINCT FROM version.document_hash) THEN RETURN jsonb_build_object('error','El acuerdo cambió; recarga antes de continuar','status',409); END IF;
  IF action IN ('agreement.create','agreement.revise') THEN
   IF action='agreement.create' AND expected<>0 THEN RETURN jsonb_build_object('error','Un acuerdo nuevo debe comenzar en la versión cero','status',400); END IF;
   IF action='agreement.revise' AND agreement.state NOT IN ('draft','changes_requested','rejected','withdrawn') THEN RETURN jsonb_build_object('error','Retira la propuesta enviada o crea un anexo del acuerdo aceptado','status',409); END IF;
   IF plan.event_id IS NULL THEN RETURN jsonb_build_object('error','Guarda el plan antes de preparar un acuerdo','status',409); END IF;
   content:=request->'content';
   IF jsonb_typeof(content) IS DISTINCT FROM 'object' OR content - ARRAY['title','body','recipientLegalName','pendingFields']<>'{}'::jsonb
    OR jsonb_typeof(content->'title') IS DISTINCT FROM 'string' OR length(btrim(content->>'title')) NOT BETWEEN 1 AND 200
    OR jsonb_typeof(content->'body') IS DISTINCT FROM 'string' OR length(btrim(content->>'body')) NOT BETWEEN 1 AND 100000
    OR jsonb_typeof(content->'recipientLegalName') IS DISTINCT FROM 'string' OR length(btrim(content->>'recipientLegalName')) NOT BETWEEN 1 AND 160
    OR jsonb_typeof(content->'pendingFields') IS DISTINCT FROM 'array' OR jsonb_array_length(content->'pendingFields')>100 THEN RETURN jsonb_build_object('error','Revisa el contenido del acuerdo','status',400); END IF;
   IF EXISTS(SELECT 1 FROM jsonb_array_elements(content->'pendingFields') p WHERE jsonb_typeof(p)<>'string' OR length(btrim(p#>>'{}')) NOT BETWEEN 1 AND 1000) THEN RETURN jsonb_build_object('error','Revisa los campos pendientes','status',400); END IF;
   content:=content||jsonb_build_object('plan',to_jsonb(plan));
   IF action='agreement.create' THEN
    recipient:=plan.recipient_party_id;
    IF request->>'parentId' IS NOT NULL THEN
     IF (request->>'parentId') !~ '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$' THEN RETURN jsonb_build_object('error','La referencia del acuerdo original es inválida','status',400); END IF;
     SELECT a.id INTO parent FROM event_agreement a WHERE a.id=(request->>'parentId')::uuid AND a.event_id=ev AND a.recipient_party_id=recipient AND a.state='accepted';
     IF NOT FOUND THEN RETURN jsonb_build_object('error','El anexo necesita un acuerdo aceptado del mismo evento y destinatario','status',409); END IF;
    END IF;
    INSERT INTO event_agreement(event_id,recipient_party_id,current_version,state,parent_id,created_by) VALUES(ev,recipient,1,'draft',parent,actor) RETURNING event_agreement.id INTO agreement_key;
    next_revision:=1;
   ELSE
    agreement_key:=agreement.id; next_revision:=agreement.current_version+1;
    IF agreement.recipient_party_id<>plan.recipient_party_id THEN RETURN jsonb_build_object('error','Cambió el destinatario del plan; crea un acuerdo nuevo','status',409); END IF;
    UPDATE event_agreement SET current_version=next_revision,state='draft' WHERE event_agreement.id=agreement_key;
   END IF;
   INSERT INTO event_agreement_version(agreement_id,version,content,document_hash,created_by) VALUES(agreement_key,next_revision,content,event_workspace_hash(content),actor);
  ELSE
   agreement_key:=agreement.id; next_revision:=agreement.current_version;
   IF action IN ('agreement.approve','agreement.send') THEN
    IF agreement.state<>'draft' OR jsonb_array_length(version.content->'pendingFields')<>0 OR jsonb_array_length(version.content#>'{plan,content,pending}')<>0
     OR (version.content->>'body') ~* '\[(COMPLETAR|PENDIENTE|POR DEFINIR)[^]]*\]'
     OR version.content#>'{plan,content,budgetCapCents}'='null'::jsonb
     OR jsonb_array_length(version.content#>'{plan,content,budget}')=0
     OR EXISTS(SELECT 1 FROM jsonb_array_elements(version.content#>'{plan,content,budget}') b WHERE b->'amountCents'='null'::jsonb)
     OR (version.content#>>'{plan,content,budgetCapCents}')::numeric IS DISTINCT FROM (SELECT sum((b->>'amountCents')::numeric) FROM jsonb_array_elements(version.content#>'{plan,content,budget}') b) THEN
     RETURN jsonb_build_object('error','Completa el alcance, los campos pendientes y el presupuesto con su tope antes de aprobar','status',409);
    END IF;
    IF action='agreement.approve' THEN
     UPDATE event_agreement_version SET approved_by=actor,approved_at=COALESCE(approved_at,clock_timestamp()) WHERE agreement_id=agreement_key AND event_agreement_version.version=next_revision;
    ELSE
     IF NOT EXISTS(SELECT 1 FROM user_credential WHERE party_id=agreement.recipient_party_id AND active) THEN RETURN jsonb_build_object('error','La cuenta destinataria está inactiva','status',409); END IF;
     IF version.approved_by IS NULL THEN RETURN jsonb_build_object('error','El organizador debe aprobar esta versión y presupuesto antes del envío','status',409); END IF;
     UPDATE event_agreement_version SET sent_at=clock_timestamp() WHERE agreement_id=agreement_key AND event_agreement_version.version=next_revision;
     UPDATE event_agreement SET state='sent' WHERE event_agreement.id=agreement_key; notify:=agreement.recipient_party_id;
    END IF;
   ELSIF action='agreement.accept' THEN
    IF actor<>agreement.recipient_party_id THEN RETURN jsonb_build_object('error','Solo el destinatario puede aceptar','status',403); END IF;
    IF request->'consent' IS DISTINCT FROM 'true'::jsonb OR jsonb_typeof(request->'acceptedName') IS DISTINCT FROM 'string' OR lower(btrim(request->>'acceptedName'))<>lower(btrim(version.content->>'recipientLegalName')) THEN RETURN jsonb_build_object('error','Confirma la aceptación y el nombre legal del destinatario','status',400); END IF;
    SELECT * INTO acceptance FROM event_agreement_acceptance WHERE agreement_id=agreement_key;
    IF FOUND THEN RETURN event_workspace_read(actor,ev); END IF;
    IF agreement.state<>'sent' OR version.approved_by IS NULL OR version.sent_at IS NULL THEN RETURN jsonb_build_object('error','Solo puedes aceptar la versión vigente aprobada y enviada','status',409); END IF;
    INSERT INTO event_agreement_acceptance(agreement_id,version,accepted_by,accepted_name,document_hash) VALUES(agreement_key,next_revision,actor,btrim(request->>'acceptedName'),version.document_hash);
    UPDATE event_agreement SET state='accepted' WHERE event_agreement.id=agreement_key; notify:=owner::bigint;
   ELSIF action IN ('agreement.request_changes','agreement.reject') THEN
    IF actor<>agreement.recipient_party_id THEN RETURN jsonb_build_object('error','Solo el destinatario puede responder a la propuesta','status',403); END IF;
    IF agreement.state<>'sent' THEN RETURN jsonb_build_object('error','Esta propuesta no está pendiente de respuesta','status',409); END IF;
    IF action='agreement.request_changes' AND (note IS NULL OR length(note)=0) THEN RETURN jsonb_build_object('error','Describe los cambios que solicitas','status',400); END IF;
    UPDATE event_agreement SET state=CASE action WHEN 'agreement.reject' THEN 'rejected' ELSE 'changes_requested' END WHERE event_agreement.id=agreement_key; notify:=owner::bigint;
   ELSIF action='agreement.withdraw' THEN
    IF agreement.state NOT IN ('sent','draft','changes_requested') THEN RETURN jsonb_build_object('error','Este acuerdo ya no se puede retirar','status',409); END IF;
    UPDATE event_agreement SET state='withdrawn' WHERE event_agreement.id=agreement_key; notify:=agreement.recipient_party_id;
   ELSIF action='agreement.comment' THEN
    IF note IS NULL OR length(note) NOT BETWEEN 1 AND 4000 THEN RETURN jsonb_build_object('error','Escribe un comentario','status',400); END IF;
    notify:=CASE WHEN owner=actor::text THEN agreement.recipient_party_id ELSE owner::bigint END;
   ELSE RETURN jsonb_build_object('error','Operación del acuerdo desconocida','status',400);
   END IF;
  END IF;
  IF note IS NOT NULL AND length(note)>4000 THEN RAISE EXCEPTION 'Comment too long' USING ERRCODE='22023'; END IF;
  INSERT INTO event_workspace_action(event_id,agreement_id,version,actor_party_id,action,comment) VALUES(ev,agreement_key,next_revision,actor,action,note);
 END IF;
 IF notify IS NOT NULL AND notify<>actor THEN
  INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_id,target_key,is_read)
   VALUES(notify,'event.agreement.updated','Acuerdo y plan del evento','Hay una propuesta, comentario o decisión para revisar.','event_agreement',ev,COALESCE(agreement_key::text,'plan'),false);
 END IF;
 result:=event_workspace_read(actor,ev);
 INSERT INTO event_workspace_command(event_id,actor_party_id,request_id,request_hash,response) VALUES(ev,actor,key,reqhash,result);
 RETURN result;
END $$;
COMMIT;
