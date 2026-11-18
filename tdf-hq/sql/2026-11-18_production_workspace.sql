BEGIN;
SET LOCAL lock_timeout='10s';
-- Additive private workspace. Existing label notes, songs and orders retain their owners.
CREATE TABLE IF NOT EXISTS production_project (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  title text NOT NULL CHECK (length(trim(title)) BETWEEN 1 AND 240),
  format text NOT NULL CHECK (format IN ('Sencillo','EP','Álbum')),
  delivery date NOT NULL, revision bigint NOT NULL DEFAULT 0
);
CREATE TABLE IF NOT EXISTS production_member (
  project uuid NOT NULL REFERENCES production_project,
  person bigint NOT NULL REFERENCES party,
  role text NOT NULL CHECK (role IN ('producer','engineer','client')),
  PRIMARY KEY(project,person)
);
CREATE TABLE IF NOT EXISTS production_song (
  project uuid NOT NULL REFERENCES production_project,
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  title text NOT NULL CHECK (length(trim(title)) BETWEEN 1 AND 240),
  label_track_id uuid REFERENCES label_track, UNIQUE(project,id)
);
CREATE TABLE IF NOT EXISTS production_task (
  project uuid NOT NULL REFERENCES production_project,
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(), UNIQUE(project,id),
  title text NOT NULL CHECK (length(trim(title)) BETWEEN 1 AND 240),
  stage text NOT NULL CHECK (length(trim(stage)) BETWEEN 1 AND 120),
  owner bigint NOT NULL, approver bigint, due date NOT NULL,
  duration integer NOT NULL CHECK (duration BETWEEN 0 AND 365),
  status text NOT NULL DEFAULT 'Pendiente' CHECK(status IN ('Pendiente','En curso','En revisión','Cambios solicitados','Completada')),
  blockage text, booking_id bigint UNIQUE REFERENCES booking,
  FOREIGN KEY(project,owner) REFERENCES production_member(project,person),
  FOREIGN KEY(project,approver) REFERENCES production_member(project,person)
);
CREATE TABLE IF NOT EXISTS production_task_song (
  project uuid NOT NULL, task uuid NOT NULL, song uuid NOT NULL,
  PRIMARY KEY(task,song),
  FOREIGN KEY(project,task) REFERENCES production_task(project,id),
  FOREIGN KEY(project,song) REFERENCES production_song(project,id)
);
CREATE TABLE IF NOT EXISTS production_collaborator (
  project uuid NOT NULL, task uuid NOT NULL, person bigint NOT NULL,
  PRIMARY KEY(task,person),
  FOREIGN KEY(project,task) REFERENCES production_task(project,id),
  FOREIGN KEY(project,person) REFERENCES production_member(project,person)
);
CREATE TABLE IF NOT EXISTS production_dependency (
  project uuid NOT NULL, task uuid NOT NULL, needs uuid NOT NULL,
  PRIMARY KEY(task,needs), CHECK(task<>needs),
  FOREIGN KEY(project,task) REFERENCES production_task(project,id),
  FOREIGN KEY(project,needs) REFERENCES production_task(project,id)
);
CREATE TABLE IF NOT EXISTS production_version (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(), task uuid NOT NULL REFERENCES production_task,
  number integer NOT NULL CHECK(number>0), file_id uuid NOT NULL REFERENCES content_storage_upload,
  created_at timestamptz NOT NULL DEFAULT now(), UNIQUE(task,number)
);
CREATE TABLE IF NOT EXISTS production_share (
  version uuid NOT NULL REFERENCES production_version, person bigint NOT NULL REFERENCES party,
  PRIMARY KEY(version,person)
);
CREATE TABLE IF NOT EXISTS production_feedback (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(), version uuid NOT NULL REFERENCES production_version,
  actor bigint NOT NULL REFERENCES party, kind text NOT NULL CHECK(kind IN ('comment','changes','approve')),
  note text NOT NULL CHECK(length(note)<=8000 AND (kind='approve' OR length(trim(note))>0)),
  request_id uuid NOT NULL, created_at timestamptz NOT NULL DEFAULT now(), UNIQUE(actor,request_id)
);
CREATE UNIQUE INDEX IF NOT EXISTS production_one_decision ON production_feedback(version) WHERE kind<>'comment';
CREATE TABLE IF NOT EXISTS production_proposal (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(), project uuid NOT NULL REFERENCES production_project,
  source uuid REFERENCES production_task, base_revision bigint NOT NULL,
  changes jsonb NOT NULL, delivery date NOT NULL, confirmed boolean NOT NULL DEFAULT false
);

-- Identical predicate for the review history and for opening the file.
CREATE OR REPLACE FUNCTION production_visible_version(who bigint, v uuid) RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT EXISTS (SELECT FROM production_version r JOIN production_task t ON t.id=r.task
    JOIN production_member m ON m.project=t.project AND m.person=who WHERE r.id=v
    AND (m.role<>'client' OR EXISTS(SELECT FROM production_share s WHERE s.version=v AND s.person=who)))
$$;

CREATE OR REPLACE FUNCTION production_fail(code int, message text) RETURNS jsonb LANGUAGE sql IMMUTABLE AS $$
  SELECT jsonb_build_object('error',code,'message',message)
$$;

CREATE OR REPLACE FUNCTION production_create(who bigint, body jsonb) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE p production_project; song_id uuid; song jsonb; stage jsonb; following uuid; task_id uuid;
        finish date; track uuid; song_title text;
BEGIN
  IF jsonb_typeof(body->'songs') IS DISTINCT FROM 'array' OR jsonb_typeof(body->'template') IS DISTINCT FROM 'array' THEN
    RETURN production_fail(400,'Indica las canciones y las etapas.');
  END IF;
  IF jsonb_array_length(body->'songs') NOT BETWEEN 1 AND 100 OR jsonb_array_length(body->'template') NOT BETWEEN 1 AND 32 THEN
    RETURN production_fail(400,'Revisa el número de canciones y etapas.');
  END IF;
  INSERT INTO production_project(title,format,delivery) VALUES(trim(body->>'title'),body->>'format',(body->>'delivery')::date) RETURNING * INTO p;
  INSERT INTO production_member VALUES(p.id,who,'producer');
  FOR song IN SELECT value FROM jsonb_array_elements(body->'songs') LOOP
    track := (song->>'track')::uuid;
    song_title := trim(song->>'title');
    IF track IS NOT NULL THEN
      SELECT title INTO STRICT song_title FROM label_track WHERE id=track AND owner_party_id=who;
    END IF;
    INSERT INTO production_song(project,title,label_track_id) VALUES(p.id,song_title,track) RETURNING id INTO song_id;
    finish := p.delivery;
    following := NULL;
    FOR stage IN SELECT value FROM jsonb_array_elements(body->'template') WITH ORDINALITY ORDER BY ordinality DESC LOOP
      INSERT INTO production_task(project,title,stage,owner,due,duration)
        VALUES(p.id,stage->>'name',stage->>'name',who,finish,(stage->>'days')::integer) RETURNING id INTO task_id;
      INSERT INTO production_task_song VALUES(p.id,task_id,song_id);
      IF following IS NOT NULL THEN INSERT INTO production_dependency VALUES(p.id,following,task_id); END IF;
      following := task_id;
      finish := finish - (stage->>'days')::integer;
    END LOOP;
  END LOOP;
  RETURN jsonb_build_object('id',p.id);
EXCEPTION WHEN data_exception OR integrity_constraint_violation OR no_data_found THEN
  RETURN production_fail(400,'Revisa los datos del proyecto y de la plantilla.');
END $$;

-- Source is a projected finish, in calendar days. Propagate only when a successor
-- has exhausted its slack. Retain the immediate predecessor for each explanation.
CREATE OR REPLACE FUNCTION production_forecast(p uuid, source_task uuid, finish date) RETURNS jsonb LANGUAGE sql STABLE AS $$
  WITH RECURSIVE impact(id,suggested,cause) AS (
    SELECT id,finish,NULL::uuid FROM production_task WHERE project=p AND id=source_task AND finish>due AND status<>'Completada'
    UNION
    SELECT t.id,i.suggested+t.duration,i.id FROM impact i
    JOIN production_dependency d ON d.needs=i.id JOIN production_task t ON t.id=d.task
    WHERE t.status<>'Completada' AND i.suggested+t.duration>t.due
  ), worst AS (SELECT DISTINCT ON(id) * FROM impact ORDER BY id,suggested DESC,cause)
  SELECT coalesce(jsonb_agg(jsonb_build_object('task',t.id,'title',t.title,'current',t.due,'suggested',w.suggested,
    'cause',c.title,'booking',t.booking_id) ORDER BY w.suggested,t.id),'[]')
  FROM worst w JOIN production_task t USING(id) LEFT JOIN production_task c ON c.id=w.cause
$$;

-- Latest finish of each unfinished predecessor is the earliest latest-start
-- of any successor. An isolated project task is itself a delivery milestone.
CREATE OR REPLACE FUNCTION production_backward(p uuid, target date) RETURNS jsonb LANGUAGE sql STABLE AS $$
  WITH RECURSIVE dates(id,finish,successor) AS (
    SELECT t.id,target,NULL::uuid FROM production_task t WHERE t.project=p AND t.status<>'Completada'
      AND NOT EXISTS(SELECT FROM production_dependency d JOIN production_task s ON s.id=d.task
        WHERE d.needs=t.id AND s.status<>'Completada')
    UNION
    SELECT n.id,a.finish-s.duration,s.id FROM dates a JOIN production_task s ON s.id=a.id
      JOIN production_dependency d ON d.task=s.id JOIN production_task n ON n.id=d.needs WHERE n.status<>'Completada'
  ), limits AS (SELECT DISTINCT ON(id) * FROM dates ORDER BY id,finish,successor)
  SELECT coalesce(jsonb_agg(jsonb_build_object('task',t.id,'title',t.title,'current',t.due,'suggested',l.finish,
    'cause',s.title,'booking',t.booking_id) ORDER BY l.finish,t.id),'[]') FROM limits l JOIN production_task t USING(id)
    LEFT JOIN production_task s ON s.id=l.successor WHERE t.due<>l.finish
$$;

CREATE OR REPLACE FUNCTION production_command(who bigint, studio boolean, p uuid, body jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE
  project_row production_project; member_role text; op text := body->>'op';
  t production_task; v production_version; f production_feedback; proposal production_proposal;
  task_id uuid; version_id uuid;
  person_id bigint; file_id_input uuid; booking_input bigint; choice text; previous uuid;
  changes jsonb; item jsonb; note_text text := coalesce(body->>'note','');
BEGIN
  -- Every writer takes the same project lock BEFORE reading any mutable state.
  SELECT q.* INTO project_row FROM production_project q JOIN production_member m ON m.project=q.id
    WHERE q.id=p AND m.person=who FOR UPDATE OF q;
  IF NOT FOUND THEN RETURN production_fail(404,'Proyecto no disponible.'); END IF;
  SELECT role INTO member_role FROM production_member WHERE project=p AND person=who;
  task_id := (body->>'task')::uuid;
  version_id := (body->>'version')::uuid;
  IF op='feedback' THEN
    SELECT * INTO v FROM production_version WHERE id=version_id;
    SELECT * INTO t FROM production_task WHERE id=v.task AND project=p;
    IF NOT FOUND OR NOT production_visible_version(who,version_id) THEN RETURN production_fail(404,'Entrega no disponible.'); END IF;
    choice := body->>'kind';
    SELECT * INTO f FROM production_feedback WHERE actor=who AND request_id=(body->>'request')::uuid;
    IF FOUND THEN
      IF (f.version,f.kind,f.note) IS DISTINCT FROM (v.id,choice,note_text) THEN RETURN production_fail(409,'El identificador ya corresponde a otra operación.'); END IF;
      RETURN jsonb_build_object('id',f.id,'kind',f.kind);
    END IF;
    IF choice IN ('changes','approve') THEN
      IF t.approver IS DISTINCT FROM who AND (choice='approve' OR member_role<>'client') THEN
        RETURN production_fail(403,'Esta decisión corresponde al aprobador designado.');
      END IF;
      IF t.status<>'En revisión' OR EXISTS(SELECT FROM production_version WHERE task=t.id AND number>v.number)
        OR EXISTS(SELECT FROM production_feedback WHERE version=v.id AND kind<>'comment') THEN
        RETURN production_fail(409,'La revisión ya cambió. Consulta la versión y su decisión en el historial.');
      END IF;
      IF choice='approve' AND (t.blockage IS NOT NULL OR EXISTS(SELECT FROM production_dependency d JOIN production_task n ON n.id=d.needs WHERE d.task=t.id AND n.status<>'Completada')) THEN
        RETURN production_fail(409,'Resuelve los bloqueos y dependencias antes de completar la tarea.');
      END IF;
      UPDATE production_task SET status=CASE choice WHEN 'approve' THEN 'Completada' ELSE 'Cambios solicitados' END WHERE id=t.id;
      UPDATE production_project SET revision=revision+1 WHERE id=p;
    END IF;
    INSERT INTO production_feedback(version,actor,kind,note,request_id)
      VALUES(v.id,who,choice,note_text,(body->>'request')::uuid) RETURNING * INTO f;
    RETURN jsonb_build_object('id',f.id,'kind',f.kind);
  END IF;

  IF member_role='client' THEN RETURN production_fail(403,'Esta acción requiere un integrante del equipo de producción.'); END IF;
  IF op IN ('invite','task','edit','confirm','dependency','backplan') AND member_role<>'producer' THEN RETURN production_fail(403,'Solo el productor puede modificar el plan.'); END IF;
  IF op IN ('progress','block','publish','propose','edit','dependency') THEN
    SELECT * INTO t FROM production_task WHERE id=task_id AND project=p;
    IF NOT FOUND THEN RETURN production_fail(404,'Tarea no disponible.'); END IF;
  END IF;

  IF op='confirm' THEN
    SELECT * INTO proposal FROM production_proposal WHERE id=(body->>'proposal')::uuid AND project=p;
    IF NOT FOUND THEN RETURN production_fail(404,'Propuesta no disponible.'); END IF;
    IF proposal.confirmed THEN RETURN jsonb_build_object('id',proposal.id); END IF;
    IF proposal.base_revision<>project_row.revision THEN RETURN production_fail(409,'El plan cambió. Revisa las fechas y dependencias actuales y calcula otra propuesta.'); END IF;
    FOR item IN SELECT value FROM jsonb_array_elements(proposal.changes) LOOP
      IF item->>'booking' IS NOT NULL THEN
        PERFORM 1 FROM booking WHERE id=(item->>'booking')::bigint AND status NOT IN ('Cancelled','NoShow')
          AND (ends_at AT TIME ZONE 'America/Guayaquil')::date=(item->>'suggested')::date FOR SHARE;
        IF NOT FOUND THEN RETURN production_fail(409,'Verifica disponibilidad y cambia la reserva en el calendario del estudio antes de confirmar.'); END IF;
      END IF;
    END LOOP;
    UPDATE production_task a SET due=(j->>'suggested')::date FROM jsonb_array_elements(proposal.changes) j WHERE a.id=(j->>'task')::uuid;
    UPDATE production_project SET delivery=proposal.delivery WHERE id=p;
    UPDATE production_proposal SET confirmed=true WHERE id=proposal.id;
  ELSIF (body->>'revision')::bigint IS DISTINCT FROM project_row.revision THEN
    RETURN production_fail(409,'El proyecto cambió. Actualiza antes de guardar.');
  ELSIF op='invite' THEN
    SELECT party_id INTO person_id FROM user_credential WHERE username=trim(body->>'username') AND active;
    IF person_id IS NULL OR body->>'role' NOT IN ('engineer','client','producer') THEN RETURN production_fail(400,'Comprueba el usuario TDF y su rol.'); END IF;
    INSERT INTO production_member VALUES(p,person_id,body->>'role');
  ELSIF op IN ('task','edit') THEN
    IF op='edit' AND EXISTS(SELECT FROM production_version WHERE task=t.id) AND t.approver IS DISTINCT FROM (body->>'approver')::bigint THEN
      RETURN production_fail(409,'La revisión ya tiene un aprobador designado.');
    END IF;
    IF NOT EXISTS(SELECT FROM production_member WHERE project=p AND person=(body->>'owner')::bigint AND role<>'client') THEN
      RETURN production_fail(400,'Elige un responsable del equipo de producción.');
    END IF;
    booking_input := (body->>'booking')::bigint;
    IF booking_input IS NOT NULL THEN
      PERFORM 1 FROM booking WHERE id=booking_input AND (studio OR party_id=who OR engineer_party_id=who)
        AND status NOT IN ('Cancelled','NoShow') AND (ends_at AT TIME ZONE 'America/Guayaquil')::date=(body->>'due')::date FOR SHARE;
      IF NOT FOUND THEN RETURN production_fail(409,'Elige una reserva accesible y su fecha actual en el calendario del estudio.'); END IF;
    END IF;
    IF op='task' THEN
      INSERT INTO production_task(project,title,stage,owner,approver,due,duration,booking_id)
        VALUES(p,body->>'title',body->>'stage',(body->>'owner')::bigint,(body->>'approver')::bigint,(body->>'due')::date,(body->>'duration')::int,booking_input)
        RETURNING id INTO task_id;
    ELSE
      IF t.status='Completada' AND t.approver IS DISTINCT FROM (body->>'approver')::bigint THEN RETURN production_fail(409,'La tarea ya está completada.'); END IF;
      UPDATE production_task SET title=body->>'title',stage=body->>'stage',owner=(body->>'owner')::bigint,approver=(body->>'approver')::bigint,
        due=(body->>'due')::date,duration=(body->>'duration')::integer,booking_id=booking_input WHERE id=task_id;
      DELETE FROM production_task_song WHERE task=task_id;
      DELETE FROM production_collaborator WHERE task=task_id;
    END IF;
    INSERT INTO production_task_song SELECT p,task_id,value::uuid FROM jsonb_array_elements_text(body->'songs');
    INSERT INTO production_collaborator SELECT p,task_id,value::bigint FROM jsonb_array_elements_text(body->'collaborators');
    IF EXISTS(SELECT FROM production_collaborator c JOIN production_member m USING(project,person) WHERE c.task=task_id AND m.role='client') THEN
      RAISE check_violation;
    END IF;
  ELSIF op='dependency' THEN
    previous := (body->>'needs')::uuid;
    IF t.status='Completada' THEN RETURN production_fail(409,'La tarea ya está completada.'); END IF;
    IF EXISTS(WITH RECURSIVE ancestors(id) AS (SELECT previous UNION SELECT d.needs FROM production_dependency d JOIN ancestors a ON d.task=a.id)
      SELECT FROM ancestors WHERE id=t.id) THEN RETURN production_fail(409,'Esta dependencia formaría un ciclo.'); END IF;
    INSERT INTO production_dependency VALUES(p,t.id,previous) ON CONFLICT DO NOTHING;
  ELSIF op='backplan' THEN
    IF (body->>'finish')::date IS NULL THEN RETURN production_fail(400,'Indica la entrega final.'); END IF;
    changes := production_backward(p,(body->>'finish')::date);
    INSERT INTO production_proposal(project,base_revision,changes,delivery)
      VALUES(p,project_row.revision,changes,(body->>'finish')::date) RETURNING * INTO proposal;
    RETURN jsonb_build_object('id',proposal.id);
  ELSIF op='propose' THEN
    changes := production_forecast(p,t.id,(body->>'finish')::date);
    IF changes='[]' THEN RETURN production_fail(400,'Indica una fecha posterior para una tarea pendiente.'); END IF;
    INSERT INTO production_proposal(project,source,base_revision,changes,delivery)
      SELECT p,t.id,project_row.revision,changes,greatest(project_row.delivery,max((j->>'suggested')::date)) FROM jsonb_array_elements(changes) j
      RETURNING * INTO proposal;
    RETURN jsonb_build_object('id',proposal.id);
  ELSIF op IN ('block','progress','publish') THEN
    IF member_role<>'producer' AND t.owner<>who AND NOT EXISTS(SELECT FROM production_collaborator WHERE task=t.id AND person=who) THEN
      RETURN production_fail(403,'La tarea está asignada a otra persona.');
    END IF;
    IF op='block' THEN
      UPDATE production_task SET blockage=nullif(trim(note_text),'') WHERE id=t.id;
    ELSIF op='progress' THEN
      choice := body->>'status';
      IF t.status IN ('Completada','En revisión') OR choice NOT IN ('Pendiente','En curso','Completada') OR
        (choice='Completada' AND (t.approver IS NOT NULL OR t.blockage IS NOT NULL OR EXISTS(
            SELECT FROM production_dependency d JOIN production_task n ON n.id=d.needs WHERE d.task=t.id AND n.status<>'Completada'))) THEN
        RETURN production_fail(409,'La tarea requiere resolver sus dependencias, bloqueo o revisión.');
      END IF;
      UPDATE production_task SET status=choice WHERE id=t.id;
    ELSE
      IF t.approver IS NULL OR t.status='Completada' THEN RETURN production_fail(409,'Revisa el aprobador y el estado de la tarea.'); END IF;
      file_id_input := (body->>'file')::uuid;
      PERFORM 1 FROM content_storage_upload WHERE id=file_id_input AND owner_party_id=who AND visibility='private'
        AND destination='tdf' AND status='completed' AND media_type LIKE 'audio/%' FOR UPDATE;
      IF NOT FOUND THEN RETURN production_fail(400,'Selecciona un audio privado de tu almacenamiento TDF.'); END IF;
      IF EXISTS(SELECT FROM production_version WHERE task=t.id AND number>=(body->>'number')::integer) THEN RETURN production_fail(409,'Usa un número de versión posterior.'); END IF;
      INSERT INTO production_version(task,number,file_id) VALUES(t.id,(body->>'number')::integer,file_id_input) RETURNING * INTO v;
      INSERT INTO production_share SELECT v.id,person FROM production_member WHERE project=p AND person=t.approver;
      FOR person_id IN SELECT value::bigint FROM jsonb_array_elements_text(body->'recipients') LOOP
        IF NOT EXISTS(SELECT FROM production_member WHERE project=p AND person=person_id) THEN RAISE check_violation; END IF;
        INSERT INTO production_share VALUES(v.id,person_id) ON CONFLICT DO NOTHING;
      END LOOP;
      UPDATE production_task SET status='En revisión' WHERE id=t.id;
    END IF;
  ELSE RETURN production_fail(400,'Operación desconocida.');
  END IF;
  UPDATE production_project SET revision=revision+1 WHERE id=p;
  RETURN jsonb_build_object('id',coalesce(v.id,task_id,p));
EXCEPTION WHEN data_exception OR integrity_constraint_violation THEN
  RETURN production_fail(400,'Comprueba los datos, fechas, destinatarios y vínculos del proyecto.');
END $$;

CREATE OR REPLACE FUNCTION production_workspace(who bigint) RETURNS jsonb LANGUAGE sql STABLE AS $$
  WITH mine AS (SELECT p.*,m.role FROM production_project p JOIN production_member m ON m.project=p.id WHERE m.person=who),
  revisions AS (
    SELECT v.id,v.task,v.number,u.media_type,t.project,
      (NOT EXISTS(SELECT FROM production_version newer WHERE newer.task=v.task AND newer.number>v.number)) AS current,
      coalesce((SELECT jsonb_agg(jsonb_build_object('kind',f.kind,'note',f.note,'actor',a.display_name,'at',f.created_at) ORDER BY f.created_at,f.id)
        FROM production_feedback f JOIN party a ON a.id=f.actor WHERE f.version=v.id),'[]') AS feedback
    FROM production_version v JOIN production_task t ON t.id=v.task JOIN mine ON mine.id=t.project
    JOIN content_storage_upload u ON u.id=v.file_id WHERE production_visible_version(who,v.id)
  ), visible AS (
    SELECT t.*,m.role FROM production_task t JOIN mine m ON m.id=t.project
    WHERE m.role<>'client' OR EXISTS(SELECT FROM revisions r WHERE r.task=t.id)
  )
  SELECT coalesce(jsonb_agg(jsonb_build_object(
    'id',m.id,'title',m.title,'format',m.format,'delivery',m.delivery,'revision',m.revision,'role',m.role,
    'people',coalesce((SELECT jsonb_agg(jsonb_build_object('id',a.id,'name',a.display_name,'role',mbr.role) ORDER BY a.display_name,a.id)
      FROM production_member mbr JOIN party a ON a.id=mbr.person WHERE mbr.project=m.id AND (m.role<>'client' OR a.id=who
      OR EXISTS(SELECT FROM visible t WHERE t.project=m.id AND a.id IN(t.owner,t.approver)))),'[]'),
    'songs',coalesce((SELECT jsonb_agg(jsonb_build_object('id',s.id,'title',s.title) ORDER BY s.title,s.id) FROM production_song s
      WHERE s.project=m.id AND (m.role<>'client' OR EXISTS(SELECT FROM visible t JOIN production_task_song j ON j.task=t.id WHERE j.song=s.id))),'[]'),
    'tasks',coalesce((SELECT jsonb_agg(jsonb_build_object(
      'id',t.id,'title',t.title,'stage',t.stage,'owner',t.owner,'approver',t.approver,'due',t.due,'duration',t.duration,
      'status',CASE WHEN m.role='client' AND NOT EXISTS(SELECT FROM revisions r WHERE r.task=t.id AND r.current) THEN 'Historial' ELSE t.status END,
      'blockage',CASE WHEN m.role<>'client' THEN t.blockage END,
      'songs',coalesce((SELECT jsonb_agg(s.song) FROM production_task_song s WHERE s.task=t.id),'[]'),
      'collaborators',coalesce((SELECT jsonb_agg(c.person) FROM production_collaborator c WHERE c.task=t.id AND m.role<>'client'),'[]'),
      'needs',coalesce((SELECT jsonb_agg(d.needs) FROM production_dependency d WHERE d.task=t.id AND m.role<>'client'),'[]'),
      'booking',(SELECT jsonb_build_object('id',b.id,'start',b.starts_at,'end',b.ends_at,'status',b.status,
        'resources',coalesce((SELECT jsonb_agg(DISTINCT resource_id) FROM booking_resource WHERE booking_id=b.id),'[]'),
        'engineer',b.engineer_party_id) FROM booking b WHERE b.id=t.booking_id AND m.role<>'client'),
      'versions',coalesce((SELECT jsonb_agg(to_jsonb(r)-'task'-'project' ORDER BY r.number DESC) FROM revisions r WHERE r.task=t.id),'[]')
    ) ORDER BY t.due,t.title,t.id) FROM visible t WHERE t.project=m.id),'[]'),
    'proposals',coalesce((SELECT jsonb_agg(jsonb_build_object('id',pr.id,'base',pr.base_revision,'changes',pr.changes,'delivery',pr.delivery,'confirmed',pr.confirmed,'backward',pr.source IS NULL)
      ORDER BY pr.base_revision DESC,pr.id) FROM production_proposal pr WHERE pr.project=m.id AND m.role<>'client'),'[]')
  ) ORDER BY m.delivery,m.title,m.id),'[]') FROM mine m
$$;

-- Party reconciliation guards use the same canonical trigger as other raw-SQL domains.
CREATE OR REPLACE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON production_member
  FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference('person');
CREATE OR REPLACE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON production_share
  FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference('person');
CREATE OR REPLACE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON production_feedback
  FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference('actor');

-- Protect historical bytes even if an older application process is deleting files.
CREATE OR REPLACE FUNCTION production_retain_file() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS(SELECT FROM production_version WHERE file_id=OLD.id) THEN
    RAISE EXCEPTION 'File is part of a production revision' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END $$;
CREATE INDEX IF NOT EXISTS production_version_file ON production_version(file_id);
CREATE OR REPLACE TRIGGER production_version_retention BEFORE UPDATE OF status,visibility,sha256,destination ON content_storage_upload
  FOR EACH ROW WHEN (OLD.status='completed' AND (OLD.status,OLD.visibility,OLD.sha256,OLD.destination)
    IS DISTINCT FROM (NEW.status,NEW.visibility,NEW.sha256,NEW.destination))
  EXECUTE FUNCTION production_retain_file();
COMMIT;
