BEGIN;
DO $$
DECLARE a uuid:=gen_random_uuid(); b uuid:=gen_random_uuid(); root uuid:=gen_random_uuid();
 child uuid:=gen_random_uuid(); grandchild uuid:=gen_random_uuid(); typ uuid;
 actor bigint; n integer; actual bigint; tracked bigint;
BEGIN
 ASSERT NOT (SELECT enabled FROM interaction_runtime WHERE singleton);
 ASSERT (SELECT count(*) FROM interaction_reaction_choice)=4;
 INSERT INTO interaction_target(id,entity_kind,entity_key) VALUES(a,'club_post','1'),(b,'club_post','2');
 INSERT INTO interaction_comment(id,target_id,author_id,root_id,body) VALUES(root,a,1,root,'Parent');
 INSERT INTO interaction_comment(id,target_id,author_id,parent_id,root_id,depth,body)
 VALUES(child,a,2,root,root,1,'Reply'),(grandchild,a,3,child,root,2,'Nested reply');
 BEGIN
  INSERT INTO interaction_comment(target_id,author_id,parent_id,root_id,depth,body)
    VALUES(b,2,root,root,1,'Cross-target');
  RAISE EXCEPTION 'Cross-target reply accepted';
 EXCEPTION WHEN check_violation THEN NULL; END;
 BEGIN
  UPDATE interaction_comment SET parent_id=grandchild,root_id=grandchild,depth=3 WHERE id=root;
  RAISE EXCEPTION 'Cycle accepted';
 EXCEPTION WHEN check_violation THEN NULL; END;
 BEGIN
  INSERT INTO interaction_comment(target_id,author_id,parent_id,root_id,depth,body)
    VALUES(a,2,child,child,2,'Wrong root');
  RAISE EXCEPTION 'Wrong root accepted';
 EXCEPTION WHEN check_violation THEN NULL; END;
 UPDATE interaction_comment SET state='deleted',body='',version=version+1 WHERE id=root;
 ASSERT (SELECT count(*) FROM interaction_comment WHERE root_id=root)=3;
 ASSERT (SELECT body FROM interaction_comment WHERE id=child)='Reply';
 BEGIN
  DELETE FROM interaction_comment WHERE id=root;
  SET CONSTRAINTS ALL IMMEDIATE;
  RAISE EXCEPTION 'Parent physically deleted';
 EXCEPTION WHEN foreign_key_violation THEN NULL; END;
 SET CONSTRAINTS ALL IMMEDIATE;
 -- Deterministic generated sequence exercises absent insert, repeated desired
 -- state, changes and deletion. Check every state against authoritative rows.
 FOR n IN 1..1200 LOOP
  actor:=1+(n*17%41);
  SELECT reaction_type_id INTO typ FROM interaction_reaction_choice
    ORDER BY default_order OFFSET (n*7%4) LIMIT 1;
  IF n%5=0 THEN DELETE FROM interaction_reaction WHERE target_id=a AND actor_id=actor;
  ELSE
    INSERT INTO interaction_reaction(target_id,actor_id,reaction_type_id) VALUES(a,actor,typ)
    ON CONFLICT(target_id,actor_id) DO UPDATE SET reaction_type_id=excluded.reaction_type_id;
  END IF;
  SELECT count(*) INTO actual FROM interaction_reaction WHERE target_id=a;
  SELECT coalesce(sum(total),0) INTO tracked FROM interaction_reaction_total WHERE target_id=a;
  ASSERT actual=tracked, 'Aggregate reaction counter drift';
  ASSERT NOT EXISTS(
    SELECT 1 FROM interaction_reaction_choice c
    WHERE (SELECT count(*) FROM interaction_reaction r WHERE r.target_id=a AND r.reaction_type_id=c.reaction_type_id)
      <> coalesce((SELECT total FROM interaction_reaction_total t WHERE t.target_id=a AND t.reaction_type_id=c.reaction_type_id),0)
  ), 'Per-type reaction counter drift';
 END LOOP;
 DELETE FROM interaction_reaction WHERE target_id=a;
 ASSERT (SELECT sum(total) FROM interaction_reaction_total WHERE target_id=a)=0;
END $$;
ROLLBACK;
