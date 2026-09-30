CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE TABLE party(id bigint PRIMARY KEY);
CREATE TABLE notification(id bigint PRIMARY KEY);
CREATE TABLE content_reaction_type(
 id uuid PRIMARY KEY, catalog_id uuid NOT NULL, code text UNIQUE NOT NULL,
 emoji text NOT NULL,name_es text NOT NULL,name_en text NOT NULL,
 sort_order bigint NOT NULL,active boolean NOT NULL,workflow_state_id uuid NOT NULL);
INSERT INTO content_reaction_type VALUES
 ('50900000-0000-4000-8000-000000000001',gen_random_uuid(),'fire','🔥','Fuego','Fire',0,true,gen_random_uuid()),
 ('50900000-0000-4000-8000-000000000002',gen_random_uuid(),'heart','❤️','Amor','Love',1,true,gen_random_uuid()),
 ('50900000-0000-4000-8000-000000000003',gen_random_uuid(),'clap','👏','Aplausos','Applause',2,true,gen_random_uuid());
INSERT INTO party SELECT generate_series(1,100);
