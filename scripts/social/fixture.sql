CREATE TABLE fan_club(id bigint PRIMARY KEY,artist_party_id bigint REFERENCES party(id));
CREATE TABLE fan_club_post(id bigint PRIMARY KEY,club_id bigint REFERENCES fan_club(id),
  fan_party_id bigint REFERENCES party(id),parent_id bigint,title text,content text NOT NULL,
  is_hidden boolean NOT NULL DEFAULT false,created_at timestamptz NOT NULL);
CREATE TABLE fan_follow(fan_party_id bigint REFERENCES party(id),artist_party_id bigint REFERENCES party(id),
  PRIMARY KEY(fan_party_id,artist_party_id));
CREATE TABLE fan_club_officer(club_id bigint,fan_party_id bigint);
CREATE TABLE fan_profile_genre_membership(fan_party_id bigint,genre_id text);
CREATE TABLE artist_profile_genre_membership(artist_party_id bigint,genre_id text);
INSERT INTO fan_club VALUES(1,5);
INSERT INTO fan_follow VALUES(2,5);
INSERT INTO fan_club_post VALUES
  (1,1,5,NULL,'first','oldest',false,'2026-01-01'),
  (2,1,5,NULL,'second','same timestamp',false,'2026-01-01'),
  (3,1,5,NULL,'third','newest',false,'2026-01-02'),
  (4,1,5,NULL,'hidden','must not leak',true,'2026-01-02'),
  (5,1,5,1,'reply','outside feed',false,'2026-01-02');
INSERT INTO fan_profile_genre_membership VALUES(2,'rock');
INSERT INTO artist_profile_genre_membership VALUES(5,'rock');
