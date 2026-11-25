-- Run in a disposable database before the feature migration and assertions.
CREATE TABLE directory_profile(id uuid PRIMARY KEY,profile_kind text);
CREATE TABLE directory_public_profile(id uuid PRIMARY KEY);
CREATE TABLE directory_profile_manager(profile_id uuid,account_party_id bigint,active boolean,can_edit boolean,can_publish boolean);
CREATE TABLE content_storage_upload(id uuid PRIMARY KEY,owner_party_id bigint,destination text,status text,visibility text,byte_size bigint,media_type text);
