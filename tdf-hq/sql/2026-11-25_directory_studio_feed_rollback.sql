BEGIN;
DROP FUNCTION directory_edit_feed(uuid,uuid,text,boolean,bigint);
DROP FUNCTION directory_publish_feed(uuid,uuid,text,uuid[],bigint);
DROP VIEW directory_public_feed;
DROP TABLE directory_feed_image;
DROP TABLE directory_feed_post;
COMMIT;
