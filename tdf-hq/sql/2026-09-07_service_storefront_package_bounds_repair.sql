-- Preserve custom package bounds while reconciling the four canonical
-- multi-song storefront tiers. The original Phase 0 migration is immutable
-- because its checksum is already recorded in production.
BEGIN;

UPDATE service_storefront_package
SET max_song_count = CASE
      WHEN service_kind = 'Mastering' AND tier = 'Pro' THEN 3
      WHEN service_kind = 'Mastering' AND tier = 'Premium' THEN 5
      WHEN service_kind = 'Bundle' AND tier = 'Pro' THEN 3
      WHEN service_kind = 'Bundle' AND tier = 'Premium' THEN 5
    END
WHERE (service_kind, tier) IN (
  ('Mastering', 'Pro'),
  ('Mastering', 'Premium'),
  ('Bundle', 'Pro'),
  ('Bundle', 'Premium')
)
AND max_song_count IS DISTINCT FROM CASE
      WHEN service_kind = 'Mastering' AND tier = 'Pro' THEN 3
      WHEN service_kind = 'Mastering' AND tier = 'Premium' THEN 5
      WHEN service_kind = 'Bundle' AND tier = 'Pro' THEN 3
      WHEN service_kind = 'Bundle' AND tier = 'Premium' THEN 5
    END;

COMMIT;
