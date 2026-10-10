-- Read-only preview of 2026-10-07_directory_artist_listing_backfill_apply.
-- Requires 2026-10-07_directory_artist_derived_listings. Never writes.
\set ON_ERROR_STOP on
BEGIN READ ONLY;

SELECT 'artist_profiles' AS metric,
       count(*) FILTER (WHERE directory_profile_kind_is_artist(profile_kind)) AS total,
       count(*) FILTER (WHERE directory_profile_kind_is_artist(profile_kind)
                          AND profile_status = 'published' AND visibility = 'public'
                          AND moderation_status = 'allowed' AND canonical_profile_id IS NULL) AS public_total
FROM directory_profile;

SELECT 'existing_derived_listings' AS metric, count(*) AS total
FROM classified WHERE source_profile_id IS NOT NULL;

SELECT 'profiles_without_preview_image' AS metric, profile.slug, profile.profile_kind
FROM directory_profile profile
WHERE directory_profile_preview_image_url(profile.id) IS NULL
  AND profile.profile_status = 'published'
ORDER BY profile.slug;

SELECT finding.finding_kind, finding.profile_id, profile.slug, finding.classified_id, finding.detail
FROM directory_artist_listing_audit() finding
LEFT JOIN directory_profile profile ON profile.id = finding.profile_id
ORDER BY finding.finding_kind, profile.slug;

ROLLBACK;
