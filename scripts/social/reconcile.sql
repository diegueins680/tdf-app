-- Operator-only, read-only health checks. Never expose these aggregates as a
-- public social endpoint or use them as evidence of relevance/conversion.
BEGIN READ ONLY;
SELECT 'blocked pairs retaining consent/follows' AS measure,count(*) AS violations
  FROM social_v2_pair WHERE (block_a OR block_b) AND (consent_a OR consent_b OR follow_a OR follow_b)
UNION ALL
SELECT 'closed accounts retaining active edges',count(*) FROM social_v2_pair p
  WHERE (p.consent_a OR p.consent_b OR p.follow_a OR p.follow_b) AND EXISTS
    (SELECT 1 FROM social_v2_preference s WHERE s.closed AND s.party_id IN (p.party_a,p.party_b))
UNION ALL
SELECT 'orphan relationship endpoints',count(*) FROM social_v2_pair p
  LEFT JOIN party a ON a.id=p.party_a LEFT JOIN party b ON b.id=p.party_b WHERE a.id IS NULL OR b.id IS NULL
UNION ALL
SELECT 'publication order above counter',count(*) FROM social_v2_publication p
  WHERE p.position>(SELECT publication_position FROM social_v2_runtime);
SELECT count(*) AS pending_publications FROM fan_club_post p
  WHERE p.parent_id IS NULL AND NOT EXISTS(SELECT 1 FROM social_v2_publication s WHERE s.post_id=p.id);
SELECT count(*) AS connected_pair_stock FROM social_v2_pair p
  WHERE p.consent_a AND p.consent_b AND social_v2_allowed(p.party_a,p.party_b);
-- Stock is a proxy, not a count of newly accepted or relevant relationships.
COMMIT;
