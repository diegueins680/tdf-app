\set ON_ERROR_STOP on

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '1s';

WITH source AS (
  SELECT document.*,
    upper(regexp_replace(btrim(COALESCE(document.family,'')), '^Family', '', 'i')) AS family_code,
    CASE regexp_replace(lower(btrim(COALESCE(document.version,''))), '[^0-9]', '', 'g')
      WHEN '432' THEN '4.3.2' WHEN '21' THEN '2.1' WHEN '11' THEN '1.1'
      WHEN '14' THEN '1.4' ELSE btrim(document.version) END AS version_code,
    CASE lower(regexp_replace(btrim(COALESCE(document.status,'')), '[^a-zA-Z0-9]+', '', 'g'))
      WHEN 'statusreceived' THEN 'received' WHEN 'received' THEN 'received'
      WHEN 'statusquarantined' THEN 'quarantined' WHEN 'quarantined' THEN 'quarantined'
      WHEN 'statusqueued' THEN 'queued' WHEN 'queued' THEN 'queued'
      WHEN 'statusvalidating' THEN 'validating' WHEN 'validating' THEN 'validating'
      WHEN 'statusinvalid' THEN 'invalid' WHEN 'invalid' THEN 'invalid'
      WHEN 'statusvalid' THEN 'valid' WHEN 'valid' THEN 'valid'
      WHEN 'statusmappingrequired' THEN 'mapping_required' WHEN 'mappingrequired' THEN 'mapping_required'
      WHEN 'statusreadytoimport' THEN 'ready_to_import' WHEN 'readytoimport' THEN 'ready_to_import'
      WHEN 'statusimporting' THEN 'importing' WHEN 'importing' THEN 'importing'
      WHEN 'statusimported' THEN 'imported' WHEN 'imported' THEN 'imported'
      WHEN 'statusimportfailed' THEN 'import_failed' WHEN 'importfailed' THEN 'import_failed'
      WHEN 'statussuperseded' THEN 'superseded' WHEN 'superseded' THEN 'superseded'
      ELSE NULL END AS state_code
  FROM ddex_document document
  WHERE document.family IS NOT NULL OR document.version IS NOT NULL
     OR document.message_type IS NOT NULL OR document.status IS NOT NULL
     OR document.standard_version_id IS NULL OR document.workflow_state_id IS NULL
), resolved AS (
  SELECT source.*,
    standard_match.candidate_count AS standard_candidates,
    standard_match.target_id AS target_standard_version_id,
    message_match.candidate_count AS message_candidates,
    message_match.target_id AS target_message_type_id,
    state_match.candidate_count AS state_candidates,
    state_match.target_id AS target_workflow_state_id
  FROM source
  LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count, (array_agg(standard.id ORDER BY standard.id))[1] AS target_id
    FROM ddex_standard_version standard
    JOIN ddex_standard_support support ON support.standard_version_id=standard.id
      AND support.deployment_code='default' AND support.active AND support.detection_enabled
    WHERE standard.active AND (
      (source.standard_version_id IS NOT NULL AND standard.id=source.standard_version_id)
      OR (source.standard_version_id IS NULL AND standard.standard_code=source.family_code
          AND standard.version_code=source.version_code)
    )
  ) standard_match ON TRUE
  LEFT JOIN LATERAL (
    SELECT CASE WHEN source.message_type IS NULL AND source.message_type_id IS NULL THEN 0 ELSE count(*) END AS candidate_count,
      (array_agg(message.id ORDER BY message.id))[1] AS target_id
    FROM ddex_message_type message
    WHERE message.active AND message.runtime_supported
      AND message.standard_version_id=standard_match.target_id
      AND ((source.message_type_id IS NOT NULL AND message.id=source.message_type_id)
        OR (source.message_type_id IS NULL AND lower(regexp_replace(message.code,'[^a-zA-Z0-9]+','','g'))
          = lower(regexp_replace(COALESCE(source.message_type,''),'[^a-zA-Z0-9]+','','g'))))
  ) message_match ON TRUE
  LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count, (array_agg(state.id ORDER BY state.id))[1] AS target_id
    FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id
    WHERE workflow.code='ddex-document-lifecycle' AND workflow.active AND state.active
      AND ((source.workflow_state_id IS NOT NULL AND state.id=source.workflow_state_id)
        OR (source.workflow_state_id IS NULL AND state.code=source.state_code))
  ) state_match ON TRUE
)
SELECT jsonb_build_object(
  'report','ddex-document-cutover', 'sourceRows',count(*),
  'mapped',count(*) FILTER (WHERE standard_candidates=1 AND state_candidates=1
    AND (message_type IS NULL AND message_type_id IS NULL OR message_candidates=1)
    AND (standard_version_id IS NULL OR standard_version_id=target_standard_version_id)
    AND (message_type_id IS NULL OR message_type_id=target_message_type_id)
    AND (workflow_state_id IS NULL OR workflow_state_id=target_workflow_state_id)),
  'unresolved',count(*) FILTER (WHERE standard_candidates=0 OR state_candidates=0
    OR (message_type IS NOT NULL OR message_type_id IS NOT NULL) AND message_candidates=0),
  'ambiguous',count(*) FILTER (WHERE standard_candidates>1 OR state_candidates>1 OR message_candidates>1),
  'conflicts',count(*) FILTER (WHERE (standard_version_id IS NOT NULL AND standard_version_id<>target_standard_version_id)
    OR (message_type_id IS NOT NULL AND message_type_id<>target_message_type_id)
    OR (workflow_state_id IS NOT NULL AND workflow_state_id<>target_workflow_state_id)),
  'rows',COALESCE(jsonb_agg(jsonb_build_object('id',id,'family',family,'version',version,
    'status',status,'messageType',message_type,'targetStandardVersionId',target_standard_version_id,
    'targetMessageTypeId',target_message_type_id,'targetWorkflowStateId',target_workflow_state_id,
    'standardCandidates',standard_candidates,'messageCandidates',message_candidates,
    'stateCandidates',state_candidates) ORDER BY id),'[]'::jsonb)
) FROM resolved;

WITH source AS (
  SELECT export.*, CASE regexp_replace(lower(btrim(COALESCE(export.ern_version,''))), '[^0-9]', '', 'g')
    WHEN '432' THEN '4.3.2' ELSE btrim(export.ern_version) END AS version_code
  FROM ddex_export export WHERE export.ern_version IS NOT NULL OR export.standard_version_id IS NULL
), resolved AS (
  SELECT source.*, match.candidate_count, match.target_id,
    EXISTS (SELECT 1 FROM ddex_partner_standard_version membership
      JOIN ddex_partner partner ON partner.id=membership.partner_id AND partner.is_active
      WHERE membership.partner_id=source.partner_id
        AND membership.standard_version_id=match.target_id AND membership.active)
    OR EXISTS (
      SELECT 1 FROM ddex_partner partner
      CROSS JOIN LATERAL jsonb_array_elements_text(
        COALESCE(to_jsonb(partner)->'allowed_versions','[]'::jsonb)
      ) legacy(legacy_version)
      WHERE partner.id=source.partner_id AND partner.is_active
        AND regexp_replace(lower(legacy.legacy_version),'[^0-9]','','g')
          =regexp_replace(lower(source.version_code),'[^0-9]','','g')
    ) AS partner_policy_resolves
  FROM source LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count, (array_agg(standard.id ORDER BY standard.id))[1] AS target_id
    FROM ddex_standard_version standard JOIN ddex_standard_support support
      ON support.standard_version_id=standard.id AND support.deployment_code='default'
      AND support.active AND support.export_enabled
    WHERE standard.active AND standard.standard_code='ERN' AND (
      (source.standard_version_id IS NOT NULL AND standard.id=source.standard_version_id)
      OR (source.standard_version_id IS NULL AND standard.version_code=source.version_code))
  ) match ON TRUE
)
SELECT jsonb_build_object('report','ddex-export-cutover','sourceRows',count(*),
  'mapped',count(*) FILTER (WHERE candidate_count=1 AND partner_policy_resolves
    AND (standard_version_id IS NULL OR standard_version_id=target_id)),
  'unresolved',count(*) FILTER (WHERE candidate_count=0),'ambiguous',count(*) FILTER (WHERE candidate_count>1),
  'conflicts',count(*) FILTER (WHERE (standard_version_id IS NOT NULL AND standard_version_id<>target_id)
    OR NOT partner_policy_resolves),
  'rows',COALESCE(jsonb_agg(jsonb_build_object('id',id,'ernVersion',ern_version,
    'targetStandardVersionId',target_id,'candidateCount',candidate_count,
    'partnerPolicyResolves',partner_policy_resolves) ORDER BY id),'[]'::jsonb)
) FROM resolved;

WITH source AS (
  SELECT partner.id, partner.name, value.legacy_version, value.ordinality
  FROM ddex_partner partner
  CROSS JOIN LATERAL jsonb_array_elements_text(
    COALESCE(to_jsonb(partner)->'allowed_versions','[]'::jsonb)
  ) WITH ORDINALITY value(legacy_version, ordinality)
  UNION ALL
  SELECT partner.id, partner.name, NULL, 1
  FROM ddex_partner partner
  WHERE partner.is_active
    AND jsonb_array_length(COALESCE(to_jsonb(partner)->'allowed_versions','[]'::jsonb))=0
    AND NOT EXISTS (SELECT 1 FROM ddex_partner_standard_version membership
      WHERE membership.partner_id=partner.id AND membership.active)
), resolved AS (
  SELECT source.*, match.candidate_count, match.target_id
  FROM source LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count, (array_agg(standard.id ORDER BY standard.id))[1] AS target_id
    FROM ddex_standard_version standard JOIN ddex_standard_support support
      ON support.standard_version_id=standard.id AND support.deployment_code='default'
      AND support.active AND support.detection_enabled
    WHERE standard.active AND source.legacy_version IS NOT NULL
      AND regexp_replace(lower(standard.version_code),'[^0-9]','','g')
        = regexp_replace(lower(source.legacy_version),'[^0-9]','','g')
  ) match ON TRUE
)
SELECT jsonb_build_object('report','ddex-partner-version-cutover','sourceRows',count(*),
  'mapped',count(*) FILTER (WHERE candidate_count=1),'unresolved',count(*) FILTER (WHERE candidate_count=0),
  'ambiguous',count(*) FILTER (WHERE candidate_count>1),
  'rows',COALESCE(jsonb_agg(jsonb_build_object('partnerId',id,'partner',name,
    'legacyVersion',legacy_version,'targetStandardVersionId',target_id,
    'candidateCount',candidate_count) ORDER BY id,ordinality),'[]'::jsonb)
) FROM resolved;

SELECT jsonb_build_object(
  'report','ddex-governed-registry',
  'standards',(SELECT count(*) FROM ddex_standard_version WHERE active),
  'runtimeMessageTypes',(SELECT count(*) FROM ddex_message_type WHERE active AND runtime_supported),
  'workflowStates',(SELECT count(*) FROM workflow_state state JOIN workflow_definition workflow
    ON workflow.id=state.workflow_id WHERE workflow.code='ddex-document-lifecycle'
    AND workflow.active AND state.active),
  'legacyDocuments',(SELECT count(*) FROM ddex_document WHERE family IS NOT NULL OR version IS NOT NULL
    OR message_type IS NOT NULL OR status IS NOT NULL),
  'legacyExports',(SELECT count(*) FROM ddex_export WHERE ern_version IS NOT NULL),
  'activePartnersWithoutCanonicalPolicy',(SELECT count(*) FROM ddex_partner partner WHERE partner.is_active
    AND NOT EXISTS (SELECT 1 FROM ddex_partner_standard_version membership
      WHERE membership.partner_id=partner.id AND membership.active))
);

ROLLBACK;
