\set ON_ERROR_STOP on

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '1s';

WITH source AS (
  SELECT 'ddex_validation_run'::text source_table,id::bigint source_record_id,
    'result'::text source_column,result::text original_value,result_id current_id,
    'ddex-validation-results'::text catalog_code,
    CASE regexp_replace(lower(COALESCE(result::text,'')),'[^a-z]','','g')
      WHEN 'resultsuccess' THEN 'success' WHEN 'success' THEN 'success' WHEN 'valid' THEN 'success'
      WHEN 'resultfailure' THEN 'failure' WHEN 'failure' THEN 'failure' WHEN 'invalid' THEN 'failure'
      WHEN 'resultwarning' THEN 'warning' WHEN 'warning' THEN 'warning' WHEN 'warnings' THEN 'warning'
    END normalized_code
  FROM ddex_validation_run
  WHERE result IS NOT NULL OR (finished_at IS NOT NULL AND result_id IS NULL)
  UNION ALL
  SELECT 'ddex_validation_issue',id,'severity',severity::text,severity_id,
    'ddex-validation-severities',
    CASE regexp_replace(lower(COALESCE(severity::text,'')),'[^a-z]','','g')
      WHEN 'severityerror' THEN 'error' WHEN 'error' THEN 'error'
      WHEN 'severitywarning' THEN 'warning' WHEN 'warning' THEN 'warning'
      WHEN 'severityinfo' THEN 'info' WHEN 'info' THEN 'info' WHEN 'information' THEN 'info'
    END
  FROM ddex_validation_issue WHERE severity IS NOT NULL OR severity_id IS NULL
  UNION ALL
  SELECT 'ddex_validation_issue',id,'layer',layer::text,layer_id,
    'ddex-validation-layers',
    CASE regexp_replace(lower(COALESCE(layer::text,'')),'[^a-z]','','g')
      WHEN 'layerxml' THEN 'xml' WHEN 'xml' THEN 'xml'
      WHEN 'layerxsd' THEN 'xsd' WHEN 'xsd' THEN 'xsd'
      WHEN 'layeravs' THEN 'avs' WHEN 'avs' THEN 'avs'
      WHEN 'layerbusiness' THEN 'business' WHEN 'business' THEN 'business'
    END
  FROM ddex_validation_issue WHERE layer IS NOT NULL OR layer_id IS NULL
  UNION ALL
  SELECT 'ddex_export',id,'validation_result',validation_result,validation_result_id,
    'ddex-validation-results',
    CASE regexp_replace(lower(COALESCE(validation_result,'')),'[^a-z]','','g')
      WHEN 'resultsuccess' THEN 'success' WHEN 'success' THEN 'success' WHEN 'valid' THEN 'success'
      WHEN 'resultfailure' THEN 'failure' WHEN 'failure' THEN 'failure' WHEN 'invalid' THEN 'failure'
      WHEN 'resultwarning' THEN 'warning' WHEN 'warning' THEN 'warning' WHEN 'warnings' THEN 'warning'
    END
  FROM ddex_export WHERE validation_result IS NOT NULL
), resolved AS (
  SELECT source.*,candidate.candidate_count,candidate.target_id
  FROM source
  LEFT JOIN LATERAL (
    SELECT count(*) candidate_count,(array_agg(item.id ORDER BY item.id))[1] target_id
    FROM (
      SELECT id FROM ddex_validation_result
        WHERE source.catalog_code='ddex-validation-results' AND active AND code=source.normalized_code
      UNION ALL
      SELECT id FROM ddex_validation_severity
        WHERE source.catalog_code='ddex-validation-severities' AND active AND code=source.normalized_code
      UNION ALL
      SELECT id FROM ddex_validation_layer
        WHERE source.catalog_code='ddex-validation-layers' AND active AND code=source.normalized_code
    ) item
  ) candidate ON TRUE
)
SELECT jsonb_build_object(
  'report','ddex-validation-reference-cutover','sourceRows',count(*),
  'mapped',count(*) FILTER (WHERE candidate_count=1 AND
    (current_id IS NULL OR current_id=target_id)),
  'unresolved',count(*) FILTER (WHERE candidate_count=0),
  'ambiguous',count(*) FILTER (WHERE candidate_count>1),
  'conflicts',count(*) FILTER (WHERE current_id IS NOT NULL AND current_id<>target_id),
  'rows',COALESCE(jsonb_agg(jsonb_build_object(
    'table',source_table,'id',source_record_id,'column',source_column,
    'originalValue',original_value,'normalizedCode',normalized_code,
    'catalogCode',catalog_code,'targetId',target_id,'candidates',candidate_count
  ) ORDER BY source_table,source_record_id,source_column),'[]'::jsonb)
) FROM resolved;

SELECT jsonb_build_object(
  'report','ddex-validation-reference-registry',
  'results',(SELECT count(*) FROM ddex_validation_result WHERE active),
  'severities',(SELECT count(*) FROM ddex_validation_severity WHERE active),
  'layers',(SELECT count(*) FROM ddex_validation_layer WHERE active)
);

ROLLBACK;
