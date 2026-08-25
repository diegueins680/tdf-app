#!/bin/sh
set -eu

TDF_DISTRIBUTION_CONTAINER="tdf-distribution-migration-test-$$"
TDF_DISTRIBUTION_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_DISTRIBUTION_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_DISTRIBUTION_CONTAINER" \
  -e POSTGRES_PASSWORD=distribution-migration-test \
  -e POSTGRES_DB=tdf_distribution_migration_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_DISTRIBUTION_CONTAINER" pg_isready -U postgres -d tdf_distribution_migration_test >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Distribution migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done
sleep 5

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_DISTRIBUTION_CONTAINER" \
    psql -q -v ON_ERROR_STOP=1 -U postgres -d tdf_distribution_migration_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_DISTRIBUTION_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_distribution_migration_test < "$1" >/dev/null
}

psql_exec -c 'CREATE EXTENSION IF NOT EXISTS pgcrypto;' >/dev/null
apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-02_ddex_catalog_core.sql"
apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-13_unified_checkout_core.sql"
apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-25_commerce_trigger_row_binding_compatibility.sql"
apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-13_distribution_accounting_core.sql"
apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-25_distribution_trigger_row_binding_compatibility.sql"
apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-25_distribution_trigger_row_binding_compatibility.sql"

schema_count=$(psql_exec -Atc "SELECT count(*) FROM information_schema.tables WHERE table_schema='public' AND table_name IN (
  'distribution_product_version','distribution_release_version','distribution_rights_declaration',
  'distribution_split_allocation','distribution_version_asset','distribution_submission',
  'distribution_partner_profile','distribution_package','distribution_delivery_attempt',
  'distribution_status_evidence','distribution_recipient_status','distribution_usage_report',
  'distribution_usage_line','royalty_statement','royalty_allocation_event',
  'distribution_beneficiary_payout_profile','distribution_payout');")
test "$schema_count" = "17"

apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-25_distribution_trigger_row_binding_compatibility_rollback.sql"
legacy_binding_count=$(psql_exec -Atc "SELECT count(*) FROM pg_proc WHERE oid IN (
  'distribution_validate_submission_gate()'::regprocedure,
  'distribution_validate_delivery()'::regprocedure,
  'distribution_validate_status_evidence()'::regprocedure,
  'distribution_validate_recipient_status()'::regprocedure,
  'distribution_validate_payout_gate()'::regprocedure
) AND strpos(pg_get_functiondef(oid), '%ROWTYPE') > 0;")
test "$legacy_binding_count" = "5"
apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-13_distribution_accounting_core_rollback.sql"
apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-13_distribution_accounting_core.sql"
apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-25_distribution_trigger_row_binding_compatibility.sql"

safe_binding_count=$(psql_exec -Atc "SELECT count(*) FROM pg_proc WHERE oid IN (
  'distribution_validate_submission_gate()'::regprocedure,
  'distribution_validate_delivery()'::regprocedure,
  'distribution_validate_status_evidence()'::regprocedure,
  'distribution_validate_recipient_status()'::regprocedure,
  'distribution_validate_payout_gate()'::regprocedure
) AND strpos(pg_get_functiondef(oid), '%ROWTYPE') = 0;")
test "$safe_binding_count" = "5"

release_id=$(psql_exec -Atc "INSERT INTO catalog_release(title,release_type,status) VALUES ('Migration Test Single','Single','Draft') RETURNING id;")
version_id="40000000-0000-0000-0000-000000000001"
sandbox_profile="50000000-0000-0000-0000-000000000001"
production_profile="50000000-0000-0000-0000-000000000002"
sandbox_package="60000000-0000-0000-0000-000000000001"
production_package="60000000-0000-0000-0000-000000000002"
sandbox_delivery="70000000-0000-0000-0000-000000000001"
production_delivery="70000000-0000-0000-0000-000000000002"

psql_exec -c "
  INSERT INTO distribution_release_version(id,release_id,version,immutable_snapshot,snapshot_sha256,created_by)
  VALUES ('$version_id',$release_id,1,'{\"title\":\"Migration Test Single\"}'::jsonb,'snapshot-sha-1',10);
" >/dev/null

if psql_exec -c "UPDATE distribution_release_version SET state='validated' WHERE id='$version_id';" >/dev/null 2>&1; then
  echo "Distribution release bypassed metadata, asset, identifier, rights, split, and terms gates" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO distribution_rights_declaration(
    release_version_id,rights_scope,territory_scope,ownership_basis,term_starts_on,warranty_version,declared_by
  ) VALUES ('$version_id','master',ARRAY['Worldwide'],'owned','2026-08-13','rights-v1',10);
  INSERT INTO distribution_split_allocation(
    release_version_id,rights_scope,participant_party_id,basis_points,acceptance_version,accepted_at
  ) VALUES
    ('$version_id','master',10,6000,'split-v1',NOW()),
    ('$version_id','master',11,4000,'split-v1',NOW());
  UPDATE distribution_release_version SET
    metadata_valid=TRUE,identifiers_valid=TRUE,assets_valid=TRUE,rights_complete=TRUE,
    splits_locked=TRUE,terms_version='distribution-v1',terms_accepted_at=NOW()
  WHERE id='$version_id';
" >/dev/null

if psql_exec -c "UPDATE distribution_split_allocation SET basis_points=3999 WHERE release_version_id='$version_id' AND participant_party_id=11;" >/dev/null 2>&1; then
  echo "Locked royalty splits accepted a destructive mutation" >&2
  exit 1
fi

psql_exec -c "
  UPDATE distribution_release_version SET state='validated' WHERE id='$version_id';
  UPDATE distribution_release_version SET state='ready_for_review' WHERE id='$version_id';
  UPDATE distribution_release_version SET state='rights_review' WHERE id='$version_id';
  UPDATE distribution_release_version SET state='payment_due' WHERE id='$version_id';
  INSERT INTO distribution_submission(
    release_version_id,commercial_gate,accepted_price_minor,currency,accepted_royalty_share_bps,
    accepted_terms_version,waiver_reason,waiver_approved_by,waiver_approved_at
  ) VALUES ('$version_id','waived',0,'USD',1000,'distribution-v1','TDF-owned sandbox catalog pilot',20,NOW());
  UPDATE distribution_release_version SET state='paid' WHERE id='$version_id';
  UPDATE distribution_release_version SET state='scheduled' WHERE id='$version_id';
" >/dev/null

partner_id=$(psql_exec -Atc "INSERT INTO ddex_partner(name,dpid,allowed_versions,is_active) VALUES ('Migration Test Recipient','PADPIDA000TEST',ARRAY['4.3.2'],TRUE) RETURNING id;")
psql_exec -c "
  INSERT INTO distribution_partner_profile(
    id,partner_id,profile_key,profile_version,sender_dpid,recipient_dpid,ern_version,
    acknowledgement_version,transport,credentials_reference,rules,environment,status,verified_by,verified_at
  ) VALUES
    ('$sandbox_profile',$partner_id,'ern-audio',1,'PADPIDA000TDF','PADPIDA000TEST','4.3.2','ack-v1','sftp','secret-manager://sandbox/ddex','{}','sandbox','verified',20,NOW()),
    ('$production_profile',$partner_id,'ern-audio',1,'PADPIDA000TDF','PADPIDA000TEST','4.3.2','ack-v1','sftp','secret-manager://production/ddex','{}','production','verified',20,NOW());
  INSERT INTO distribution_package(
    id,release_version_id,partner_profile_id,message_id,package_version,xml_private_uri,
    manifest_private_uri,xml_sha256,manifest_sha256,asset_checksums,validation_evidence
  ) VALUES
    ('$sandbox_package','$version_id','$sandbox_profile','MSG-SANDBOX-1',1,'private://sandbox/message.xml','private://sandbox/manifest.json','xml-sha-sandbox','manifest-sha-sandbox','{}','{\"profile\":\"sandbox\"}'),
    ('$production_package','$version_id','$production_profile','MSG-PRODUCTION-1',1,'private://production/message.xml','private://production/manifest.json','xml-sha-production','manifest-sha-production','{}','{\"profile\":\"production\"}');
  UPDATE distribution_release_version SET state='package_generated' WHERE id='$version_id';
" >/dev/null

if psql_exec -c "UPDATE distribution_package SET xml_sha256='tampered' WHERE id='$sandbox_package';" >/dev/null 2>&1; then
  echo "Immutable DDEX package accepted a mutation" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO distribution_delivery_attempt(
    id,release_version_id,partner_profile_id,package_id,operation,environment,state,transport_reference,sent_at
  ) VALUES ('$sandbox_delivery','$version_id','$sandbox_profile','$sandbox_package','new_release','sandbox','sent','sandbox-transport-1',NOW());
" >/dev/null

if psql_exec -c "INSERT INTO distribution_delivery_attempt(
  id,release_version_id,partner_profile_id,package_id,operation,environment,state,transport_reference,sent_at
) VALUES ('$production_delivery','$version_id','$production_profile','$production_package','new_release','production','sent','production-transport-1',NOW());" >/dev/null 2>&1; then
  echo "Production delivery ran while its feature gate was disabled" >&2
  exit 1
fi

psql_exec -c "UPDATE revenue_feature_flag SET enabled=TRUE, reason='migration constraint test only' WHERE flag_key='distribution.ern_delivery' AND environment='production';
  INSERT INTO distribution_delivery_attempt(
    id,release_version_id,partner_profile_id,package_id,operation,environment,state,transport_reference,sent_at
  ) VALUES ('$production_delivery','$version_id','$production_profile','$production_package','new_release','production','sent','production-transport-1',NOW());" >/dev/null

if psql_exec -c "INSERT INTO distribution_status_evidence(
  delivery_attempt_id,recipient_event_id,evidence_kind,status,payload_sha256,occurred_at
) VALUES ('$production_delivery','mock-event-1','mock','acknowledged','mock-sha',NOW());" >/dev/null 2>&1; then
  echo "Mock evidence transitioned a production distribution record" >&2
  exit 1
fi

evidence_id=$(psql_exec -Atc "INSERT INTO distribution_status_evidence(
  delivery_attempt_id,recipient_event_id,evidence_kind,status,payload_sha256,occurred_at
) VALUES ('$sandbox_delivery','sandbox-event-1','sandbox','acknowledged','sandbox-evidence-sha',NOW()) RETURNING id;")
psql_exec -c "INSERT INTO distribution_recipient_status(
  release_version_id,partner_profile_id,current_status,evidence_id
) VALUES ('$version_id','$sandbox_profile','acknowledged','$evidence_id');" >/dev/null

report_id=$(psql_exec -Atc "INSERT INTO distribution_usage_report(
  partner_profile_id,report_reference,report_family,report_version,period_start,period_end,currency,private_uri,sha256,status
) VALUES ('$sandbox_profile','DSR-2026-07','DSR','1.4','2026-07-01','2026-07-31','USD','private://sandbox/dsr.tsv','dsr-sha-1','ingested') RETURNING id;")
usage_id=$(psql_exec -Atc "INSERT INTO distribution_usage_line(
  report_id,source_line_key,release_id,dsp,territory,usage_type,commercial_model,usage_count,gross_minor,partner_deduction_minor,net_minor,normalized_data
) VALUES ('$report_id','line-1',$release_id,'Migration DSP','EC','stream','subscription',100,1000,100,900,'{}') RETURNING id;")
statement_id=$(psql_exec -Atc "INSERT INTO royalty_statement(
  beneficiary_party_id,period_start,period_end,currency,state,gross_minor,deductions_minor,net_minor,issued_at
) VALUES (10,'2026-07-01','2026-07-31','USD','payable',900,90,810,NOW()) RETURNING id;")
allocation_id=$(psql_exec -Atc "INSERT INTO royalty_allocation_event(
  statement_id,usage_line_id,event_kind,amount_minor,currency,source_reference
) VALUES ('$statement_id','$usage_id','participant_share',810,'USD','line-1:party-10') RETURNING id;")

if psql_exec -c "UPDATE distribution_usage_line SET gross_minor=9999 WHERE id='$usage_id';" >/dev/null 2>&1; then
  echo "Normalized usage evidence accepted a destructive correction" >&2
  exit 1
fi
if psql_exec -c "DELETE FROM royalty_allocation_event WHERE id='$allocation_id';" >/dev/null 2>&1; then
  echo "Royalty allocation history accepted destructive deletion" >&2
  exit 1
fi

psql_exec -c "INSERT INTO distribution_beneficiary_payout_profile(
  beneficiary_party_id,kyc_status,tax_status,payout_account_status,encrypted_account_reference
) VALUES (10,'verified','verified','verified','vault://beneficiary/10');" >/dev/null

if psql_exec -c "INSERT INTO distribution_payout(
  statement_id,beneficiary_party_id,environment,method,status,amount_minor,currency,idempotency_key,requested_by,approved_by,approved_at
) VALUES ('$statement_id',10,'production','bank_transfer_manual','approved',810,'USD','payout-bad-separation',20,20,NOW());" >/dev/null 2>&1; then
  echo "Payout accepted the requester as its own approver" >&2
  exit 1
fi

payout_id=$(psql_exec -Atc "INSERT INTO distribution_payout(
  statement_id,beneficiary_party_id,environment,method,status,amount_minor,currency,idempotency_key,requested_by,approved_by,approved_at
) VALUES ('$statement_id',10,'production','bank_transfer_manual','approved',810,'USD','payout-1',20,21,NOW()) RETURNING id;")
if psql_exec -c "UPDATE distribution_payout SET status='processing' WHERE id='$payout_id';" >/dev/null 2>&1; then
  echo "Production payout ran without explicit production authorization" >&2
  exit 1
fi

if apply_file "$TDF_DISTRIBUTION_ROOT/tdf-hq/sql/2026-08-13_distribution_accounting_core_rollback.sql" 2>/dev/null; then
  echo "Rollback removed distribution or royalty tables after evidence existed" >&2
  exit 1
fi

echo "Distribution migration passed immutable checksum restoration, compatibility rollback/reapply, lifecycle, split, package, evidence, royalty, separation-of-duty, and payout-gate checks."
