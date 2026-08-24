{-# LANGUAGE OverloadedStrings #-}

module TDF.Catalog.Seed
  ( seedCatalogFoundation
  , validateCatalogRuntimeRegistries
  ) where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistValue (..))
import Database.Persist.Sql (Single (..), SqlPersistT, rawExecute, rawSql)
import TDF.Catalog.CountryReferenceSeed
  ( CountryReferenceSeed (..)
  , countryReferenceSeeds
  , countryReferenceSnapshotDate
  , countryReferenceSourceVersion
  )
import TDF.SocialEventLifecycle
  ( recognizedSocialEventCapabilityCodes
  , recognizedSocialEventStateCodes
  , socialEventWorkflowCode
  )

-- Idempotent foundation data. Governed datasets are generated from reviewed,
-- versioned snapshots; runtime code consumes the persisted rows, never this
-- bootstrap representation directly.
seedCatalogFoundation :: SqlPersistT IO ()
seedCatalogFoundation = do
  seedWorkflows
  seedSecurityRegistry
  seedCatalogDefinitions
  seedInternationalFoundation
  seedDdexFoundation
  seedDomainFoundation
  seedRadioAutoStopOptions
  seedAppearanceModes
  seedFeedbackCatalogs
  seedRadioStreams
  seedContentTypes
  seedAuthoredContents

  -- Runtime validation is deliberately owned by App.Boot after this seed
  -- transaction has committed. During a coordinated legacy cutover the
  -- persisted registry must exist before the bounded backfill can resolve old
  -- string values; the application still refuses to serve until the explicit
  -- post-migration validation succeeds.

seedWorkflows :: SqlPersistT IO ()
seedWorkflows = do
  forM_ workflows $ \(identifier, code, nameEs, nameEn, sensitive, publicRead) ->
    rawExecute
      "INSERT INTO workflow_definition (id, code, name_es, name_en, sensitive, public_read, active, version) VALUES (?::uuid, ?, ?, ?, ?, ?, TRUE, 1) ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, sensitive=EXCLUDED.sensitive, public_read=EXCLUDED.public_read, active=TRUE WHERE workflow_definition.name_es IS DISTINCT FROM EXCLUDED.name_es OR workflow_definition.name_en IS DISTINCT FROM EXCLUDED.name_en OR workflow_definition.sensitive IS DISTINCT FROM EXCLUDED.sensitive OR workflow_definition.public_read IS DISTINCT FROM EXCLUDED.public_read OR workflow_definition.active IS DISTINCT FROM TRUE"
      [PersistText identifier, PersistText code, PersistText nameEs, PersistText nameEn, PersistBool sensitive, PersistBool publicRead]
  forM_ workflowStates $ \(identifier, workflowCode, code, nameEs, nameEn, orderValue, terminal) ->
    rawExecute
      "INSERT INTO workflow_state (id, workflow_id, code, name_es, name_en, sort_order, terminal, active, version) SELECT ?::uuid, id, ?, ?, ?, ?, ?, TRUE, 1 FROM workflow_definition WHERE code=? ON CONFLICT (workflow_id, code) DO NOTHING"
      [PersistText identifier, PersistText code, PersistText nameEs, PersistText nameEn, PersistInt64 orderValue, PersistBool terminal, PersistText workflowCode]
  forM_ transitions $ \(workflowCode, fromCode, toCode, requiresReview, distinctApprover) ->
    rawExecute
      "INSERT INTO workflow_transition (workflow_id, from_state_id, to_state_id, requires_review, requires_distinct_approver, active, version) SELECT w.id, f.id, t.id, ?, ?, TRUE, 1 FROM workflow_definition w JOIN workflow_state f ON f.workflow_id=w.id AND f.code=? JOIN workflow_state t ON t.workflow_id=w.id AND t.code=? WHERE w.code=? ON CONFLICT (workflow_id, from_state_id, to_state_id) DO NOTHING"
      [PersistBool requiresReview, PersistBool distinctApprover, PersistText fromCode, PersistText toCode, PersistText workflowCode]
  forM_ workflowDefaults $ \(workflowCode, stateCode) ->
    rawExecute
      "INSERT INTO workflow_default_state (workflow_id, state_id, context, active, version) SELECT w.id, s.id, 'initial', TRUE, 1 FROM workflow_definition w JOIN workflow_state s ON s.workflow_id=w.id AND s.code=? WHERE w.code=? ON CONFLICT (workflow_id, context) WHERE active DO NOTHING"
      [PersistText stateCode, PersistText workflowCode]
  forM_ workflowCapabilities $ \(workflowCode, stateCode, capabilityCode) ->
    rawExecute
      "INSERT INTO workflow_state_capability (state_id, capability_code, enabled, version) SELECT s.id, ?, TRUE, 1 FROM workflow_state s JOIN workflow_definition w ON w.id=s.workflow_id WHERE w.code=? AND s.code=? ON CONFLICT (state_id, capability_code) DO NOTHING"
      [PersistText capabilityCode, PersistText workflowCode, PersistText stateCode]
  where
    workflows =
      [ ("00000000-0000-4000-8000-000000000101", "catalog-publication", "Publicación de catálogos", "Catalog publication", False, False)
      , ("00000000-0000-4000-8000-000000000102", "sensitive-publication", "Publicación sensible", "Sensitive publication", True, False)
      , ("00000000-0000-4000-8000-000000000103", "governed-import", "Importación gobernada", "Governed import", True, False)
      , ("00000000-0000-4000-8000-000000000104", socialEventWorkflowCode, "Ciclo de vida de eventos sociales", "Social event lifecycle", False, True)
      , ("00000000-0000-4000-8000-000000000105", "pipeline-recording", "Pipeline de grabación", "Recording pipeline", False, False)
      , ("00000000-0000-4000-8000-000000000106", "pipeline-mixing", "Pipeline de mezcla", "Mixing pipeline", False, False)
      , ("00000000-0000-4000-8000-000000000107", "pipeline-mastering", "Pipeline de masterización", "Mastering pipeline", False, False)
      , ("00000000-0000-4000-8000-000000000108", "pipeline-rehearsal", "Pipeline de ensayo", "Rehearsal pipeline", False, False)
      , ("00000000-0000-4000-8000-000000000109", "pipeline-classes", "Pipeline de clases", "Classes pipeline", False, False)
      , ("00000000-0000-4000-8000-000000000110", "pipeline-event-production", "Pipeline de producción de eventos", "Event production pipeline", False, False)
      , ("00000000-0000-4000-8000-000000000111", "ddex-document-lifecycle", "Ciclo de vida de documentos DDEX", "DDEX document lifecycle", True, False)
      , ("00000000-0000-4000-8000-000000000112", "ddex-validation-lifecycle", "Ciclo de validación DDEX", "DDEX validation lifecycle", True, False)
      , ("00000000-0000-4000-8000-000000000113", "ddex-import-plan-lifecycle", "Ciclo de planes de importación DDEX", "DDEX import plan lifecycle", True, False)
      , ("00000000-0000-4000-8000-000000000114", "ddex-import-run-lifecycle", "Ciclo de ejecuciones de importación DDEX", "DDEX import run lifecycle", True, False)
      , ("00000000-0000-4000-8000-000000000115", "ddex-export-lifecycle", "Ciclo de exportaciones DDEX", "DDEX export lifecycle", True, False)
      , ("00000000-0000-4000-8000-000000000116", "ddex-job-lifecycle", "Ciclo de trabajos DDEX", "DDEX job lifecycle", True, False)
      ]
    workflowStates = concat
      [ statesFor "catalog-publication" 200
      , statesFor "sensitive-publication" 210
      , statesFor "governed-import" 220
      ]
      <> socialEventStates
      <> pipelineStates
      <> ddexDocumentStates
      <> ddexOperationalStates
    statesFor workflowCode base =
      [ (stateUuid (base + 1), workflowCode, "draft", "Borrador", "Draft", 10, False)
      , (stateUuid (base + 2), workflowCode, "review", "En revisión", "In review", 20, False)
      , (stateUuid (base + 3), workflowCode, "approved", "Aprobado", "Approved", 30, False)
      , (stateUuid (base + 4), workflowCode, "rejected", "Rechazado", "Rejected", 40, True)
      , (stateUuid (base + 5), workflowCode, "published", "Publicado", "Published", 50, True)
      , (stateUuid (base + 6), workflowCode, "archived", "Archivado", "Archived", 60, False)
      ]
    stateUuid suffix = "00000000-0000-4000-8000-000000000" <> pad3 suffix
    socialEventStates =
      [ (stateUuid 231, "social-event-lifecycle", "planning", "En planificación", "Planning", 10, False)
      , (stateUuid 232, "social-event-lifecycle", "announced", "Anunciado", "Announced", 20, False)
      , (stateUuid 233, "social-event-lifecycle", "on_sale", "En venta", "On sale", 30, False)
      , (stateUuid 234, "social-event-lifecycle", "live", "En vivo", "Live", 40, False)
      , (stateUuid 235, "social-event-lifecycle", "postponed", "Pospuesto", "Postponed", 50, False)
      , (stateUuid 236, "social-event-lifecycle", "unavailable", "No disponible", "Unavailable", 60, False)
      , (stateUuid 237, "social-event-lifecycle", "out_of_scope", "Fuera de cobertura", "Out of scope", 70, False)
      , (stateUuid 238, "social-event-lifecycle", "completed", "Completado", "Completed", 80, True)
      , (stateUuid 239, "social-event-lifecycle", "cancelled", "Cancelado", "Cancelled", 90, True)
      ]
    pipelineStates =
      pipelineStateRows "pipeline-recording" 240
        [ ("inquiry", "Consulta", "Inquiry"), ("quoted", "Cotizado", "Quoted")
        , ("scheduled", "Agendado", "Scheduled"), ("in-session", "En sesión", "In session")
        , ("editing", "Edición", "Editing"), ("approved", "Aprobado", "Approved")
        , ("delivered", "Entregado", "Delivered"), ("closed", "Cerrado", "Closed")
        ]
      <> pipelineStateRows "pipeline-mixing" 250
        [ ("brief", "Brief", "Brief"), ("prep", "Preparación", "Prep")
        , ("v1-sent", "V1 enviada", "V1 sent"), ("revisions", "Revisiones", "Revisions")
        , ("approved", "Aprobado", "Approved"), ("delivered", "Entregado", "Delivered")
        ]
      <> pipelineStateRows "pipeline-mastering" 260
        [ ("brief", "Brief", "Brief"), ("v1", "V1", "V1")
        , ("revisions", "Revisiones", "Revisions"), ("approved", "Aprobado", "Approved")
        , ("ddp-delivered", "DDP entregado", "DDP delivered")
        ]
      <> pipelineStateRows "pipeline-rehearsal" 270
        [ ("booked", "Reservado", "Booked"), ("in-use", "En uso", "In use")
        , ("completed", "Completado", "Completed"), ("no-show", "Inasistencia", "No-show")
        ]
      <> pipelineStateRows "pipeline-classes" 280
        [ ("enrolled", "Inscrito", "Enrolled"), ("scheduled", "Agendado", "Scheduled")
        , ("attended", "Asistió", "Attended"), ("makeup-needed", "Reposición necesaria", "Make-up needed")
        , ("completed", "Completado", "Completed")
        ]
      <> pipelineStateRows "pipeline-event-production" 290
        [ ("lead", "Prospecto", "Lead"), ("proposal", "Propuesta", "Proposal")
        , ("confirmed", "Confirmado", "Confirmed"), ("pre-production", "Preproducción", "Pre-production")
        , ("onsite", "En sitio", "Onsite"), ("post-production", "Posproducción", "Post-production")
        , ("settled", "Liquidado", "Settled")
        ]
    pipelineStateRows workflowCode base values =
      [ (stateUuid (base + position), workflowCode, code, nameEs, nameEn, fromIntegral position * 10, position == length values)
      | (position, (code, nameEs, nameEn)) <- zip [1 :: Int ..] values
      ]
    ddexDocumentStates =
      [ (stateUuid 301, "ddex-document-lifecycle", "received", "Recibido", "Received", 10, False)
      , (stateUuid 302, "ddex-document-lifecycle", "quarantined", "En cuarentena", "Quarantined", 20, False)
      , (stateUuid 303, "ddex-document-lifecycle", "queued", "En cola", "Queued", 30, False)
      , (stateUuid 304, "ddex-document-lifecycle", "validating", "Validando", "Validating", 40, False)
      , (stateUuid 305, "ddex-document-lifecycle", "invalid", "Inválido", "Invalid", 50, False)
      , (stateUuid 306, "ddex-document-lifecycle", "valid", "Válido", "Valid", 60, False)
      , (stateUuid 307, "ddex-document-lifecycle", "mapping_required", "Requiere mapeo", "Mapping required", 70, False)
      , (stateUuid 308, "ddex-document-lifecycle", "ready_to_import", "Listo para importar", "Ready to import", 80, False)
      , (stateUuid 309, "ddex-document-lifecycle", "importing", "Importando", "Importing", 90, False)
      , (stateUuid 310, "ddex-document-lifecycle", "imported", "Importado", "Imported", 100, False)
      , (stateUuid 311, "ddex-document-lifecycle", "import_failed", "Importación fallida", "Import failed", 110, False)
      , (stateUuid 312, "ddex-document-lifecycle", "superseded", "Reemplazado", "Superseded", 120, True)
      ]
    ddexOperationalStates =
      [ (stateUuid 313, "ddex-validation-lifecycle", "pending", "Pendiente", "Pending", 10, False)
      , (stateUuid 314, "ddex-validation-lifecycle", "running", "En ejecución", "Running", 20, False)
      , (stateUuid 315, "ddex-validation-lifecycle", "succeeded", "Exitosa", "Succeeded", 30, True)
      , (stateUuid 316, "ddex-validation-lifecycle", "failed", "Fallida", "Failed", 40, True)
      , (stateUuid 317, "ddex-validation-lifecycle", "warning", "Completada con advertencias", "Completed with warnings", 50, True)
      , (stateUuid 318, "ddex-import-plan-lifecycle", "draft", "Borrador", "Draft", 10, False)
      , (stateUuid 319, "ddex-import-plan-lifecycle", "resolved", "Resuelto", "Resolved", 20, False)
      , (stateUuid 320, "ddex-import-plan-lifecycle", "committed", "Confirmado", "Committed", 30, True)
      , (stateUuid 321, "ddex-import-plan-lifecycle", "abandoned", "Abandonado", "Abandoned", 40, True)
      , (stateUuid 322, "ddex-import-run-lifecycle", "pending", "Pendiente", "Pending", 10, False)
      , (stateUuid 323, "ddex-import-run-lifecycle", "running", "En ejecución", "Running", 20, False)
      , (stateUuid 324, "ddex-import-run-lifecycle", "succeeded", "Exitosa", "Succeeded", 30, True)
      , (stateUuid 325, "ddex-import-run-lifecycle", "failed", "Fallida", "Failed", 40, True)
      , (stateUuid 326, "ddex-import-run-lifecycle", "rolled_back", "Revertida", "Rolled back", 50, True)
      , (stateUuid 327, "ddex-export-lifecycle", "queued", "En cola", "Queued", 10, False)
      , (stateUuid 328, "ddex-export-lifecycle", "rendering", "Generando", "Rendering", 20, False)
      , (stateUuid 329, "ddex-export-lifecycle", "ready", "Lista", "Ready", 30, True)
      , (stateUuid 330, "ddex-export-lifecycle", "failed", "Fallida", "Failed", 40, True)
      , (stateUuid 331, "ddex-job-lifecycle", "pending", "Pendiente", "Pending", 10, False)
      , (stateUuid 332, "ddex-job-lifecycle", "processing", "Procesando", "Processing", 20, False)
      , (stateUuid 333, "ddex-job-lifecycle", "completed", "Completado", "Completed", 30, True)
      , (stateUuid 334, "ddex-job-lifecycle", "failed", "Fallido", "Failed", 40, True)
      , (stateUuid 335, "ddex-job-lifecycle", "retry", "Pendiente de reintento", "Retry pending", 50, False)
      ]
    transitions =
      [ (workflow, fromCode, toCode, True, workflow /= "catalog-publication")
      | workflow <- ["catalog-publication", "sensitive-publication", "governed-import"]
      , (fromCode, toCode) <- [("draft", "review"), ("review", "approved"), ("review", "rejected"), ("rejected", "review"), ("approved", "published"), ("published", "archived"), ("archived", "review")]
      ] <> socialEventTransitions <> pipelineTransitions <> ddexDocumentTransitions <> ddexOperationalTransitions
    socialEventTransitions =
      [ ("social-event-lifecycle", fromCode, toCode, False, False)
      | (fromCode, toCode) <-
          [ ("planning", "announced"), ("planning", "on_sale"), ("planning", "live"), ("planning", "postponed"), ("planning", "unavailable"), ("planning", "out_of_scope"), ("planning", "completed"), ("planning", "cancelled")
          , ("announced", "on_sale"), ("announced", "live"), ("announced", "postponed"), ("announced", "cancelled"), ("announced", "completed"), ("announced", "unavailable"), ("announced", "out_of_scope")
          , ("on_sale", "announced"), ("on_sale", "live"), ("on_sale", "postponed"), ("on_sale", "cancelled"), ("on_sale", "completed"), ("on_sale", "unavailable"), ("on_sale", "out_of_scope")
          , ("postponed", "announced"), ("postponed", "on_sale"), ("postponed", "live"), ("postponed", "cancelled"), ("postponed", "completed"), ("postponed", "unavailable"), ("postponed", "out_of_scope")
          , ("live", "completed"), ("live", "cancelled")
          , ("unavailable", "announced"), ("unavailable", "on_sale"), ("unavailable", "live"), ("unavailable", "postponed"), ("unavailable", "completed"), ("unavailable", "out_of_scope")
          , ("out_of_scope", "announced"), ("out_of_scope", "on_sale"), ("out_of_scope", "live"), ("out_of_scope", "postponed"), ("out_of_scope", "completed"), ("out_of_scope", "unavailable")
          ]
      ]
    pipelineTransitions =
      [ (workflowCode, fromCode, toCode, False, False)
      | (workflowCode, codes) <- pipelineWorkflowCodes
      , fromCode <- codes
      , toCode <- codes
      , fromCode /= toCode
      ]
    pipelineWorkflowCodes =
      [ ("pipeline-recording", ["inquiry", "quoted", "scheduled", "in-session", "editing", "approved", "delivered", "closed"])
      , ("pipeline-mixing", ["brief", "prep", "v1-sent", "revisions", "approved", "delivered"])
      , ("pipeline-mastering", ["brief", "v1", "revisions", "approved", "ddp-delivered"])
      , ("pipeline-rehearsal", ["booked", "in-use", "completed", "no-show"])
      , ("pipeline-classes", ["enrolled", "scheduled", "attended", "makeup-needed", "completed"])
      , ("pipeline-event-production", ["lead", "proposal", "confirmed", "pre-production", "onsite", "post-production", "settled"])
      ]
    ddexDocumentTransitions =
      [ ("ddex-document-lifecycle", fromCode, toCode, False, False)
      | (fromCode, toCode) <-
          [ ("received", "quarantined"), ("received", "queued")
          , ("quarantined", "queued"), ("quarantined", "superseded")
          , ("queued", "validating"), ("queued", "quarantined")
          , ("validating", "invalid"), ("validating", "valid"), ("validating", "mapping_required"), ("validating", "quarantined")
          , ("invalid", "queued"), ("invalid", "superseded")
          , ("valid", "mapping_required"), ("valid", "ready_to_import"), ("valid", "superseded")
          , ("mapping_required", "ready_to_import"), ("mapping_required", "superseded")
          , ("ready_to_import", "importing"), ("ready_to_import", "superseded")
          , ("importing", "imported"), ("importing", "import_failed")
          , ("import_failed", "ready_to_import"), ("import_failed", "superseded")
          , ("imported", "superseded")
          ]
      ]
    ddexOperationalTransitions = concat
      [ operationalTransitions "ddex-validation-lifecycle"
          [("pending","running"),("pending","failed"),("running","succeeded"),("running","failed"),("running","warning")]
      , operationalTransitions "ddex-import-plan-lifecycle"
          [("draft","resolved"),("draft","abandoned"),("resolved","draft"),("resolved","committed"),("resolved","abandoned")]
      , operationalTransitions "ddex-import-run-lifecycle"
          [("pending","running"),("pending","failed"),("running","succeeded"),("running","failed"),("running","rolled_back"),("succeeded","rolled_back")]
      , operationalTransitions "ddex-export-lifecycle"
          [("queued","rendering"),("queued","failed"),("rendering","ready"),("rendering","failed")]
      , operationalTransitions "ddex-job-lifecycle"
          [("pending","processing"),("pending","failed"),("processing","completed"),("processing","failed"),("processing","retry"),("retry","processing"),("retry","failed")]
      ]
    operationalTransitions workflowCode pairs =
      [(workflowCode, fromCode, toCode, False, False) | (fromCode, toCode) <- pairs]
    workflowDefaults =
      [ ("catalog-publication", "draft")
      , ("sensitive-publication", "draft")
      , ("governed-import", "draft")
      , ("social-event-lifecycle", "planning")
      , ("pipeline-recording", "inquiry")
      , ("pipeline-mixing", "brief")
      , ("pipeline-mastering", "brief")
      , ("pipeline-rehearsal", "booked")
      , ("pipeline-classes", "enrolled")
      , ("pipeline-event-production", "lead")
      , ("ddex-document-lifecycle", "received")
      , ("ddex-validation-lifecycle", "pending")
      , ("ddex-import-plan-lifecycle", "draft")
      , ("ddex-import-run-lifecycle", "pending")
      , ("ddex-export-lifecycle", "queued")
      , ("ddex-job-lifecycle", "pending")
      ]
    workflowCapabilities =
      [ ("social-event-lifecycle", stateCode, "public-listable")
      | stateCode <- ["announced", "on_sale", "live", "postponed"]
      ] <>
      [ ("social-event-lifecycle", stateCode, "ticket-purchase")
      | stateCode <- ["announced", "on_sale", "live"]
      ]

seedSecurityRegistry :: SqlPersistT IO ()
seedSecurityRegistry = do
  forM_ (zip [0 :: Int ..] securityModules) $ \(position, (code, nameEs, nameEn)) ->
    rawExecute
      "INSERT INTO security_module (code, name_es, name_en, sort_order, active, internal_only, version) VALUES (?, ?, ?, ?, TRUE, TRUE, 1) ON CONFLICT (code) DO NOTHING"
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistInt64 (fromIntegral position)]
  forM_ securityActions $ \(code, nameEs, nameEn, sensitive) ->
    rawExecute
      "INSERT INTO security_action (code, name_es, name_en, sensitive, grantable, active, version) VALUES (?, ?, ?, ?, TRUE, TRUE, 1) ON CONFLICT (code) DO NOTHING"
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistBool sensitive]
  forM_ securityModules $ \(moduleCode, nameEs, nameEn) ->
    rawExecute
      "INSERT INTO security_permission (code, module_id, action_id, resource_scope, name_es, name_en, sensitive, public_metadata, active, version) SELECT ? || '.access', m.id, a.id, 'module', ?, ?, FALSE, FALSE, TRUE, 1 FROM security_module m CROSS JOIN security_action a WHERE m.code=? AND a.code='access' ON CONFLICT (code) DO NOTHING"
      [PersistText moduleCode, PersistText ("Acceso a " <> nameEs), PersistText (nameEn <> " access"), PersistText moduleCode]
  forM_ catalogPermissions $ \(actionCode, sensitive) ->
    rawExecute
      "INSERT INTO security_permission (code, module_id, action_id, resource_scope, name_es, name_en, sensitive, public_metadata, active, version) SELECT 'catalog.' || ?, m.id, a.id, 'catalog', 'Catálogos: ' || ?, 'Catalogs: ' || ?, ?, FALSE, TRUE, 1 FROM security_module m CROSS JOIN security_action a WHERE m.code='catalog' AND a.code=? ON CONFLICT (code) DO NOTHING"
      [PersistText actionCode, PersistText actionCode, PersistText actionCode, PersistBool sensitive, PersistText actionCode]
  forM_ securityPermissions $ \(actionCode, sensitive) ->
    rawExecute
      "INSERT INTO security_permission (code, module_id, action_id, resource_scope, name_es, name_en, sensitive, public_metadata, active, version) SELECT 'security.' || ?, m.id, a.id, 'security-registry', 'Seguridad: ' || ?, 'Security: ' || ?, ?, FALSE, TRUE, 1 FROM security_module m CROSS JOIN security_action a WHERE m.code='admin' AND a.code=? ON CONFLICT (code) DO NOTHING"
      [PersistText actionCode, PersistText actionCode, PersistText actionCode, PersistBool sensitive, PersistText actionCode]
  forM_ pipelinePermissions $ \(actionCode, nameEs, nameEn, sensitive) ->
    rawExecute
      "INSERT INTO security_permission (code, module_id, action_id, resource_scope, name_es, name_en, sensitive, public_metadata, active, version) SELECT 'pipeline.' || ?, m.id, a.id, 'pipeline', ?, ?, ?, FALSE, TRUE, 1 FROM security_module m CROSS JOIN security_action a WHERE m.code='scheduling' AND a.code=? ON CONFLICT (code) DO NOTHING"
      [PersistText actionCode, PersistText nameEs, PersistText nameEn, PersistBool sensitive, PersistText actionCode]
  forM_ (zip [0 :: Int ..] securityRoles) $ \(position, (code, nameEs, nameEn, emergency, selfAssignable, automaticAssignable)) ->
    rawExecute
      "INSERT INTO security_role (code, name_es, name_en, sort_order, system_role, emergency_administrator, self_assignable, automatic_assignable, active, workflow_state_id, published_revision, version) SELECT ?, ?, ?, ?, TRUE, ?, ?, ?, TRUE, ws.id, 1, 1 FROM workflow_state ws JOIN workflow_definition w ON w.id=ws.workflow_id WHERE w.code='sensitive-publication' AND ws.code='published' ON CONFLICT (code) DO NOTHING"
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistInt64 (fromIntegral position), PersistBool emergency, PersistBool selfAssignable, PersistBool automaticAssignable]
  rawExecute
    "UPDATE security_role SET self_assignable=TRUE, updated_at=CURRENT_TIMESTAMP WHERE code='fan' AND version=1 AND self_assignable=FALSE"
    []
  rawExecute
    "UPDATE security_role SET automatic_assignable=TRUE, updated_at=CURRENT_TIMESTAMP WHERE code IN ('customer','artist','student','teacher') AND version=1 AND automatic_assignable=FALSE"
    []
  forM_ securityRoleAssignmentPolicies $ \(identifier, code, triggerCode, roleCode, nameEs, nameEn, requiresVerifiedEmail) ->
    rawExecute
      "INSERT INTO security_role_assignment_policy (id, code, trigger_code, role_id, name_es, name_en, requires_verified_email, active, version) SELECT ?::uuid, ?, ?, r.id, ?, ?, ?, TRUE, 1 FROM security_role r WHERE r.code=? ON CONFLICT (code) DO NOTHING"
      [PersistText identifier, PersistText code, PersistText triggerCode, PersistText nameEs, PersistText nameEn, PersistBool requiresVerifiedEmail, PersistText roleCode]
  forM_ roleModuleGrants $ \(roleCode, moduleCodes) ->
    forM_ moduleCodes $ \moduleCode ->
      rawExecute
        "INSERT INTO role_permission (role_id, permission_id, granted_by, active, version) SELECT r.id, p.id, NULL, TRUE, 1 FROM security_role r JOIN security_permission p ON p.code=? || '.access' WHERE r.code=? ON CONFLICT (role_id, permission_id) DO NOTHING"
        [PersistText moduleCode, PersistText roleCode]
  forM_ roleCatalogGrants $ \(roleCode, actionCodes) ->
    forM_ actionCodes $ \actionCode ->
      rawExecute
        "INSERT INTO role_permission (role_id, permission_id, granted_by, active, version) SELECT r.id, p.id, NULL, TRUE, 1 FROM security_role r JOIN security_permission p ON p.code='catalog.' || ? WHERE r.code=? ON CONFLICT (role_id, permission_id) DO NOTHING"
        [PersistText actionCode, PersistText roleCode]
  forM_ roleSecurityGrants $ \(roleCode, actionCodes) ->
    forM_ actionCodes $ \actionCode ->
      rawExecute
        "INSERT INTO role_permission (role_id, permission_id, granted_by, approval_mode, active, version) SELECT r.id, p.id, NULL, 'bootstrap', TRUE, 1 FROM security_role r JOIN security_permission p ON p.code='security.' || ? WHERE r.code=? ON CONFLICT (role_id, permission_id) DO NOTHING"
        [PersistText actionCode, PersistText roleCode]
  forM_ rolePipelineGrants $ \(roleCode, actionCodes) ->
    forM_ actionCodes $ \actionCode ->
      rawExecute
        "INSERT INTO role_permission (role_id, permission_id, granted_by, active, version) SELECT r.id, p.id, NULL, TRUE, 1 FROM security_role r JOIN security_permission p ON p.code='pipeline.' || ? WHERE r.code=? ON CONFLICT (role_id, permission_id) DO NOTHING"
        [PersistText actionCode, PersistText roleCode]
  where
    securityModules =
      [ ("crm", "CRM", "CRM")
      , ("scheduling", "Agenda", "Scheduling")
      , ("packages", "Paquetes", "Packages")
      , ("invoicing", "Facturación", "Invoicing")
      , ("admin", "Administración", "Administration")
      , ("internships", "Pasantías", "Internships")
      , ("ops", "Operaciones", "Operations")
      , ("catalog", "Catálogos", "Catalogs")
      ]
    securityActions =
      [ ("access", "Acceder", "Access", False)
      , ("read", "Leer", "Read", False)
      , ("create", "Crear", "Create", False)
      , ("update", "Editar", "Update", False)
      , ("delete", "Eliminar", "Delete", True)
      , ("review", "Revisar", "Review", True)
      , ("approve", "Aprobar", "Approve", True)
      , ("publish", "Publicar", "Publish", True)
      , ("import", "Importar", "Import", True)
      , ("export", "Exportar", "Export", False)
      , ("merge", "Fusionar", "Merge", True)
      , ("replace", "Reemplazar", "Replace", True)
      , ("deactivate", "Desactivar", "Deactivate", True)
      , ("restore", "Restaurar", "Restore", True)
      , ("assign", "Asignar", "Assign", True)
      , ("emergency-recover", "Recuperación de emergencia", "Emergency recover", True)
      ]
    catalogPermissions =
      [ ("read", False), ("create", False), ("update", False), ("review", True)
      , ("approve", True), ("publish", True), ("import", True), ("export", False)
      , ("merge", True), ("replace", True), ("deactivate", True), ("restore", True)
      ]
    securityPermissions =
      [ ("read", True)
      , ("create", True)
      , ("review", True)
      , ("approve", True)
      , ("assign", True)
      , ("emergency-recover", True)
      ]
    pipelinePermissions =
      [ ("read", "Pipelines: leer", "Pipelines: read", False)
      , ("create", "Pipelines: crear tarjetas", "Pipelines: create cards", False)
      , ("update", "Pipelines: editar y mover tarjetas", "Pipelines: update and move cards", False)
      , ("delete", "Pipelines: eliminar tarjetas", "Pipelines: delete cards", True)
      ]
    securityRoles =
      [ ("admin", "Administrador", "Administrator", True, False, False)
      , ("manager", "Gerente", "Manager", False, False, False)
      , ("studio-manager", "Gerente de estudio", "Studio manager", False, False, False)
      , ("engineer", "Ingeniero", "Engineer", False, False, False)
      , ("teacher", "Docente", "Teacher", False, False, True)
      , ("reception", "Recepción", "Reception", False, False, False)
      , ("accounting", "Contabilidad", "Accounting", False, False, False)
      , ("live-sessions-producer", "Productor de Live Sessions", "Live Sessions producer", False, False, False)
      , ("intern", "Pasante", "Intern", False, False, False)
      , ("artist", "Artista", "Artist", False, False, True)
      , ("artista", "Artista (histórico)", "Artist (historical)", False, False, False)
      , ("webmaster", "Webmaster", "Webmaster", False, False, False)
      , ("promotor", "Promotor", "Promoter", False, False, False)
      , ("promoter", "Promotor (inglés)", "Promoter", False, False, False)
      , ("producer", "Productor", "Producer", False, False, False)
      , ("agency", "Agencia", "Agency", False, False, False)
      , ("songwriter", "Compositor", "Songwriter", False, False, False)
      , ("dj", "DJ", "DJ", False, False, False)
      , ("publicist", "Relacionista público", "Publicist", False, False, False)
      , ("tour-manager", "Gerente de gira", "Tour manager", False, False, False)
      , ("label-rep", "Representante de sello", "Label representative", False, False, False)
      , ("stage-manager", "Director de escenario", "Stage manager", False, False, False)
      , ("road-crew", "Equipo de gira", "Road crew", False, False, False)
      , ("photographer", "Fotógrafo", "Photographer", False, False, False)
      , ("a-and-r", "A&R", "A&R", False, False, False)
      , ("student", "Estudiante", "Student", False, False, True)
      , ("vendor", "Proveedor", "Vendor", False, False, False)
      , ("read-only", "Solo lectura", "Read only", False, False, False)
      , ("customer", "Cliente", "Customer", False, False, True)
      , ("fan", "Fan", "Fan", False, True, False)
      , ("maintenance", "Mantenimiento", "Maintenance", False, False, False)
      ]
    securityRoleAssignmentPolicies =
      [ ("00000000-0000-4000-8000-000000000301", "account.signup.customer", "account-signup", "customer", "Cliente al crear cuenta", "Customer on account signup", False)
      , ("00000000-0000-4000-8000-000000000302", "account.google.customer", "google-account-create", "customer", "Cliente con cuenta Google", "Customer on Google account creation", True)
      , ("00000000-0000-4000-8000-000000000303", "artist.verified-claim.artist", "verified-artist-claim", "artist", "Artista por reclamo verificado", "Artist on verified profile claim", True)
      , ("00000000-0000-4000-8000-000000000304", "account.generated.customer", "generated-account-create", "customer", "Cliente por cuenta generada", "Customer on generated account creation", False)
      , ("00000000-0000-4000-8000-000000000305", "course.registration.student", "course-registration", "student", "Estudiante por registro de curso", "Student on course registration", False)
      , ("00000000-0000-4000-8000-000000000306", "trial.inquiry.student", "trial-inquiry", "student", "Estudiante por consulta de clase", "Student on lesson inquiry", False)
      , ("00000000-0000-4000-8000-000000000307", "trial.teacher-subject.teacher", "teacher-subject-configured", "teacher", "Docente por configuración de materias", "Teacher on subject configuration", False)
      , ("00000000-0000-4000-8000-000000000308", "trial.teacher-student.student", "teacher-student-linked", "student", "Estudiante por vínculo docente", "Student on teacher link", False)
      , ("00000000-0000-4000-8000-000000000309", "trial.student-created.student", "student-created", "student", "Estudiante creado por la escuela", "Student created by school", False)
      , ("00000000-0000-4000-8000-000000000310", "live-session.artist-profile.artist", "artist-profile-created", "artist", "Artista por perfil de Live Session", "Artist on Live Session profile creation", False)
      ]
    roleModuleGrants =
      [ ("admin", ["crm", "scheduling", "packages", "invoicing", "admin", "internships", "ops", "catalog"])
      , ("manager", ["crm", "scheduling", "packages", "invoicing", "internships", "ops", "catalog"])
      , ("studio-manager", ["crm", "scheduling", "packages", "invoicing", "admin", "internships", "ops", "catalog"])
      , ("reception", ["crm", "scheduling"])
      , ("accounting", ["invoicing"])
      , ("engineer", ["scheduling"])
      , ("teacher", ["scheduling"])
      , ("live-sessions-producer", ["crm", "scheduling"])
      , ("intern", ["internships"])
      , ("artist", ["scheduling", "packages"])
      , ("artista", ["scheduling", "packages"])
      , ("webmaster", ["admin", "crm"])
      , ("producer", ["crm", "scheduling"])
      , ("label-rep", ["catalog"])
      , ("a-and-r", ["crm", "scheduling", "catalog"])
      , ("student", ["scheduling"])
      , ("vendor", ["packages"])
      , ("customer", ["packages"])
      , ("read-only", ["crm", "catalog"])
      , ("maintenance", ["packages", "scheduling", "ops"])
      ]
    roleCatalogGrants =
      [ ("admin", map fst catalogPermissions)
      , ("manager", ["read", "create", "update", "review", "export"])
      , ("studio-manager", ["read", "create", "update", "review", "export"])
      , ("label-rep", ["read", "create", "update", "export"])
      , ("a-and-r", ["read", "create", "update", "export"])
      , ("webmaster", ["read"])
      , ("read-only", ["read", "export"])
      ]
    roleSecurityGrants =
      [ ("admin", map fst securityPermissions)
      ]
    rolePipelineGrants =
      [ ("admin", pipelineActionCodes)
      , ("manager", pipelineActionCodes)
      , ("studio-manager", pipelineActionCodes)
      , ("reception", ["read", "create", "update"])
      , ("engineer", ["read", "update"])
      , ("teacher", ["read", "update"])
      , ("live-sessions-producer", ["read", "create", "update"])
      , ("producer", ["read", "create", "update"])
      , ("a-and-r", ["read"])
      ]
    pipelineActionCodes = map (\(actionCode, _, _, _) -> actionCode) pipelinePermissions

seedCatalogDefinitions :: SqlPersistT IO ()
seedCatalogDefinitions =
  forM_ (zip [1 :: Int ..] catalogDefinitions) $ \(position, (code, classification, entityKind, nameEs, nameEn, publicRead, sensitive, workflowCode)) ->
    rawExecute
      "INSERT INTO catalog_definition (id, code, classification, entity_kind, name_es, name_en, public_read, sensitive, ordering_mode, workflow_id, cache_revision, active, version) SELECT ?::uuid, ?, ?, ?, ?, ?, ?, ?, 'manual', w.id, 1, TRUE, 1 FROM workflow_definition w WHERE w.code=? ON CONFLICT (code) DO NOTHING"
      [ PersistText (catalogUuid position), PersistText code, PersistText classification, PersistText entityKind
      , PersistText nameEs, PersistText nameEn, PersistBool publicRead, PersistBool sensitive, PersistText workflowCode
      ]
  where
    catalogDefinitions =
      [ dynamic "genres" "genre" "Géneros" "Genres" True
      , dynamic "instruments" "instrument" "Instrumentos" "Instruments" True
      , dynamic "service-categories" "service_category" "Categorías de servicios" "Service categories" True
      , dynamic "services" "service_offering" "Servicios" "Services" True
      , dynamic "event-types" "event_type" "Tipos de evento" "Event types" True
      , dynamic "booking-types" "booking_type" "Tipos de reserva" "Booking types" True
      , dynamic "release-types" "release_type" "Tipos de lanzamiento" "Release types" True
      , dynamic "recording-types" "recording_type" "Tipos de grabación" "Recording types" True
      , dynamic "recording-session-types" "recording_session_type" "Tipos de sesión" "Recording session types" True
      , dynamic "content-categories" "content_category" "Categorías de contenido" "Content categories" True
      , dynamic "tags" "tag" "Etiquetas" "Tags" True
      , dynamic "reaction-types" "reaction_type" "Tipos de reacción" "Reaction types" True
      , dynamic "records-releases" "record_release" "Lanzamientos" "Releases" True
      , dynamic "records-recordings" "recording" "Grabaciones" "Recordings" True
      , dynamic "records-sessions" "recording_session" "Sesiones" "Sessions" True
      , dynamic "editorial-collections" "editorial_collection" "Colecciones editoriales" "Editorial collections" True
      , governed "countries" "country_reference" "Países" "Countries" True
      , governed "subdivisions" "subdivision_reference" "Subdivisiones" "Subdivisions" True
      , governed "cities" "city_reference" "Ciudades" "Cities" True
      , governed "currencies" "currency_reference" "Monedas" "Currencies" True
      , governed "languages" "language_reference" "Idiomas" "Languages" True
      , governed "locales" "locale_reference" "Configuraciones regionales" "Locales" True
      , governed "external-providers" "external_provider" "Proveedores externos" "External providers" False
      , governed "external-provider-codes" "external_provider_code" "Códigos de proveedores" "Provider codes" False
      , governed "ddex-message-types" "ddex_message_type" "Mensajes DDEX" "DDEX message types" False
      , governed "ddex-codes" "ddex_code" "Vocabularios DDEX" "DDEX codes" False
      , security "security-roles" "security_role" "Roles de seguridad" "Security roles"
      , security "security-modules" "security_module" "Módulos de seguridad" "Security modules"
      , security "security-permissions" "security_permission" "Permisos de seguridad" "Security permissions"
      -- Append-only: catalog UUIDs are position-derived bootstrap identities.
      -- Never insert a new definition above an existing entry.
      , dynamic "record-contributors" "record_contributor" "Colaboradores discográficos" "Record contributors" True
      , dynamic "authored-content" "authored_content" "Contenido editorial" "Authored content" False
      , dynamic "service-pricing-models" "service_pricing_model" "Modelos de precio" "Service pricing models" True
      , governed "tax-rates" "tax_rate_reference" "Tasas tributarias" "Tax rates" False
      , dynamic "service-resource-selection-modes" "service_resource_selection_mode" "Políticas de selección de recursos" "Resource selection policies" False
      , dynamic "radio-auto-stop-options" "radio_auto_stop_option" "Duraciones de autoapagado de Radio" "Radio auto-stop durations" False
      , dynamic "appearance-modes" "appearance_mode_option" "Modos de apariencia" "Appearance modes" True
      , dynamic "feedback-categories" "feedback_category" "Categorías de feedback" "Feedback categories" True
      , dynamic "feedback-severities" "feedback_severity" "Severidades de feedback" "Feedback severities" True
      , governed "ddex-standard-versions" "ddex_standard_version" "Versiones de estándares DDEX" "DDEX standard versions" False
      , governed "ddex-vocabularies" "ddex_vocabulary" "Vocabularios DDEX" "DDEX vocabularies" False
      , technical "ddex-job-operations" "ddex_job_operation" "Operaciones técnicas DDEX" "DDEX technical operations"
      , technical "ddex-import-operations" "ddex_import_operation" "Operaciones de auditoría de importación DDEX" "DDEX import audit operations"
      , technical "ddex-validation-results" "ddex_validation_result" "Resultados de validación DDEX" "DDEX validation results"
      , technical "ddex-validation-severities" "ddex_validation_severity" "Severidades de validación DDEX" "DDEX validation severities"
      , technical "ddex-validation-layers" "ddex_validation_layer" "Capas de validación DDEX" "DDEX validation layers"
      , dynamic "content-reaction-types" "content_reaction_type" "Reacciones de contenido" "Content reactions" True
      , dynamic "creator-badge-types" "creator_badge_type" "Insignias de creadores" "Creator badges" True
      , dynamic "professions" "profession" "Profesiones musicales" "Music professions" True
      , dynamic "classified-categories" "classified_category" "Categorías de clasificados" "Classified categories" True
      , dynamic "compensation-types" "compensation_type" "Tipos de compensación" "Compensation types" True
      , governed "metropolitan-areas" "metropolitan_area" "Áreas metropolitanas" "Metropolitan areas" True
      ]
    dynamic code entityKind nameEs nameEn publicRead = (code, "dynamic-business-catalog", entityKind, nameEs, nameEn, publicRead, False, "catalog-publication")
    governed code entityKind nameEs nameEn publicRead = (code, "governed-reference-data", entityKind, nameEs, nameEn, publicRead, True, "governed-import")
    security code entityKind nameEs nameEn = (code, "security-system-registry", entityKind, nameEs, nameEn, False, True, "sensitive-publication")
    technical code entityKind nameEs nameEn = (code, "genuine-technical-constant", entityKind, nameEs, nameEn, False, True, "governed-import")

seedInternationalFoundation :: SqlPersistT IO ()
seedInternationalFoundation = do
  forM_ currencies $ \(position, (code, numericCode, nameEs, nameEn, symbol, minorUnits)) ->
    rawExecute
      "INSERT INTO currency_reference (code, numeric_code, name_es, name_en, symbol, minor_units, standard, source_version, last_synced_at, active, sort_order, version) VALUES (?, ?, ?, ?, ?, ?, 'ISO 4217', 'ISO 4217:2025-05-12', now(), TRUE, ?, 1) ON CONFLICT (code) DO NOTHING"
      [PersistText code, PersistText numericCode, PersistText nameEs, PersistText nameEn, PersistText symbol, PersistInt64 minorUnits, PersistInt64 (fromIntegral position)]
  forM_ countryReferenceSeeds $ \(CountryReferenceSeed alpha2 alpha3 numericCode nameEs nameEn position) ->
    rawExecute
      "INSERT INTO country_reference (alpha2, alpha3, numeric_code, name_es, name_en, standard, source_version, effective_from, deprecated_at, last_synced_at, active, sort_order, version) VALUES (?, ?, ?, ?, ?, 'UN M49 / ISO 3166-1', ?, ?::date, NULL, ?::date::timestamptz, TRUE, ?, 1) ON CONFLICT (alpha2) DO UPDATE SET alpha3=EXCLUDED.alpha3, numeric_code=EXCLUDED.numeric_code, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, standard=EXCLUDED.standard, source_version=EXCLUDED.source_version, effective_from=EXCLUDED.effective_from, deprecated_at=NULL, last_synced_at=EXCLUDED.last_synced_at, active=TRUE, sort_order=EXCLUDED.sort_order, version=country_reference.version+1 WHERE (country_reference.alpha3, country_reference.numeric_code, country_reference.name_es, country_reference.name_en, country_reference.standard, country_reference.source_version, country_reference.effective_from, country_reference.deprecated_at, country_reference.last_synced_at, country_reference.active, country_reference.sort_order) IS DISTINCT FROM (EXCLUDED.alpha3, EXCLUDED.numeric_code, EXCLUDED.name_es, EXCLUDED.name_en, EXCLUDED.standard, EXCLUDED.source_version, EXCLUDED.effective_from, EXCLUDED.deprecated_at, EXCLUDED.last_synced_at, EXCLUDED.active, EXCLUDED.sort_order)"
      [ PersistText alpha2
      , PersistText alpha3
      , PersistText numericCode
      , PersistText nameEs
      , PersistText nameEn
      , PersistText countryReferenceSourceVersion
      , PersistText countryReferenceSnapshotDate
      , PersistText countryReferenceSnapshotDate
      , PersistInt64 (fromIntegral position)
      ]
  rawExecute
    "UPDATE country_reference SET active=FALSE, deprecated_at=COALESCE(deprecated_at, ?::date::timestamptz), last_synced_at=?::date::timestamptz, version=version+1 WHERE standard='UN M49 / ISO 3166-1' AND source_version<>? AND active"
    [ PersistText countryReferenceSnapshotDate
    , PersistText countryReferenceSnapshotDate
    , PersistText countryReferenceSourceVersion
    ]
  forM_ languages $ \(position, (iso6391, iso6392T, nameEs, nameEn)) ->
    rawExecute
      "INSERT INTO language_reference (iso6391, iso6392_t, name_es, name_en, standard, source_version, last_synced_at, active, sort_order, version) VALUES (?, ?, ?, ?, 'ISO 639', 'ISO 639:2025', now(), TRUE, ?, 1) ON CONFLICT (iso6392_t) DO NOTHING"
      [PersistText iso6391, PersistText iso6392T, PersistText nameEs, PersistText nameEn, PersistInt64 (fromIntegral position)]
  forM_ locales $ \(position, (code, languageCode, countryCode, nameEs, nameEn, isDefault)) ->
    rawExecute
      "INSERT INTO locale_reference (code, language_id, country_id, name_es, name_en, default_for_platform, source_version, last_synced_at, active, sort_order, version) SELECT ?, l.id, c.id, ?, ?, ?, 'Unicode CLDR 47', now(), TRUE, ?, 1 FROM language_reference l LEFT JOIN country_reference c ON c.alpha2=? WHERE l.iso6391=? ON CONFLICT (code) DO NOTHING"
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistBool isDefault, PersistInt64 (fromIntegral position), PersistText countryCode, PersistText languageCode]
  forM_ providers $ \(position, (code, nameEs, nameEn, providerKind, sourceVersion)) ->
    rawExecute
      "INSERT INTO external_provider (code, name_es, name_en, provider_kind, public_metadata, active, source_version, last_synced_at, sort_order, version) VALUES (?, ?, ?, ?, TRUE, TRUE, ?, now(), ?, 1) ON CONFLICT (code) DO NOTHING"
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistText providerKind, PersistText sourceVersion, PersistInt64 (fromIntegral position)]
  forM_ currencies $ \(_, (code, _, _, _, _, _)) ->
    rawExecute
      "INSERT INTO deployment_currency_enablement (deployment_code, currency_id, enabled, default_currency, version) SELECT 'default', id, TRUE, code='USD', 1 FROM currency_reference WHERE code=? ON CONFLICT (deployment_code, currency_id) DO NOTHING"
      [PersistText code]
  forM_ locales $ \(_, (code, _, _, _, _, _)) ->
    rawExecute
      "INSERT INTO deployment_locale_enablement (deployment_code, locale_id, enabled, default_locale, version) SELECT 'default', id, TRUE, code='es', 1 FROM locale_reference WHERE code=? ON CONFLICT (deployment_code, locale_id) DO NOTHING"
      [PersistText code]
  where
    currencies = zip [0 :: Int ..]
      [ ("USD", "840", "Dólar estadounidense", "US Dollar", "$", 2)
      , ("EUR", "978", "Euro", "Euro", "€", 2)
      , ("GBP", "826", "Libra esterlina", "Pound Sterling", "£", 2)
      , ("CAD", "124", "Dólar canadiense", "Canadian Dollar", "C$", 2)
      , ("AUD", "036", "Dólar australiano", "Australian Dollar", "A$", 2)
      , ("JPY", "392", "Yen", "Yen", "¥", 0)
      , ("BRL", "986", "Real brasileño", "Brazilian Real", "R$", 2)
      ]
    languages = zip [0 :: Int ..]
      [ ("es", "spa", "Español", "Spanish")
      , ("en", "eng", "Inglés", "English")
      , ("fr", "fra", "Francés", "French")
      , ("de", "deu", "Alemán", "German")
      , ("pt", "por", "Portugués", "Portuguese")
      ]
    locales = zip [0 :: Int ..]
      [ ("es", "es", "EC", "Español", "Spanish", True)
      , ("en", "en", "US", "Inglés", "English", False)
      , ("fr", "fr", "", "Francés", "French", False)
      , ("de", "de", "", "Alemán", "German", False)
      , ("pt", "pt", "", "Portugués", "Portuguese", False)
      ]
    providers = zip [0 :: Int ..]
      [ ("spotify", "Spotify", "Spotify", "streaming-platform", "Spotify Web API 2026-08")
      , ("youtube", "YouTube", "YouTube", "video-platform", "YouTube Data API v3")
      ]

seedDdexFoundation :: SqlPersistT IO ()
seedDdexFoundation = do
  forM_ ddexCatalogCodes $ \catalogCode ->
    rawExecute
      "UPDATE catalog_definition SET source_name='DDEX Knowledge Base', source_version='DDEX standards snapshot 2026-08-11', source_effective_date='2026-08-11'::date, last_synced_at='2026-08-11'::date::timestamptz, updated_at=CURRENT_TIMESTAMP, version=version+1 WHERE code=? AND (source_name, source_version, source_effective_date, last_synced_at) IS DISTINCT FROM ('DDEX Knowledge Base', 'DDEX standards snapshot 2026-08-11', '2026-08-11'::date, '2026-08-11'::date::timestamptz)"
      [PersistText catalogCode]
  forM_ standardVersions $ \(identifier, standardCode, versionCode, nameEs, nameEn, descriptionEs, descriptionEn, namespaceUri, sourceUri, position) ->
    rawExecute
      "INSERT INTO ddex_standard_version (id, standard_code, version_code, name_es, name_en, description_es, description_en, namespace_uri, schema_uri, source_uri, effective_from, effective_until, deprecated_at, replacement_id, source_version, last_synced_at, active, sort_order, version) VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, NULL, ?, NULL, NULL, NULL, NULL, 'DDEX standards snapshot 2026-08-11', '2026-08-11'::date::timestamptz, TRUE, ?, 1) ON CONFLICT (standard_code, version_code) DO UPDATE SET name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, namespace_uri=EXCLUDED.namespace_uri, source_uri=EXCLUDED.source_uri, source_version=EXCLUDED.source_version, last_synced_at=EXCLUDED.last_synced_at, active=TRUE, sort_order=EXCLUDED.sort_order, version=ddex_standard_version.version+1 WHERE (ddex_standard_version.name_es, ddex_standard_version.name_en, ddex_standard_version.description_es, ddex_standard_version.description_en, ddex_standard_version.namespace_uri, ddex_standard_version.source_uri, ddex_standard_version.source_version, ddex_standard_version.last_synced_at, ddex_standard_version.active, ddex_standard_version.sort_order) IS DISTINCT FROM (EXCLUDED.name_es, EXCLUDED.name_en, EXCLUDED.description_es, EXCLUDED.description_en, EXCLUDED.namespace_uri, EXCLUDED.source_uri, EXCLUDED.source_version, EXCLUDED.last_synced_at, EXCLUDED.active, EXCLUDED.sort_order)"
      [ PersistText identifier, PersistText standardCode, PersistText versionCode
      , PersistText nameEs, PersistText nameEn, PersistText descriptionEs, PersistText descriptionEn
      , maybe PersistNull PersistText namespaceUri, PersistText sourceUri, PersistInt64 position
      ]
  forM_ standardSupports $ \(supportId, standardCode, versionCode, detectionEnabled, validationEnabled, importEnabled, exportEnabled) ->
    rawExecute
      "INSERT INTO ddex_standard_support (id, standard_version_id, deployment_code, detection_enabled, validation_enabled, import_enabled, export_enabled, active, updated_at, version) SELECT ?::uuid, standard.id, 'default', ?, ?, ?, ?, TRUE, CURRENT_TIMESTAMP, 1 FROM ddex_standard_version standard WHERE standard.standard_code=? AND standard.version_code=? ON CONFLICT (standard_version_id, deployment_code) DO UPDATE SET detection_enabled=EXCLUDED.detection_enabled, validation_enabled=EXCLUDED.validation_enabled, import_enabled=EXCLUDED.import_enabled, export_enabled=EXCLUDED.export_enabled, active=TRUE, updated_at=CURRENT_TIMESTAMP, version=ddex_standard_support.version+1 WHERE (ddex_standard_support.detection_enabled, ddex_standard_support.validation_enabled, ddex_standard_support.import_enabled, ddex_standard_support.export_enabled, ddex_standard_support.active) IS DISTINCT FROM (EXCLUDED.detection_enabled, EXCLUDED.validation_enabled, EXCLUDED.import_enabled, EXCLUDED.export_enabled, EXCLUDED.active)"
      [ PersistText supportId, PersistBool detectionEnabled, PersistBool validationEnabled
      , PersistBool importEnabled, PersistBool exportEnabled, PersistText standardCode, PersistText versionCode
      ]
  rawExecute
    "INSERT INTO ddex_message_type (id, standard_version_id, code, name_es, name_en, description_es, description_en, sort_order, active, deprecated_at, replacement_id, source_version, last_synced_at, runtime_supported, version) SELECT '40200000-0000-4000-8000-000000000001'::uuid, standard.id, 'NewReleaseMessage', 'Mensaje de nuevo lanzamiento', 'New Release Message', 'Mensaje ERN para comunicar lanzamientos y recursos.', 'ERN message used to communicate releases and resources.', 10, TRUE, NULL, NULL, 'ERN 4.3.2', '2026-08-11'::date::timestamptz, TRUE, 1 FROM ddex_standard_version standard WHERE standard.standard_code='ERN' AND standard.version_code='4.3.2' ON CONFLICT (standard_version_id, code) DO UPDATE SET name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, source_version=EXCLUDED.source_version, last_synced_at=EXCLUDED.last_synced_at, runtime_supported=TRUE, active=TRUE, version=ddex_message_type.version+1 WHERE (ddex_message_type.name_es, ddex_message_type.name_en, ddex_message_type.description_es, ddex_message_type.description_en, ddex_message_type.source_version, ddex_message_type.last_synced_at, ddex_message_type.runtime_supported, ddex_message_type.active) IS DISTINCT FROM (EXCLUDED.name_es, EXCLUDED.name_en, EXCLUDED.description_es, EXCLUDED.description_en, EXCLUDED.source_version, EXCLUDED.last_synced_at, EXCLUDED.runtime_supported, EXCLUDED.active)"
    []
  forM_ jobOperations $ \(identifier, code, nameEs, nameEn, position) ->
    rawExecute
      "INSERT INTO ddex_job_operation (id, code, name_es, name_en, description_es, description_en, active, sort_order, version) VALUES (?::uuid, ?, ?, ?, NULL, NULL, TRUE, ?, 1) ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es,name_en=EXCLUDED.name_en,active=TRUE,sort_order=EXCLUDED.sort_order,version=ddex_job_operation.version+1 WHERE (ddex_job_operation.name_es,ddex_job_operation.name_en,ddex_job_operation.active,ddex_job_operation.sort_order) IS DISTINCT FROM (EXCLUDED.name_es,EXCLUDED.name_en,EXCLUDED.active,EXCLUDED.sort_order)"
      [PersistText identifier, PersistText code, PersistText nameEs, PersistText nameEn, PersistInt64 position]
  forM_ importOperations $ \(identifier, code, nameEs, nameEn, position) ->
    rawExecute
      "INSERT INTO ddex_import_operation (id, code, name_es, name_en, description_es, description_en, active, sort_order, version) VALUES (?::uuid, ?, ?, ?, NULL, NULL, TRUE, ?, 1) ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es,name_en=EXCLUDED.name_en,active=TRUE,sort_order=EXCLUDED.sort_order,version=ddex_import_operation.version+1 WHERE (ddex_import_operation.name_es,ddex_import_operation.name_en,ddex_import_operation.active,ddex_import_operation.sort_order) IS DISTINCT FROM (EXCLUDED.name_es,EXCLUDED.name_en,EXCLUDED.active,EXCLUDED.sort_order)"
      [PersistText identifier, PersistText code, PersistText nameEs, PersistText nameEn, PersistInt64 position]
  forM_ validationResults $ \(identifier, code, nameEs, nameEn, position) ->
    seedValidationRegistry "ddex_validation_result" identifier code nameEs nameEn position
  forM_ validationSeverities $ \(identifier, code, nameEs, nameEn, position) ->
    seedValidationRegistry "ddex_validation_severity" identifier code nameEs nameEn position
  forM_ validationLayers $ \(identifier, code, nameEs, nameEn, position) ->
    seedValidationRegistry "ddex_validation_layer" identifier code nameEs nameEn position
  where
    ddexCatalogCodes = ["ddex-standard-versions", "ddex-message-types", "ddex-vocabularies", "ddex-codes"]
    standardVersions =
      [ ("40000000-0000-4000-8000-000000000001", "ERN", "4.3.2", "Notificación electrónica de lanzamientos 4.3.2", "Electronic Release Notification 4.3.2", "Versión vigente de ERN publicada por DDEX.", "Current ERN version published by DDEX.", Just "http://ddex.net/xml/ern/432", "https://kb.ddex.net/implementing-each-standard/electronic-release-notification-message-suite-%28ern%29/", 10)
      , ("40000000-0000-4000-8000-000000000002", "RIN", "2.1", "Notificación de información de grabación 2.1", "Recording Information Notification 2.1", "Versión vigente de RIN publicada por DDEX.", "Current RIN version published by DDEX.", Nothing, "https://kb.ddex.net/reference-material/standards-specifications/", 20)
      , ("40000000-0000-4000-8000-000000000003", "MEAD", "1.1", "Metadatos audiovisuales 1.1", "Media Enrichment and Description 1.1", "Versión vigente de MEAD publicada por DDEX.", "Current MEAD version published by DDEX.", Nothing, "https://kb.ddex.net/reference-material/standards-specifications/", 30)
      , ("40000000-0000-4000-8000-000000000004", "DSR", "1.4", "Arquitectura de reportes de ventas digitales 1.4", "Digital Sales Reporting architecture 1.4", "Arquitectura vigente de DSR; no representa un formato XML ejecutable.", "Current DSR architecture; this does not represent an executable XML format.", Nothing, "https://kb.ddex.net/reference-material/standards-specifications/", 40)
      ]
    standardSupports =
      [ ("40100000-0000-4000-8000-000000000001", "ERN", "4.3.2", True, True, True, True)
      , ("40100000-0000-4000-8000-000000000002", "RIN", "2.1", True, False, False, False)
      , ("40100000-0000-4000-8000-000000000003", "MEAD", "1.1", True, False, False, False)
      , ("40100000-0000-4000-8000-000000000004", "DSR", "1.4", False, False, False, False)
      ]
    jobOperations =
      [ ("40300000-0000-4000-8000-000000000001", "validate", "Validar", "Validate", 10)
      , ("40300000-0000-4000-8000-000000000002", "import", "Importar", "Import", 20)
      , ("40300000-0000-4000-8000-000000000003", "export", "Exportar", "Export", 30)
      , ("40300000-0000-4000-8000-000000000004", "cleanup", "Limpiar", "Cleanup", 40)
      ]
    importOperations =
      [ ("40400000-0000-4000-8000-000000000001", "create", "Crear", "Create", 10)
      , ("40400000-0000-4000-8000-000000000002", "update", "Actualizar", "Update", 20)
      , ("40400000-0000-4000-8000-000000000003", "skip", "Omitir", "Skip", 30)
      ]
    validationResults =
      [ ("40500000-0000-4000-8000-000000000001", "success", "Exitosa", "Successful", 10)
      , ("40500000-0000-4000-8000-000000000002", "failure", "Fallida", "Failed", 20)
      , ("40500000-0000-4000-8000-000000000003", "warning", "Con advertencias", "With warnings", 30)
      ]
    validationSeverities =
      [ ("40600000-0000-4000-8000-000000000001", "error", "Error", "Error", 10)
      , ("40600000-0000-4000-8000-000000000002", "warning", "Advertencia", "Warning", 20)
      , ("40600000-0000-4000-8000-000000000003", "info", "Información", "Information", 30)
      ]
    validationLayers =
      [ ("40700000-0000-4000-8000-000000000001", "xml", "XML", "XML", 10)
      , ("40700000-0000-4000-8000-000000000002", "xsd", "Esquema XSD", "XSD schema", 20)
      , ("40700000-0000-4000-8000-000000000003", "avs", "Valores permitidos", "Allowed values", 30)
      , ("40700000-0000-4000-8000-000000000004", "business", "Reglas de negocio", "Business rules", 40)
      ]
    seedValidationRegistry :: Text -> Text -> Text -> Text -> Text -> Int64 -> SqlPersistT IO ()
    seedValidationRegistry table identifier code nameEs nameEn position =
      rawExecute
        ("INSERT INTO " <> table <> " (id,code,name_es,name_en,description_es,description_en,active,sort_order,version) VALUES (?::uuid,?,?,?,NULL,NULL,TRUE,?,1) ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es,name_en=EXCLUDED.name_en,active=TRUE,sort_order=EXCLUDED.sort_order,version=" <> table <> ".version+1 WHERE (" <> table <> ".name_es," <> table <> ".name_en," <> table <> ".active," <> table <> ".sort_order) IS DISTINCT FROM (EXCLUDED.name_es,EXCLUDED.name_en,EXCLUDED.active,EXCLUDED.sort_order)")
        [PersistText identifier, PersistText code, PersistText nameEs, PersistText nameEn, PersistInt64 position]

seedDomainFoundation :: SqlPersistT IO ()
seedDomainFoundation = do
  seedFlatReference "release_type_reference" "release-types" releaseTypes
  seedFlatReference "recording_type_reference" "recording-types" recordingTypes
  seedFlatReference "recording_session_type" "recording-session-types" recordingSessionTypes
  seedHierarchical "genre" "genres" genres
  rawExecute
    "INSERT INTO artist_profile_genre_membership (artist_party_id, genre_id, sort_order, created_at) SELECT profile.artist_party_id, resolved.genre_id, legacy_value.position - 1, CURRENT_TIMESTAMP FROM artist_profile profile CROSS JOIN LATERAL regexp_split_to_table(profile.genres, '\\s*,\\s*') WITH ORDINALITY AS legacy_value(value, position) CROSS JOIN LATERAL (SELECT (array_agg(item.id))[1] AS genre_id FROM genre item WHERE item.active AND (lower(btrim(item.code))=lower(btrim(legacy_value.value)) OR lower(btrim(item.name_es))=lower(btrim(legacy_value.value)) OR lower(btrim(item.name_en))=lower(btrim(legacy_value.value))) HAVING count(*)=1) resolved WHERE profile.genres IS NOT NULL AND btrim(legacy_value.value)<>'' ON CONFLICT (artist_party_id, genre_id) DO NOTHING"
    []
  rawExecute
    "INSERT INTO fan_profile_genre_membership (fan_party_id, genre_id, sort_order, created_at) SELECT profile.fan_party_id, resolved.genre_id, legacy_value.position - 1, CURRENT_TIMESTAMP FROM fan_profile profile CROSS JOIN LATERAL regexp_split_to_table(profile.favorite_genres, '\\s*,\\s*') WITH ORDINALITY AS legacy_value(value, position) CROSS JOIN LATERAL (SELECT (array_agg(item.id))[1] AS genre_id FROM genre item WHERE item.active AND (lower(btrim(item.code))=lower(btrim(legacy_value.value)) OR lower(btrim(item.name_es))=lower(btrim(legacy_value.value)) OR lower(btrim(item.name_en))=lower(btrim(legacy_value.value))) HAVING count(*)=1) resolved WHERE profile.favorite_genres IS NOT NULL AND btrim(legacy_value.value)<>'' ON CONFLICT (fan_party_id, genre_id) DO NOTHING"
    []
  rawExecute
    "INSERT INTO artist_genre_membership (artist_id, genre_id, sort_order, created_at) SELECT legacy.artist_id, legacy.genre_id, row_number() OVER (PARTITION BY legacy.artist_id ORDER BY legacy.genre)::integer - 1, CURRENT_TIMESTAMP FROM artist_genre legacy JOIN genre canonical ON canonical.id=legacy.genre_id AND canonical.active WHERE legacy.genre_id IS NOT NULL ON CONFLICT (artist_id, genre_id) DO NOTHING"
    []
  seedHierarchical "instrument" "instruments" instruments
  seedServiceFoundation
  seedPipelineWorkflowBindings
  seedFlatCatalog "booking_type" "booking-types" bookingTypes
  seedFlatCatalog "event_type" "event-types" eventTypes
  rawExecute
    "INSERT INTO catalog_scoped_default (catalog_id, entity_id, scope_kind, scope_id, locale_id, effective_from, active, created_by, version) SELECT catalog.id, item.id, 'social-event', 'global', NULL, CURRENT_TIMESTAMP, TRUE, NULL, 1 FROM catalog_definition catalog JOIN event_type item ON item.catalog_id=catalog.id AND item.code='party' AND item.active WHERE catalog.code='event-types' AND NOT EXISTS (SELECT 1 FROM catalog_scoped_default existing WHERE existing.catalog_id=catalog.id AND existing.scope_kind='social-event' AND existing.scope_id='global' AND existing.locale_id IS NULL AND existing.active)"
    []
  forM_ (zip [0 :: Int ..] reactions) $ \(position, (identifier, code, emoji, nameEs, nameEn)) ->
    rawExecute
      "INSERT INTO reaction_type (id, catalog_id, code, emoji, name_es, name_en, current_slug, sort_order, active, workflow_state_id, version) SELECT ?::uuid, c.id, ?, ?, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='reaction-types' ON CONFLICT (code) DO UPDATE SET emoji=EXCLUDED.emoji, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en WHERE reaction_type.id=EXCLUDED.id AND reaction_type.version=1"
      [PersistText identifier, PersistText code, PersistText emoji, PersistText nameEs, PersistText nameEn, PersistText code, PersistInt64 (fromIntegral position)]
  forM_ (zip [0 :: Int ..] contentReactions) $ \(position, (identifier, code, emoji, nameEs, nameEn)) ->
    rawExecute
      "INSERT INTO content_reaction_type (id, catalog_id, code, emoji, name_es, name_en, current_slug, sort_order, active, workflow_state_id, version) SELECT ?::uuid, c.id, ?, ?, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='content-reaction-types' ON CONFLICT (code) DO UPDATE SET emoji=EXCLUDED.emoji, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en WHERE content_reaction_type.id=EXCLUDED.id AND content_reaction_type.version=1"
      [PersistText identifier, PersistText code, PersistText emoji, PersistText nameEs, PersistText nameEn, PersistText code, PersistInt64 (fromIntegral position)]
  forM_ (zip [0 :: Int ..] creatorBadges) $ \(position, (identifier, code, nameEs, nameEn)) ->
    rawExecute
      "INSERT INTO creator_badge_type (id, catalog_id, code, name_es, name_en, current_slug, sort_order, active, workflow_state_id, version) SELECT ?::uuid, c.id, ?, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='creator-badge-types' ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en WHERE creator_badge_type.id=EXCLUDED.id AND creator_badge_type.version=1"
      [PersistText identifier, PersistText code, PersistText nameEs, PersistText nameEn, PersistText code, PersistInt64 (fromIntegral position)]
  where
    releaseTypes =
      [ ("album", "Álbum", "Album"), ("single", "Sencillo", "Single"), ("ep", "EP", "EP")
      , ("compilation", "Compilación", "Compilation"), ("live-album", "Álbum en vivo", "Live album")
      , ("remix-album", "Álbum de remixes", "Remix album"), ("soundtrack", "Banda sonora", "Soundtrack")
      , ("spoken-word", "Palabra hablada", "Spoken word")
      ]
    recordingTypes = [("sound-recording", "Grabación sonora", "Sound recording"), ("music-video", "Video musical", "Music video")]
    recordingSessionTypes =
      [ ("band-recording", "Grabación de banda", "Band recording")
      , ("voice-recording", "Grabación de voz", "Voice recording")
      , ("recording", "Grabación", "Recording")
      , ("dj-rehearsal", "Ensayo de DJ", "DJ rehearsal")
      ]
    genres =
      [ ("rock", "Rock", "Rock"), ("pop", "Pop", "Pop"), ("hip-hop", "Hip-hop", "Hip-Hop")
      , ("jazz", "Jazz", "Jazz"), ("electronic", "Electrónica", "Electronic"), ("classical", "Clásica", "Classical")
      , ("country", "Country", "Country"), ("r-and-b", "R&B", "R&B"), ("latin", "Latina", "Latin")
      , ("indie", "Indie", "Indie"), ("alternative", "Alternativa", "Alternative"), ("metal", "Metal", "Metal")
      , ("soul", "Soul", "Soul"), ("blues", "Blues", "Blues"), ("reggae", "Reggae", "Reggae")
      , ("folk", "Folk", "Folk"), ("punk", "Punk", "Punk"), ("ambient", "Ambient", "Ambient")
      , ("rap", "Rap", "Rap"), ("ska", "Ska", "Ska"), ("funk", "Funk", "Funk")
      ]
    instruments =
      [ ("voice", "Voz", "Voice"), ("acoustic-guitar", "Guitarra acústica", "Acoustic guitar")
      , ("electric-guitar", "Guitarra eléctrica", "Electric guitar"), ("bass-guitar", "Bajo", "Bass guitar")
      , ("drums", "Batería", "Drums"), ("percussion", "Percusión", "Percussion")
      , ("piano", "Piano", "Piano"), ("keyboards", "Teclados", "Keyboards")
      , ("violin", "Violín", "Violin"), ("saxophone", "Saxofón", "Saxophone")
      , ("trumpet", "Trompeta", "Trumpet"), ("dj-controller", "Controlador DJ", "DJ controller")
      ]
    bookingTypes =
      [ ("studio-recording", "Grabación de estudio", "Studio recording")
      , ("rehearsal", "Ensayo", "Rehearsal"), ("course", "Curso", "Course")
      , ("event", "Evento", "Event"), ("live-session", "Sesión en vivo", "Live session")
      ]
    eventTypes =
      [ ("party", "Fiesta", "Party"), ("concert", "Concierto", "Concert")
      , ("festival", "Festival", "Festival"), ("conference", "Conferencia", "Conference")
      , ("showcase", "Showcase", "Showcase"), ("meeting", "Reunión", "Meeting")
      , ("workshop", "Taller", "Workshop"), ("recording-session", "Sesión de grabación", "Recording session")
      , ("rehearsal", "Ensayo", "Rehearsal"), ("livestream", "Transmisión en vivo", "Livestream")
      , ("wedding", "Boda", "Wedding"), ("corporate", "Evento corporativo", "Corporate event")
      , ("retreat", "Retiro", "Retreat"), ("photo-session", "Sesión fotográfica", "Photo session")
      , ("other", "Otro", "Other")
      ]
    reactions =
      [ ("50800000-0000-4000-8000-000000000001", "fire", "🔥", "Fuego", "Fire")
      , ("50800000-0000-4000-8000-000000000002", "love", "❤️", "Me encanta", "Love")
      , ("50800000-0000-4000-8000-000000000003", "applause", "👏", "Aplauso", "Applause")
      ]
    contentReactions =
      [ ("50900000-0000-4000-8000-000000000001", "fire", "🔥", "Fuego", "Fire")
      , ("50900000-0000-4000-8000-000000000002", "heart", "❤️", "Me encanta", "Love")
      , ("50900000-0000-4000-8000-000000000003", "clap", "👏", "Aplauso", "Applause")
      , ("50900000-0000-4000-8000-000000000004", "mic_drop", "🎤", "Mic drop", "Mic drop")
      , ("50900000-0000-4000-8000-000000000005", "skull", "💀", "Me muero", "I'm dead")
      ]
    creatorBadges =
      [ ("50a00000-0000-4000-8000-000000000001", "trendsetter", "Marcador de tendencia", "Trendsetter")
      , ("50a00000-0000-4000-8000-000000000002", "regular", "Miembro frecuente", "Regular")
      , ("50a00000-0000-4000-8000-000000000003", "og", "Miembro fundador", "Founding member")
      ]

-- These launch stations used to be a frontend constant. They are now
-- installation bootstrap records in the specialized radio_stream table.
-- Composite editorial labels are intentionally not guessed into a genre;
-- only the reviewed exact KEXP -> indie relation is seeded.
seedRadioStreams :: SqlPersistT IO ()
seedRadioStreams =
  forM_ stations $ \(streamUrl, stationName, countryCode, genreCode) ->
    rawExecute
      "INSERT INTO radio_stream (stream_url, name, country, country_id, genre, genre_id, is_active, last_checked_at, created_at, updated_at) VALUES (?, ?, NULL, (SELECT id FROM country_reference WHERE alpha2=? AND active AND deprecated_at IS NULL), NULL, (SELECT id FROM genre WHERE code=? AND active), TRUE, NULL, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP) ON CONFLICT (stream_url) DO UPDATE SET country_id=COALESCE(radio_stream.country_id, EXCLUDED.country_id), genre_id=COALESCE(radio_stream.genre_id, EXCLUDED.genre_id), updated_at=CASE WHEN (radio_stream.country_id IS NULL AND EXCLUDED.country_id IS NOT NULL) OR (radio_stream.genre_id IS NULL AND EXCLUDED.genre_id IS NOT NULL) THEN CURRENT_TIMESTAMP ELSE radio_stream.updated_at END"
      [ PersistText streamUrl
      , PersistText stationName
      , PersistText countryCode
      , maybe PersistNull PersistText genreCode
      ]
  where
    stations =
      [ ("https://icecast.radiofrance.fr/fip-midfi.mp3", "Cosmic Cycles", "FR", Nothing)
      , ("https://kexp-mp3-128.streamguys1.com/kexp128.mp3", "KEXP Seattle", "US", Just "indie")
      , ("https://fm939.wnyc.org/wnycfm-web", "WNYC FM", "US", Nothing)
      ]

-- Browser broadcast limits are product policy, not numeric implementation
-- constants. The seed only bootstraps a new installation; runtime selection,
-- labels, ordering, and the scoped default are database-authoritative.
seedRadioAutoStopOptions :: SqlPersistT IO ()
seedRadioAutoStopOptions = do
  forM_ (zip [0 :: Int ..] options) $ \(position, (code, nameEs, nameEn, descriptionEs, descriptionEn, durationMinutes)) ->
    rawExecute
      "INSERT INTO radio_auto_stop_option (catalog_id, code, name_es, name_en, description_es, description_en, current_slug, duration_minutes, sort_order, active, workflow_state_id, version) SELECT c.id, ?, ?, ?, ?, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='radio-auto-stop-options' ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, duration_minutes=EXCLUDED.duration_minutes WHERE radio_auto_stop_option.version=1"
      [ PersistText code
      , PersistText nameEs
      , PersistText nameEn
      , PersistText descriptionEs
      , PersistText descriptionEn
      , PersistText code
      , PersistInt64 (fromIntegral durationMinutes)
      , PersistInt64 (fromIntegral position)
      ]
  rawExecute
    "INSERT INTO catalog_scoped_default (catalog_id, entity_id, scope_kind, scope_id, locale_id, effective_from, active, created_by, version) SELECT c.id, option.id, 'radio-broadcast', 'global', NULL, CURRENT_TIMESTAMP, TRUE, NULL, 1 FROM catalog_definition c JOIN radio_auto_stop_option option ON option.catalog_id=c.id AND option.code='minutes-120' AND option.active WHERE c.code='radio-auto-stop-options' AND NOT EXISTS (SELECT 1 FROM catalog_scoped_default existing WHERE existing.catalog_id=c.id AND existing.scope_kind='radio-broadcast' AND existing.scope_id='global' AND existing.locale_id IS NULL AND existing.active)"
    []
  where
    options :: [(Text, Text, Text, Text, Text, Int)]
    options =
      [ ("unlimited", "Sin límite", "No limit", "La transmisión continúa hasta que se detenga manualmente.", "The broadcast continues until it is stopped manually.", 0)
      , ("minutes-30", "30 minutos", "30 minutes", "Detener la transmisión después de 30 minutos.", "Stop the broadcast after 30 minutes.", 30)
      , ("minutes-60", "60 minutos", "60 minutes", "Detener la transmisión después de 60 minutos.", "Stop the broadcast after 60 minutes.", 60)
      , ("minutes-90", "90 minutos", "90 minutes", "Detener la transmisión después de 90 minutos.", "Stop the broadcast after 90 minutes.", 90)
      , ("minutes-120", "120 minutos", "120 minutes", "Detener la transmisión después de 120 minutos.", "Stop the broadcast after 120 minutes.", 120)
      , ("minutes-180", "180 minutos", "180 minutes", "Detener la transmisión después de 180 minutos.", "Stop the broadcast after 180 minutes.", 180)
      ]

-- Renderer discriminants remain exhaustively recognized by web/mobile code,
-- while persisted rows own availability, bilingual labels, ordering, and the
-- application default. These values are emergency bootstrap data only.
seedAppearanceModes :: SqlPersistT IO ()
seedAppearanceModes = do
  forM_ (zip [0 :: Int ..] options) $ \(position, (code, nameEs, nameEn, descriptionEs, descriptionEn)) ->
    rawExecute
      "INSERT INTO appearance_mode_option (catalog_id, code, name_es, name_en, description_es, description_en, current_slug, sort_order, active, workflow_state_id, version) SELECT c.id, ?, ?, ?, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='appearance-modes' ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en WHERE appearance_mode_option.version=1"
      [ PersistText code
      , PersistText nameEs
      , PersistText nameEn
      , PersistText descriptionEs
      , PersistText descriptionEn
      , PersistText code
      , PersistInt64 (fromIntegral position)
      ]
  rawExecute
    "INSERT INTO catalog_scoped_default (catalog_id, entity_id, scope_kind, scope_id, locale_id, effective_from, active, created_by, version) SELECT c.id, option.id, 'appearance-mode', 'global', NULL, CURRENT_TIMESTAMP, TRUE, NULL, 1 FROM catalog_definition c JOIN appearance_mode_option option ON option.catalog_id=c.id AND option.code='system' AND option.active WHERE c.code='appearance-modes' AND NOT EXISTS (SELECT 1 FROM catalog_scoped_default existing WHERE existing.catalog_id=c.id AND existing.scope_kind='appearance-mode' AND existing.scope_id='global' AND existing.locale_id IS NULL AND existing.active)"
    []
  where
    options :: [(Text, Text, Text, Text, Text)]
    options =
      [ ("system", "Usar configuración del sistema", "Use system setting", "Sigue automáticamente la apariencia clara u oscura del dispositivo.", "Automatically follows the device light or dark appearance.")
      , ("light", "Tema claro", "Light theme", "Usa siempre la apariencia clara.", "Always use the light appearance.")
      , ("dark", "Tema oscuro", "Dark theme", "Usa siempre la apariencia oscura.", "Always use the dark appearance.")
      ]

-- Feedback categories and severities are product-governed values. Explicit
-- UUIDs make installation seeds and production backfills converge on the same
-- immutable identities without relying on labels, codes, or insertion order.
seedFeedbackCatalogs :: SqlPersistT IO ()
seedFeedbackCatalogs = do
  forM_ (zip [0 :: Int ..] categories) $ \(position, (itemId, code, nameEs, nameEn)) ->
    rawExecute
      "INSERT INTO feedback_category (id, catalog_id, code, name_es, name_en, current_slug, sort_order, active, workflow_state_id, published_revision, version) SELECT ?::uuid, c.id, ?, ?, ?, ?, ?, TRUE, ws.id, 1, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='feedback-categories' ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en WHERE feedback_category.id=EXCLUDED.id AND feedback_category.version=1"
      [PersistText itemId, PersistText code, PersistText nameEs, PersistText nameEn, PersistText code, PersistInt64 (fromIntegral position)]
  forM_ (zip [0 :: Int ..] severities) $ \(position, (itemId, code, nameEs, nameEn)) ->
    rawExecute
      "INSERT INTO feedback_severity (id, catalog_id, code, name_es, name_en, current_slug, sort_order, active, workflow_state_id, published_revision, version) SELECT ?::uuid, c.id, ?, ?, ?, ?, ?, TRUE, ws.id, 1, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='feedback-severities' ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en WHERE feedback_severity.id=EXCLUDED.id AND feedback_severity.version=1"
      [PersistText itemId, PersistText code, PersistText nameEs, PersistText nameEn, PersistText code, PersistInt64 (fromIntegral position)]
  rawExecute
    "INSERT INTO catalog_scoped_default (catalog_id, entity_id, scope_kind, scope_id, locale_id, effective_from, active, created_by, version) SELECT c.id, item.id, 'feedback-category', 'global', NULL, CURRENT_TIMESTAMP, TRUE, NULL, 1 FROM catalog_definition c JOIN feedback_category item ON item.catalog_id=c.id AND item.code='bug' AND item.active WHERE c.code='feedback-categories' AND NOT EXISTS (SELECT 1 FROM catalog_scoped_default existing WHERE existing.catalog_id=c.id AND existing.scope_kind='feedback-category' AND existing.scope_id='global' AND existing.locale_id IS NULL AND existing.active)"
    []
  rawExecute
    "INSERT INTO catalog_scoped_default (catalog_id, entity_id, scope_kind, scope_id, locale_id, effective_from, active, created_by, version) SELECT c.id, item.id, 'feedback-severity', 'global', NULL, CURRENT_TIMESTAMP, TRUE, NULL, 1 FROM catalog_definition c JOIN feedback_severity item ON item.catalog_id=c.id AND item.code='p2' AND item.active WHERE c.code='feedback-severities' AND NOT EXISTS (SELECT 1 FROM catalog_scoped_default existing WHERE existing.catalog_id=c.id AND existing.scope_kind='feedback-severity' AND existing.scope_id='global' AND existing.locale_id IS NULL AND existing.active)"
    []
  where
    categories =
      [ ("31000000-0000-4000-8000-000000000001", "bug", "Bug", "Bug")
      , ("31000000-0000-4000-8000-000000000002", "idea", "Idea", "Idea")
      , ("31000000-0000-4000-8000-000000000003", "ux", "UX", "UX")
      , ("31000000-0000-4000-8000-000000000004", "datos", "Datos", "Data")
      , ("31000000-0000-4000-8000-000000000005", "suggestion", "Sugerencia", "Suggestion")
      , ("31000000-0000-4000-8000-000000000006", "question", "Pregunta", "Question")
      , ("31000000-0000-4000-8000-000000000007", "accessibility", "Accesibilidad", "Accessibility")
      , ("31000000-0000-4000-8000-000000000008", "permissions", "Permisos", "Permissions")
      , ("31000000-0000-4000-8000-000000000009", "performance", "Rendimiento", "Performance")
      , ("31000000-0000-4000-8000-000000000010", "content_translation", "Contenido o traducción", "Content or translation")
      ]
    severities =
      [ ("32000000-0000-4000-8000-000000000001", "p1", "P1 - Crítico", "P1 - Critical")
      , ("32000000-0000-4000-8000-000000000002", "p2", "P2 - Alto", "P2 - High")
      , ("32000000-0000-4000-8000-000000000003", "p3", "P3 - Medio", "P3 - Medium")
      , ("32000000-0000-4000-8000-000000000004", "p4", "P4 - Bajo", "P4 - Low")
      ]

seedFlatReference :: Text -> Text -> [(Text, Text, Text)] -> SqlPersistT IO ()
seedFlatReference table catalogCode values =
  forM_ (zip [0 :: Int ..] values) $ \(position, (code, nameEs, nameEn)) ->
    rawExecute
      ("INSERT INTO " <> table <> " (catalog_id, code, name_es, name_en, sort_order, active, workflow_state_id, version) SELECT c.id, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code=? ON CONFLICT (code) DO NOTHING")
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistInt64 (fromIntegral position), PersistText catalogCode]

seedHierarchical :: Text -> Text -> [(Text, Text, Text)] -> SqlPersistT IO ()
seedHierarchical table catalogCode values =
  forM_ (zip [0 :: Int ..] values) $ \(position, (code, nameEs, nameEn)) ->
    rawExecute
      ("INSERT INTO " <> table <> " (catalog_id, code, name_es, name_en, current_slug, sort_order, active, workflow_state_id, published_revision, version) SELECT c.id, ?, ?, ?, ?, ?, TRUE, ws.id, 1, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code=? ON CONFLICT (code) DO NOTHING")
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistText code, PersistInt64 (fromIntegral position), PersistText catalogCode]

seedFlatCatalog :: Text -> Text -> [(Text, Text, Text)] -> SqlPersistT IO ()
seedFlatCatalog table catalogCode values =
  forM_ (zip [0 :: Int ..] values) $ \(position, (code, nameEs, nameEn)) ->
    rawExecute
      ("INSERT INTO " <> table <> " (catalog_id, code, name_es, name_en, current_slug, sort_order, active, workflow_state_id, published_revision, version) SELECT c.id, ?, ?, ?, ?, ?, TRUE, ws.id, 1, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code=? ON CONFLICT (code) DO NOTHING")
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistText code, PersistInt64 (fromIntegral position), PersistText catalogCode]

seedServiceFoundation :: SqlPersistT IO ()
seedServiceFoundation = do
  forM_ (zip [0 :: Int ..] categories) $ \(position, (code, nameEs, nameEn)) ->
    rawExecute
      "INSERT INTO service_category (catalog_id, code, name_es, name_en, current_slug, sort_order, active, workflow_state_id, version) SELECT c.id, ?, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='service-categories' ON CONFLICT (code) DO NOTHING"
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistText code, PersistInt64 (fromIntegral position)]
  forM_ (zip [0 :: Int ..] pricingModels) $ \(position, (code, nameEs, nameEn)) ->
    rawExecute
      "INSERT INTO service_pricing_model (catalog_id, code, name_es, name_en, current_slug, sort_order, active, workflow_state_id, version) SELECT c.id, ?, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='service-pricing-models' ON CONFLICT (code) DO NOTHING"
      [PersistText code, PersistText nameEs, PersistText nameEn, PersistText code, PersistInt64 (fromIntegral position)]
  forM_ (zip [0 :: Int ..] resourceSelectionModes) $ \(position, (code, nameEs, nameEn, descriptionEs, descriptionEn)) ->
    rawExecute
      "INSERT INTO service_resource_selection_mode (catalog_id, code, name_es, name_en, description_es, description_en, current_slug, sort_order, active, workflow_state_id, version) SELECT c.id, ?, ?, ?, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition c JOIN workflow_state ws ON ws.workflow_id=c.workflow_id AND ws.code='published' WHERE c.code='service-resource-selection-modes' ON CONFLICT (code) DO UPDATE SET name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en WHERE service_resource_selection_mode.version=1"
      [ PersistText code, PersistText nameEs, PersistText nameEn, PersistText descriptionEs
      , PersistText descriptionEn, PersistText code, PersistInt64 (fromIntegral position)
      ]
  rawExecute
    "UPDATE service_offering_default_resource relationship SET selection_mode_id=mode.id FROM service_resource_selection_mode mode WHERE relationship.selection_mode_id IS NULL AND relationship.selection_mode=mode.code"
    []
  rawExecute
    "ALTER TABLE service_offering_default_resource VALIDATE CONSTRAINT service_offering_default_resource_canonical_selection_mode_check"
    []
  forM_ (zip [0 :: Int ..] offerings) $ \(position, (code, categoryCode, nameEs, nameEn, pricingModel, billingEs, billingEn, defaultDurationMinutes, requiresEngineer)) ->
    rawExecute
      "INSERT INTO service_offering (catalog_id, category_id, code, name_es, name_en, current_slug, pricing_model_id, pricing_model_code, currency_id, billing_unit_es, billing_unit_en, default_duration_minutes, requires_engineer, sort_order, active, workflow_state_id, version) SELECT catalog.id, category.id, ?, ?, ?, ?, pricing.id, NULL, currency.id, ?, ?, ?, ?, ?, TRUE, ws.id, 1 FROM catalog_definition catalog JOIN workflow_state ws ON ws.workflow_id=catalog.workflow_id AND ws.code='published' JOIN service_category category ON category.code=? AND category.active JOIN service_pricing_model pricing ON pricing.code=? AND pricing.active JOIN currency_reference currency ON currency.code='USD' AND currency.active WHERE catalog.code='services' ON CONFLICT (code) DO UPDATE SET pricing_model_id=EXCLUDED.pricing_model_id, pricing_model_code=NULL, requires_engineer=EXCLUDED.requires_engineer, default_duration_minutes=EXCLUDED.default_duration_minutes WHERE service_offering.pricing_model_id IS DISTINCT FROM EXCLUDED.pricing_model_id OR service_offering.pricing_model_code IS NOT NULL OR service_offering.requires_engineer IS DISTINCT FROM EXCLUDED.requires_engineer OR service_offering.default_duration_minutes IS DISTINCT FROM EXCLUDED.default_duration_minutes"
      [ PersistText code, PersistText nameEs, PersistText nameEn, PersistText code
      , maybe PersistNull PersistText billingEs, maybe PersistNull PersistText billingEn
      , maybe PersistNull PersistInt64 defaultDurationMinutes
      , PersistBool requiresEngineer, PersistInt64 (fromIntegral position), PersistText categoryCode, PersistText pricingModel
      ]
  where
    categories =
      [ ("recording", "Grabación", "Recording"), ("mixing", "Mezcla", "Mixing")
      , ("mastering", "Masterización", "Mastering"), ("rehearsal", "Ensayo", "Rehearsal")
      , ("education", "Educación", "Education"), ("event-production", "Producción de eventos", "Event production")
      ]
    pricingModels =
      [ ("hourly", "Por hora", "Hourly")
      , ("per-song", "Por canción", "Per song")
      , ("per-episode", "Por episodio", "Per episode")
      , ("package", "Paquete", "Package")
      , ("quote", "Cotización", "Quote")
      ]
    resourceSelectionModes =
      [ ("all", "Todos los recursos", "All resources", "Reserva todos los recursos configurados.", "Reserve every configured resource.")
      , ("first-available", "Primer recurso disponible", "First available resource", "Reserva la primera alternativa disponible según el orden configurado.", "Reserve the first available alternative in configured order.")
      ]
    offerings =
      [ ("band-recording", "recording", "Grabación de banda", "Band recording", "hourly", Just "hora", Just "hour", Just 120, True)
      , ("voice-recording", "recording", "Grabación de voz", "Voice recording", "hourly", Just "hora", Just "hour", Just 120, True)
      , ("recording", "recording", "Grabación", "Recording", "hourly", Just "hora", Just "hour", Just 120, True)
      , ("audiovisual-live-recording", "recording", "Grabación audiovisual en vivo", "Live audiovisual recording", "quote", Nothing, Nothing, Just 120, True)
      , ("podcast-recording", "recording", "Podcast", "Podcast recording", "per-episode", Just "episodio", Just "episode", Just 60, True)
      , ("mixing", "mixing", "Mezcla", "Mixing", "per-song", Just "canción", Just "song", Just 120, True)
      , ("mastering", "mastering", "Masterización", "Mastering", "per-song", Just "canción", Just "song", Just 120, True)
      , ("rehearsal", "rehearsal", "Ensayo", "Rehearsal", "hourly", Just "hora", Just "hour", Just 90, False)
      , ("dj-booth-practice", "rehearsal", "Práctica en cabina DJ", "DJ booth practice", "hourly", Just "hora", Just "hour", Just 60, False)
      , ("classes", "education", "Clases", "Classes", "package", Just "paquete", Just "package", Just 60, False)
      , ("event-production", "event-production", "Producción de eventos", "Event production", "quote", Nothing, Nothing, Just 120, False)
      ]

seedPipelineWorkflowBindings :: SqlPersistT IO ()
seedPipelineWorkflowBindings =
  forM_ bindings $ \(serviceCode, workflowCode) ->
    rawExecute
      "INSERT INTO pipeline_workflow_binding (service_offering_id, workflow_id, active, version) SELECT service.id, workflow.id, TRUE, 1 FROM service_offering service CROSS JOIN workflow_definition workflow WHERE service.code=? AND service.active AND workflow.code=? AND workflow.active ON CONFLICT (service_offering_id) DO UPDATE SET workflow_id=EXCLUDED.workflow_id, active=TRUE, updated_at=CURRENT_TIMESTAMP, version=pipeline_workflow_binding.version+1 WHERE pipeline_workflow_binding.workflow_id IS DISTINCT FROM EXCLUDED.workflow_id OR pipeline_workflow_binding.active=FALSE"
      [PersistText serviceCode, PersistText workflowCode]
  where
    bindings =
      [ ("band-recording", "pipeline-recording")
      , ("voice-recording", "pipeline-recording")
      , ("recording", "pipeline-recording")
      , ("audiovisual-live-recording", "pipeline-recording")
      , ("podcast-recording", "pipeline-recording")
      , ("mixing", "pipeline-mixing")
      , ("mastering", "pipeline-mastering")
      , ("rehearsal", "pipeline-rehearsal")
      , ("dj-booth-practice", "pipeline-rehearsal")
      , ("classes", "pipeline-classes")
      , ("event-production", "pipeline-event-production")
      ]

validateCatalogRuntimeRegistries :: SqlPersistT IO ()
validateCatalogRuntimeRegistries = do
  appearanceRows <-
    ( rawSql
        "SELECT option.code FROM appearance_mode_option option JOIN catalog_definition catalog ON catalog.id=option.catalog_id WHERE catalog.code='appearance-modes' ORDER BY option.code"
        []
        :: SqlPersistT IO [Single Text]
    )
  let appearanceCodes = sort [code | Single code <- appearanceRows]
      recognizedAppearanceCodes = ["dark", "light", "system"]
  unless (appearanceCodes == recognizedAppearanceCodes) $
    liftIO . ioError . userError $
      "Persisted appearance modes do not match executable renderer capabilities: "
        <> show appearanceCodes
  appearanceDefaults <-
    ( rawSql
        "SELECT COUNT(*) FROM catalog_scoped_default scoped_default JOIN catalog_definition catalog ON catalog.id=scoped_default.catalog_id JOIN appearance_mode_option option ON option.id=scoped_default.entity_id AND option.catalog_id=catalog.id JOIN workflow_state workflow_state ON workflow_state.id=option.workflow_state_id WHERE catalog.code='appearance-modes' AND scoped_default.scope_kind='appearance-mode' AND scoped_default.scope_id='global' AND scoped_default.locale_id IS NULL AND scoped_default.active=TRUE AND (scoped_default.effective_from IS NULL OR scoped_default.effective_from<=CURRENT_TIMESTAMP) AND (scoped_default.effective_until IS NULL OR scoped_default.effective_until>CURRENT_TIMESTAMP) AND option.active=TRUE AND option.deprecated_at IS NULL AND workflow_state.code='published'"
        []
        :: SqlPersistT IO [Single Int]
    )
  case appearanceDefaults of
    [Single 1] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Persisted appearance modes require exactly one active global default: "
        <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate the persisted appearance-mode default")
  feedbackDefaults <-
    ( rawSql
        "SELECT COUNT(*) FROM catalog_scoped_default scoped_default JOIN catalog_definition catalog ON catalog.id=scoped_default.catalog_id LEFT JOIN feedback_category category ON catalog.entity_kind='feedback_category' AND category.id=scoped_default.entity_id AND category.catalog_id=catalog.id LEFT JOIN feedback_severity severity ON catalog.entity_kind='feedback_severity' AND severity.id=scoped_default.entity_id AND severity.catalog_id=catalog.id LEFT JOIN workflow_state workflow_state ON workflow_state.id=COALESCE(category.workflow_state_id,severity.workflow_state_id) WHERE ((catalog.code='feedback-categories' AND scoped_default.scope_kind='feedback-category' AND category.active AND category.deprecated_at IS NULL) OR (catalog.code='feedback-severities' AND scoped_default.scope_kind='feedback-severity' AND severity.active AND severity.deprecated_at IS NULL)) AND scoped_default.scope_id='global' AND scoped_default.locale_id IS NULL AND scoped_default.active=TRUE AND (scoped_default.effective_from IS NULL OR scoped_default.effective_from<=CURRENT_TIMESTAMP) AND (scoped_default.effective_until IS NULL OR scoped_default.effective_until>CURRENT_TIMESTAMP) AND workflow_state.code='published'"
        []
        :: SqlPersistT IO [Single Int]
    )
  case feedbackDefaults of
    [Single 2] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Persisted feedback catalogs require one active global default each: "
        <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate persisted feedback defaults")
  modeRows <-
    ( rawSql
        "SELECT code FROM service_resource_selection_mode WHERE active=TRUE ORDER BY code"
        []
        :: SqlPersistT IO [Single Text]
    )
  let activeModeCodes = sort [code | Single code <- modeRows]
      recognizedModeCodes = ["all", "first-available"]
  unless (activeModeCodes == recognizedModeCodes) $
    liftIO . ioError . userError $
      "Persisted service resource selection modes do not match executable capabilities: "
        <> show activeModeCodes
  invalidRelationships <-
    ( rawSql
        "SELECT COUNT(*) FROM service_offering_default_resource relationship LEFT JOIN service_resource_selection_mode mode ON mode.id=relationship.selection_mode_id WHERE relationship.active=TRUE AND (mode.id IS NULL OR mode.active=FALSE OR mode.code NOT IN ('all','first-available'))"
        []
        :: SqlPersistT IO [Single Int]
    )
  case invalidRelationships of
    [Single 0] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Active service default-resource relationships have invalid canonical selection modes: "
        <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate service resource selection relationships")
  invalidInputRows <-
    ( rawSql
        "SELECT COUNT(*) FROM input_row WHERE instrument IS NOT NULL OR instrument_id IS NULL OR mic_id IS NULL"
        []
        :: SqlPersistT IO [Single Int]
    )
  case invalidInputRows of
    [Single 0] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Input rows require canonical instrument_id and mic_id before cutover: " <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate canonical input-row references")
  legacyLiveSessionReferences <-
    ( rawSql
        "SELECT (SELECT COUNT(*) FROM live_session_musician WHERE instrument IS NOT NULL OR role IS NOT NULL) + (SELECT COUNT(*) FROM live_session_intake WHERE primary_genre IS NOT NULL)"
        []
        :: SqlPersistT IO [Single Int]
    )
  case legacyLiveSessionReferences of
    [Single 0] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Live-session catalog relationships require canonical IDs before cutover: " <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate live-session catalog references")
  invalidFeedbackReferences <-
    ( rawSql
        "SELECT COUNT(*) FROM feedback item LEFT JOIN feedback_category category ON category.id=item.category_id LEFT JOIN workflow_state category_state ON category_state.id=category.workflow_state_id LEFT JOIN feedback_severity severity ON severity.id=item.severity_id LEFT JOIN workflow_state severity_state ON severity_state.id=severity.workflow_state_id WHERE item.category IS NOT NULL OR item.severity IS NOT NULL OR item.category_id IS NULL OR item.severity_id IS NULL OR category.active IS DISTINCT FROM TRUE OR category.deprecated_at IS NOT NULL OR category_state.code IS DISTINCT FROM 'published' OR severity.active IS DISTINCT FROM TRUE OR severity.deprecated_at IS NOT NULL OR severity_state.code IS DISTINCT FROM 'published'"
        []
        :: SqlPersistT IO [Single Int]
    )
  case invalidFeedbackReferences of
    [Single 0] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Feedback requires canonical category_id and severity_id before cutover: " <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate canonical feedback references")
  eventTypeDefaults <-
    ( rawSql
        "SELECT COUNT(*) FROM catalog_scoped_default scoped_default JOIN catalog_definition catalog ON catalog.id=scoped_default.catalog_id JOIN event_type item ON item.id=scoped_default.entity_id AND item.catalog_id=catalog.id JOIN workflow_state state ON state.id=item.workflow_state_id WHERE catalog.code='event-types' AND scoped_default.scope_kind='social-event' AND scoped_default.scope_id='global' AND scoped_default.locale_id IS NULL AND scoped_default.active=TRUE AND (scoped_default.effective_from IS NULL OR scoped_default.effective_from<=CURRENT_TIMESTAMP) AND (scoped_default.effective_until IS NULL OR scoped_default.effective_until>CURRENT_TIMESTAMP) AND item.active=TRUE AND item.deprecated_at IS NULL AND state.code='published'"
        []
        :: SqlPersistT IO [Single Int]
    )
  case eventTypeDefaults of
    [Single 1] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Persisted event types require exactly one active social-event default: " <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate the social-event type default")
  invalidSocialEventTypes <-
    ( rawSql
        "SELECT COUNT(*) FROM social_event event LEFT JOIN event_type item ON item.id=event.event_type_id LEFT JOIN catalog_definition catalog ON catalog.id=item.catalog_id LEFT JOIN workflow_state state ON state.id=item.workflow_state_id WHERE event.event_type_id IS NULL OR item.id IS NULL OR item.active IS DISTINCT FROM TRUE OR item.deprecated_at IS NOT NULL OR catalog.code IS DISTINCT FROM 'event-types' OR catalog.active IS DISTINCT FROM TRUE OR state.code IS DISTINCT FROM 'published' OR state.active IS DISTINCT FROM TRUE OR state.workflow_id IS DISTINCT FROM catalog.workflow_id OR item.effective_from>CURRENT_DATE OR item.effective_until<CURRENT_DATE OR (event.metadata IS NOT NULL AND event.metadata::jsonb ?? 'eventType')"
        []
        :: SqlPersistT IO [Single Int]
    )
  case invalidSocialEventTypes of
    [Single 0] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Social events require canonical event_type_id without metadata eventType strings before cutover: " <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate canonical social-event type references")
  invalidMomentReactionReferences <-
    ( rawSql
        "SELECT COUNT(*) FROM event_moment_reaction reaction LEFT JOIN reaction_type item ON item.id=reaction.reaction_type_id LEFT JOIN catalog_definition catalog ON catalog.id=item.catalog_id LEFT JOIN workflow_state state ON state.id=item.workflow_state_id WHERE reaction.reaction IS NOT NULL OR reaction.reaction_type_id IS NULL OR item.id IS NULL OR item.active IS DISTINCT FROM TRUE OR item.deprecated_at IS NOT NULL OR catalog.code IS DISTINCT FROM 'reaction-types' OR catalog.active IS DISTINCT FROM TRUE OR state.code IS DISTINCT FROM 'published' OR state.active IS DISTINCT FROM TRUE OR state.workflow_id IS DISTINCT FROM catalog.workflow_id"
        []
        :: SqlPersistT IO [Single Int]
    )
  case invalidMomentReactionReferences of
    [Single 0] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Event moment reactions require canonical reaction_type_id without copied reaction strings before cutover: " <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate canonical event-moment reaction references")
  invalidContentReactionReferences <-
    ( rawSql
        "SELECT (SELECT COUNT(*) FROM fan_club_post_reaction reaction LEFT JOIN content_reaction_type item ON item.id=reaction.reaction_type_id LEFT JOIN catalog_definition catalog ON catalog.id=item.catalog_id LEFT JOIN workflow_state state ON state.id=item.workflow_state_id WHERE item.id IS NULL OR item.active IS DISTINCT FROM TRUE OR item.deprecated_at IS NOT NULL OR catalog.code IS DISTINCT FROM 'content-reaction-types' OR catalog.active IS DISTINCT FROM TRUE OR state.code IS DISTINCT FROM 'published' OR state.active IS DISTINCT FROM TRUE OR state.workflow_id IS DISTINCT FROM catalog.workflow_id) + (SELECT COUNT(*) FROM fan_club_memory_reaction reaction LEFT JOIN content_reaction_type item ON item.id=reaction.reaction_type_id LEFT JOIN catalog_definition catalog ON catalog.id=item.catalog_id LEFT JOIN workflow_state state ON state.id=item.workflow_state_id WHERE item.id IS NULL OR item.active IS DISTINCT FROM TRUE OR item.deprecated_at IS NOT NULL OR catalog.code IS DISTINCT FROM 'content-reaction-types' OR catalog.active IS DISTINCT FROM TRUE OR state.code IS DISTINCT FROM 'published' OR state.active IS DISTINCT FROM TRUE OR state.workflow_id IS DISTINCT FROM catalog.workflow_id)"
        []
        :: SqlPersistT IO [Single Int]
    )
  case invalidContentReactionReferences of
    [Single 0] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Fan club content reactions require specialized canonical reaction type references: " <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate canonical content reaction references")
  rawExecute
    "DO $$ DECLARE legacy_rows bigint; BEGIN IF to_regclass('public.content_reaction') IS NOT NULL THEN EXECUTE 'SELECT COUNT(*) FROM content_reaction' INTO legacy_rows; IF legacy_rows<>0 THEN RAISE EXCEPTION 'legacy content_reaction rows must pass the guarded foreign-key cutover before startup: %',legacy_rows USING ERRCODE='23514'; END IF; END IF; END $$"
    []
  rawExecute
    "WITH counts AS (SELECT item_id,COUNT(*)::bigint AS usage_count FROM (SELECT reaction_type_id AS item_id FROM fan_club_post_reaction UNION ALL SELECT reaction_type_id FROM fan_club_memory_reaction) usage_refs GROUP BY item_id) UPDATE content_reaction_type item SET usage_count=COALESCE(counts.usage_count,0) FROM counts WHERE item.id=counts.item_id AND item.usage_count IS DISTINCT FROM counts.usage_count"
    []
  rawExecute
    "UPDATE content_reaction_type item SET usage_count=0 WHERE item.usage_count<>0 AND NOT EXISTS (SELECT 1 FROM fan_club_post_reaction reaction WHERE reaction.reaction_type_id=item.id) AND NOT EXISTS (SELECT 1 FROM fan_club_memory_reaction reaction WHERE reaction.reaction_type_id=item.id)"
    []
  invalidCreatorBadgeReferences <-
    ( rawSql
        "SELECT COUNT(*) FROM creator_badge badge LEFT JOIN creator_badge_type item ON item.id=badge.badge_type_id LEFT JOIN catalog_definition catalog ON catalog.id=item.catalog_id LEFT JOIN workflow_state state ON state.id=item.workflow_state_id WHERE item.id IS NULL OR item.active IS DISTINCT FROM TRUE OR item.deprecated_at IS NOT NULL OR catalog.code IS DISTINCT FROM 'creator-badge-types' OR catalog.active IS DISTINCT FROM TRUE OR state.code IS DISTINCT FROM 'published' OR state.active IS DISTINCT FROM TRUE OR state.workflow_id IS DISTINCT FROM catalog.workflow_id"
        []
        :: SqlPersistT IO [Single Int]
    )
  case invalidCreatorBadgeReferences of
    [Single 0] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "Creator badges require canonical active published badge type references: " <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate canonical creator badge references")
  rawExecute
    "WITH counts AS (SELECT badge_type_id,count(*)::bigint AS usage_count FROM creator_badge GROUP BY badge_type_id) UPDATE creator_badge_type item SET usage_count=counts.usage_count FROM counts WHERE item.id=counts.badge_type_id AND item.usage_count IS DISTINCT FROM counts.usage_count"
    []
  rawExecute
    "UPDATE creator_badge_type item SET usage_count=0 WHERE item.usage_count<>0 AND NOT EXISTS (SELECT 1 FROM creator_badge badge WHERE badge.badge_type_id=item.id)"
    []
  lifecycleStateRows <-
    ( rawSql
        "SELECT state.code FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code='social-event-lifecycle' AND workflow.active=TRUE AND state.active=TRUE ORDER BY state.code"
        []
        :: SqlPersistT IO [Single Text]
    )
  let lifecycleCodes = sort [code | Single code <- lifecycleStateRows]
      recognizedLifecycleCodes = sort recognizedSocialEventStateCodes
  unless (all (`elem` lifecycleCodes) recognizedLifecycleCodes) $
    liftIO . ioError . userError $
      "Persisted social-event states are missing code-recognized parser states: "
        <> show (filter (`notElem` lifecycleCodes) recognizedLifecycleCodes)
  initialLifecycleStates <-
    ( rawSql
        "SELECT COUNT(*) FROM workflow_default_state default_state JOIN workflow_definition workflow ON workflow.id=default_state.workflow_id JOIN workflow_state state ON state.id=default_state.state_id AND state.workflow_id=workflow.id WHERE workflow.code='social-event-lifecycle' AND workflow.active=TRUE AND default_state.context='initial' AND default_state.active=TRUE AND state.active=TRUE"
        []
        :: SqlPersistT IO [Single Int]
    )
  unless (initialLifecycleStates == [Single 1]) $
    liftIO . ioError . userError $ "Persisted social-event lifecycle requires exactly one active initial state: " <> show initialLifecycleStates
  unknownLifecycleCapabilities <-
    ( rawSql
        "SELECT COUNT(*) FROM workflow_state_capability capability JOIN workflow_state state ON state.id=capability.state_id JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code=? AND capability.enabled=TRUE AND NOT (capability.capability_code = ANY(?))"
        [PersistText socialEventWorkflowCode, PersistArray (map PersistText recognizedSocialEventCapabilityCodes)]
        :: SqlPersistT IO [Single Int]
    )
  unless (unknownLifecycleCapabilities == [Single 0]) $
    liftIO . ioError . userError $ "Persisted social-event lifecycle contains unknown enabled capabilities: " <> show unknownLifecycleCapabilities
  invalidSocialEventStates <-
    ( rawSql
        "SELECT COUNT(*) FROM social_event event LEFT JOIN workflow_state state ON state.id=event.workflow_state_id LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE event.workflow_state_id IS NULL OR state.id IS NULL OR state.active IS DISTINCT FROM TRUE OR workflow.code IS DISTINCT FROM 'social-event-lifecycle' OR workflow.active IS DISTINCT FROM TRUE OR (event.metadata IS NOT NULL AND event.metadata::jsonb ?? 'eventStatus')"
        []
        :: SqlPersistT IO [Single Int]
    )
  unless (invalidSocialEventStates == [Single 0]) $
    liftIO . ioError . userError $
      "Social events require canonical workflow_state_id without metadata eventStatus strings before cutover: " <> show invalidSocialEventStates
  pipelineRegistryCounts <-
    ( rawSql
        "SELECT (SELECT COUNT(*) FROM workflow_definition WHERE code LIKE 'pipeline-%' AND active AND NOT public_read AND NOT sensitive), (SELECT COUNT(*) FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code LIKE 'pipeline-%' AND workflow.active AND state.active), (SELECT COUNT(*) FROM pipeline_workflow_binding WHERE active), (SELECT COUNT(*) FROM workflow_default_state default_state JOIN workflow_definition workflow ON workflow.id=default_state.workflow_id JOIN workflow_state state ON state.id=default_state.state_id AND state.workflow_id=workflow.id WHERE workflow.code LIKE 'pipeline-%' AND workflow.active AND default_state.context='initial' AND default_state.active AND state.active)"
        []
        :: SqlPersistT IO [(Single Int, Single Int, Single Int, Single Int)]
    )
  unless (pipelineRegistryCounts == [(Single 6, Single 35, Single 11, Single 6)]) $
    liftIO . ioError . userError $
      "Persisted pipeline registry requires six internal workflows, 35 states, 11 bindings, and one active initial state per workflow: " <> show pipelineRegistryCounts
  invalidPipelineCards <-
    ( rawSql
        "SELECT COUNT(*) FROM pipeline_card card LEFT JOIN pipeline_workflow_binding binding ON binding.service_offering_id=card.service_offering_id AND binding.active LEFT JOIN service_offering service ON service.id=card.service_offering_id AND service.active LEFT JOIN workflow_state state ON state.id=card.workflow_state_id AND state.active LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id AND workflow.active WHERE card.service_kind IS NOT NULL OR card.stage IS NOT NULL OR service.id IS NULL OR state.id IS NULL OR binding.workflow_id IS DISTINCT FROM workflow.id"
        []
        :: SqlPersistT IO [Single Int]
    )
  unless (invalidPipelineCards == [Single 0]) $
    liftIO . ioError . userError $
      "Pipeline cards require canonical service_offering_id and workflow_state_id without legacy strings before cutover: " <> show invalidPipelineCards
  ddexSupportRows <-
    ( rawSql
        "SELECT standard.standard_code || ':' || standard.version_code || ':' || support.detection_enabled::text || ':' || support.validation_enabled::text || ':' || support.import_enabled::text || ':' || support.export_enabled::text FROM ddex_standard_support support JOIN ddex_standard_version standard ON standard.id=support.standard_version_id WHERE support.deployment_code='default' AND support.active AND standard.active ORDER BY standard.standard_code, standard.version_code"
        []
        :: SqlPersistT IO [Single Text]
    )
  let expectedDdexSupportRows = sort
        [ "DSR:1.4:false:false:false:false"
        , "ERN:4.3.2:true:true:true:true"
        , "MEAD:1.1:true:false:false:false"
        , "RIN:2.1:true:false:false:false"
        ]
      actualDdexSupportRows = sort [value | Single value <- ddexSupportRows]
  unless (actualDdexSupportRows == expectedDdexSupportRows) $
    liftIO . ioError . userError $
      "Persisted DDEX deployment support does not match executable capabilities: " <> show actualDdexSupportRows
  runtimeDdexMessages <-
    ( rawSql
        "SELECT standard.standard_code || ':' || standard.version_code || ':' || message.code FROM ddex_message_type message JOIN ddex_standard_version standard ON standard.id=message.standard_version_id WHERE message.active AND message.runtime_supported ORDER BY standard.standard_code, standard.version_code, message.code"
        []
        :: SqlPersistT IO [Single Text]
    )
  unless (runtimeDdexMessages == [Single "ERN:4.3.2:NewReleaseMessage"]) $
    liftIO . ioError . userError $
      "Persisted DDEX runtime message types do not match the implemented parser: " <> show runtimeDdexMessages
  ddexWorkflowCounts <-
    ( rawSql
        "SELECT (SELECT COUNT(*) FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code='ddex-document-lifecycle' AND workflow.active AND workflow.sensitive AND NOT workflow.public_read AND state.active), (SELECT COUNT(*) FROM workflow_default_state default_state JOIN workflow_definition workflow ON workflow.id=default_state.workflow_id JOIN workflow_state state ON state.id=default_state.state_id AND state.workflow_id=workflow.id WHERE workflow.code='ddex-document-lifecycle' AND workflow.active AND default_state.context='initial' AND default_state.active AND state.active AND state.code='received')"
        []
        :: SqlPersistT IO [(Single Int, Single Int)]
    )
  unless (ddexWorkflowCounts == [(Single 12, Single 1)]) $
    liftIO . ioError . userError $
      "Persisted DDEX document lifecycle requires 12 active states and received as its unique initial state: " <> show ddexWorkflowCounts
  ddexOperationalRegistryCounts <-
    ( rawSql
        "SELECT (SELECT COUNT(*) FROM workflow_definition WHERE code IN ('ddex-validation-lifecycle','ddex-import-plan-lifecycle','ddex-import-run-lifecycle','ddex-export-lifecycle','ddex-job-lifecycle') AND active AND sensitive AND NOT public_read), (SELECT COUNT(*) FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code IN ('ddex-validation-lifecycle','ddex-import-plan-lifecycle','ddex-import-run-lifecycle','ddex-export-lifecycle','ddex-job-lifecycle') AND workflow.active AND state.active), (SELECT COUNT(*) FROM workflow_default_state default_state JOIN workflow_definition workflow ON workflow.id=default_state.workflow_id JOIN workflow_state state ON state.id=default_state.state_id AND state.workflow_id=workflow.id WHERE workflow.code IN ('ddex-validation-lifecycle','ddex-import-plan-lifecycle','ddex-import-run-lifecycle','ddex-export-lifecycle','ddex-job-lifecycle') AND workflow.active AND default_state.context='initial' AND default_state.active AND state.active), (SELECT COUNT(*) FROM ddex_job_operation WHERE active), (SELECT COUNT(*) FROM ddex_import_operation WHERE active), (SELECT COUNT(*) FROM ddex_validation_result WHERE active), (SELECT COUNT(*) FROM ddex_validation_severity WHERE active), (SELECT COUNT(*) FROM ddex_validation_layer WHERE active)"
        []
        :: SqlPersistT IO [(Single Int, Single Int, Single Int, Single Int, Single Int, Single Int, Single Int, Single Int)]
    )
  unless (ddexOperationalRegistryCounts == [(Single 5, Single 23, Single 5, Single 4, Single 3, Single 3, Single 3, Single 4)]) $
    liftIO . ioError . userError $
      "Persisted DDEX operational registry requires five sensitive workflows, 23 states, five defaults, and complete operation/validation registries: " <> show ddexOperationalRegistryCounts
  invalidDdexDocuments <-
    ( rawSql
        "SELECT COUNT(*) FROM ddex_document document LEFT JOIN ddex_standard_version standard ON standard.id=document.standard_version_id LEFT JOIN ddex_standard_support support ON support.standard_version_id=standard.id AND support.deployment_code='default' AND support.active LEFT JOIN ddex_message_type message ON message.id=document.message_type_id AND message.standard_version_id=standard.id LEFT JOIN workflow_state state ON state.id=document.workflow_state_id LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE document.standard_version_id IS NULL OR document.workflow_state_id IS NULL OR document.family IS NOT NULL OR document.version IS NOT NULL OR document.message_type IS NOT NULL OR document.status IS NOT NULL OR standard.active IS DISTINCT FROM TRUE OR support.detection_enabled IS DISTINCT FROM TRUE OR (document.message_type_id IS NOT NULL AND (message.active IS DISTINCT FROM TRUE OR message.runtime_supported IS DISTINCT FROM TRUE)) OR workflow.code IS DISTINCT FROM 'ddex-document-lifecycle' OR workflow.active IS DISTINCT FROM TRUE OR state.active IS DISTINCT FROM TRUE"
        []
        :: SqlPersistT IO [Single Int]
    )
  unless (invalidDdexDocuments == [Single 0]) $
    liftIO . ioError . userError $
      "DDEX documents require canonical governed references without legacy strings before cutover: " <> show invalidDdexDocuments
  invalidDdexPartners <-
    ( rawSql
        "SELECT COUNT(*) FROM ddex_partner partner WHERE jsonb_array_length(COALESCE(to_jsonb(partner)->'allowed_versions','[]'::jsonb))<>0 OR (partner.is_active AND NOT EXISTS (SELECT 1 FROM ddex_partner_standard_version membership JOIN ddex_standard_version standard ON standard.id=membership.standard_version_id JOIN ddex_standard_support support ON support.standard_version_id=standard.id AND support.deployment_code='default' WHERE membership.partner_id=partner.id AND membership.active AND standard.active AND support.active AND support.detection_enabled))"
        []
        :: SqlPersistT IO [Single Int]
    )
  unless (invalidDdexPartners == [Single 0]) $
    liftIO . ioError . userError $
      "DDEX partners reject legacy allowedVersions strings and every active partner requires an active canonical standard policy: " <> show invalidDdexPartners
  invalidDdexExports <-
    ( rawSql
        "SELECT COUNT(*) FROM ddex_export export LEFT JOIN ddex_standard_version standard ON standard.id=export.standard_version_id LEFT JOIN ddex_standard_support support ON support.standard_version_id=standard.id AND support.deployment_code='default' AND support.active LEFT JOIN ddex_partner_standard_version membership ON membership.partner_id=export.partner_id AND membership.standard_version_id=standard.id AND membership.active LEFT JOIN ddex_partner partner ON partner.id=membership.partner_id WHERE export.standard_version_id IS NULL OR export.ern_version IS NOT NULL OR standard.active IS DISTINCT FROM TRUE OR support.export_enabled IS DISTINCT FROM TRUE OR membership.id IS NULL OR partner.is_active IS DISTINCT FROM TRUE"
        []
        :: SqlPersistT IO [Single Int]
    )
  unless (invalidDdexExports == [Single 0]) $
    liftIO . ioError . userError $
      "DDEX exports require canonical export-enabled standard and partner policies: " <> show invalidDdexExports
  invalidDdexOperationalRows <-
    ( rawSql
        "SELECT (SELECT COUNT(*) FROM ddex_validation_run item LEFT JOIN workflow_state state ON state.id=item.workflow_state_id LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code IS DISTINCT FROM 'ddex-validation-lifecycle' OR state.active IS DISTINCT FROM TRUE OR workflow.active IS DISTINCT FROM TRUE) + (SELECT COUNT(*) FROM ddex_import_plan item LEFT JOIN workflow_state state ON state.id=item.workflow_state_id LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE item.status IS NOT NULL OR workflow.code IS DISTINCT FROM 'ddex-import-plan-lifecycle' OR state.active IS DISTINCT FROM TRUE OR workflow.active IS DISTINCT FROM TRUE) + (SELECT COUNT(*) FROM ddex_import_run item LEFT JOIN workflow_state state ON state.id=item.workflow_state_id LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE item.status IS NOT NULL OR workflow.code IS DISTINCT FROM 'ddex-import-run-lifecycle' OR state.active IS DISTINCT FROM TRUE OR workflow.active IS DISTINCT FROM TRUE) + (SELECT COUNT(*) FROM ddex_export item LEFT JOIN workflow_state state ON state.id=item.workflow_state_id LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code IS DISTINCT FROM 'ddex-export-lifecycle' OR state.active IS DISTINCT FROM TRUE OR workflow.active IS DISTINCT FROM TRUE) + (SELECT COUNT(*) FROM ddex_job item LEFT JOIN workflow_state state ON state.id=item.workflow_state_id LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id LEFT JOIN ddex_job_operation operation ON operation.id=item.operation_id WHERE item.job_type IS NOT NULL OR item.status IS NOT NULL OR workflow.code IS DISTINCT FROM 'ddex-job-lifecycle' OR state.active IS DISTINCT FROM TRUE OR workflow.active IS DISTINCT FROM TRUE OR operation.active IS DISTINCT FROM TRUE) + (SELECT COUNT(*) FROM ddex_import_change item LEFT JOIN ddex_import_operation operation ON operation.id=item.operation_id WHERE item.operation IS NOT NULL OR operation.active IS DISTINCT FROM TRUE)"
        []
        :: SqlPersistT IO [Single Int]
    )
  unless (invalidDdexOperationalRows == [Single 0]) $
    liftIO . ioError . userError $
      "DDEX operational rows require persisted workflow-state and operation IDs without legacy strings: " <> show invalidDdexOperationalRows
  invalidDdexValidationReferences <-
    ( rawSql
        "SELECT (SELECT COUNT(*) FROM ddex_validation_run item LEFT JOIN ddex_validation_result result ON result.id=item.result_id WHERE item.result IS NOT NULL OR (item.finished_at IS NOT NULL AND item.result_id IS NULL) OR (item.result_id IS NOT NULL AND result.active IS DISTINCT FROM TRUE)) + (SELECT COUNT(*) FROM ddex_validation_issue item LEFT JOIN ddex_validation_severity severity ON severity.id=item.severity_id LEFT JOIN ddex_validation_layer layer ON layer.id=item.layer_id WHERE item.severity IS NOT NULL OR item.layer IS NOT NULL OR severity.active IS DISTINCT FROM TRUE OR layer.active IS DISTINCT FROM TRUE) + (SELECT COUNT(*) FROM ddex_export item LEFT JOIN ddex_validation_result result ON result.id=item.validation_result_id WHERE item.validation_result IS NOT NULL OR (item.validation_result_id IS NOT NULL AND result.active IS DISTINCT FROM TRUE))"
        []
        :: SqlPersistT IO [Single Int]
    )
  unless (invalidDdexValidationReferences == [Single 0]) $
    liftIO . ioError . userError $
      "DDEX validation reports require persisted result, severity, and layer IDs without legacy constructors: " <> show invalidDdexValidationReferences

seedContentTypes :: SqlPersistT IO ()
seedContentTypes =
  forM_ contentTypes $ \(code, entityKind, nameEs, nameEn, schema, publicRoute, adminRoute, publicRead) ->
    rawExecute
      "INSERT INTO content_type (code, entity_kind, name_es, name_en, schema_json, schema_version, public_route_pattern, admin_route_pattern, public_read, active, workflow_state_id, version) SELECT ?, ?, ?, ?, ?, 2, ?, ?, ?, TRUE, ws.id, 1 FROM workflow_state ws JOIN workflow_definition w ON w.id=ws.workflow_id WHERE w.code='catalog-publication' AND ws.code='published' ON CONFLICT (code) DO UPDATE SET schema_json=EXCLUDED.schema_json, schema_version=EXCLUDED.schema_version, public_route_pattern=EXCLUDED.public_route_pattern, admin_route_pattern=EXCLUDED.admin_route_pattern, updated_at=CURRENT_TIMESTAMP, version=content_type.version+1 WHERE content_type.schema_version<EXCLUDED.schema_version AND content_type.version=1"
      [PersistText code, PersistText entityKind, PersistText nameEs, PersistText nameEn, PersistText schema, maybe PersistNull PersistText publicRoute, maybe PersistNull PersistText adminRoute, PersistBool publicRead]
  where
    contentTypes =
      [ ("record-release", "record_release", "Lanzamiento", "Release", "{\"type\":\"object\",\"entity\":\"record_release\",\"required\":[\"releaseTypeId\",\"titleEs\",\"titleEn\"]}", Just "/records/releases/{slug}", Just "/catalogos/records-releases/{id}", True)
      , ("recording", "recording", "Grabación", "Recording", "{\"type\":\"object\",\"entity\":\"recording\",\"required\":[\"recordingTypeId\",\"titleEs\",\"titleEn\"]}", Just "/records/recordings/{slug}", Just "/catalogos/records-recordings/{id}", True)
      , ("recording-session", "recording_session", "Sesión", "Recording session", "{\"type\":\"object\",\"entity\":\"recording_session\",\"required\":[\"sessionTypeId\",\"titleEs\",\"titleEn\"]}", Just "/records/sessions/{slug}", Just "/catalogos/records-sessions/{id}", True)
      , ("editorial-collection", "editorial_collection", "Colección editorial", "Editorial collection", "{\"type\":\"object\",\"entity\":\"editorial_collection\",\"required\":[\"collectionType\",\"nameEs\",\"nameEn\"]}", Just "/records/{slug}", Just "/catalogos/editorial-collections/{id}", True)
      , ("fan-hub-page", "authored_page", "Página Fan Hub", "Fan Hub page", "{\"type\":\"object\",\"required\":[\"heroTitle\",\"heroSubtitle\",\"sections\"],\"example\":{\"heroTitle\":\"Descubre artistas emergentes\",\"heroSubtitle\":\"Sigue y guarda lanzamientos para escuchar luego.\",\"sections\":[]}}", Just "/fans", Just "/cms/{id}", True)
      , ("course-production-page", "authored_page", "Página del curso de producción", "Production course page", "{\"type\":\"object\",\"required\":[\"heroTitle\",\"heroSubtitle\",\"sessions\"],\"example\":{\"heroTitle\":\"Producción musical en vivo\",\"heroSubtitle\":\"Reserva tu cupo con clases hands-on.\",\"sessions\":[]}}", Just "/curso/{slug}", Just "/cms/{id}", True)
      ]

seedAuthoredContents :: SqlPersistT IO ()
seedAuthoredContents = do
  forM_ authoredContents $ \(identifier, code, contentTypeCode, nameEs, nameEn, slug, publicRoute, sortOrder) ->
    rawExecute
      "INSERT INTO authored_content (id, content_type_id, code, name_es, name_en, current_slug, public_route, sort_order, active, workflow_state_id, published_revision, version) SELECT ?::uuid, ct.id, ?, ?, ?, ?, ?, ?, TRUE, ws.id, 1, 1 FROM content_type ct JOIN workflow_state ws ON ws.id=ct.workflow_state_id WHERE ct.code=? AND ct.active AND ws.code='published' ON CONFLICT (code) DO NOTHING"
      [ PersistText identifier, PersistText code, PersistText nameEs, PersistText nameEn
      , PersistText slug, PersistText publicRoute, PersistInt64 sortOrder, PersistText contentTypeCode
      ]
  rawExecute
    "INSERT INTO catalog_slug_alias (id, catalog_id, entity_kind, entity_id, scope, slug, current, redirect_status, created_at) SELECT gen_random_uuid(), c.id, 'authored_content', a.id, 'cms-public', a.current_slug, TRUE, 308, now() FROM authored_content a JOIN catalog_definition c ON c.code='authored-content' ON CONFLICT (scope, slug) DO UPDATE SET entity_id=EXCLUDED.entity_id, current=TRUE, retired_at=NULL"
    []
  -- Exact, deterministic legacy mapping. Structured Records rows intentionally
  -- remain unmapped and read-only because they now belong to typed Records entities.
  rawExecute
    "UPDATE cms_content c SET authored_content_id=a.id FROM authored_content a WHERE c.authored_content_id IS NULL AND c.slug=a.current_slug"
    []
  where
    authoredContents =
      [ ("20000000-0000-4000-8000-000000000001", "fan-hub", "fan-hub-page", "Fan Hub", "Fan Hub", "fan-hub", "/fans", 10)
      , ("20000000-0000-4000-8000-000000000002", "course-production", "course-production-page", "Curso de producción", "Production course", "course-production", "/curso/produccion-musical", 20)
      ]

catalogUuid :: Int -> Text
catalogUuid suffix = "10000000-0000-4000-8000-000000000" <> pad3 suffix

pad3 :: Int -> Text
pad3 value
  | value < 10 = "00" <> tshow value
  | value < 100 = "0" <> tshow value
  | otherwise = tshow value

tshow :: Show value => value -> Text
tshow = T.pack . show
