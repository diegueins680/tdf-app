# Tracker de aprobación y evidencia — Reputación contextual v1

> **Estado:** plantilla operativa. Actualizarla en el ticket o sistema de
> gestión aprobado; no usarla como sustituto de consentimientos o auditorías del
> producto.

## Objetivo

Centralizar las decisiones necesarias para pasar de código integrado y feature
flag apagado a staging y, después, a un piloto consentido. Una casilla marcada
sin enlace a evidencia no cuenta como aprobación.

## Estado de salida actual

| Área | Estado | Evidencia requerida | Responsable |
| --- | --- | --- | --- |
| Código y CI | Completado | Merge, checks y Build Image verdes | Ingeniería |
| Gates de activación | Activación autorizada; verificación pendiente | Flujo contextual `true` en staging/producción; worker de staging queda para el segundo rollout, tras validar su compuerta DB de `simulation` | Operaciones |
| Producto/Legal | Aprobación registrada; cierre pendiente | [Acta interna aprobada](rollout-evidence/2026-09-06-product-legal-approval.md) + checklist completo | Producto + Legal |
| Worker/observabilidad | Pendiente (infraestructura preparada) | Migración/worker en simulación; aún faltan staging, dashboards, alertas, DLQ y on-call aprobados | Infraestructura |
| Moderación/RBAC | Pendiente | Roles, taxonomía, apelaciones y auditoría | T&S + Admin |
| QA/accesibilidad/piloto | Pendiente | Evidencia WCAG, dispositivo físico y staging | QA + Accesibilidad |

## Checklist de aprobación

### Producto, Legal y Privacidad

- [ ] Jurisdicciones y base de tratamiento aprobadas para cada territorio. La
  [matriz de bases](rollout-evidence/2026-09-06-product-legal-approval.md)
  registra la decisión de producto, pero sigue pendiente la validación local de
  Ecuador, Colombia, Perú, México y la matriz estatal de Estados Unidos.
- [ ] Consentimiento granular, retiro y copy ES/EN aprobados y publicados. Copy aprobado: [05-consent-copy-es-en-draft.md](05-consent-copy-es-en-draft.md); pendiente de publicación y enlaces finales.
- [ ] Retención, exportación, eliminación/anominización y excepciones de
  seguridad aprobadas.
- [ ] Términos, ayuda y proceso de apelación aprobados.
- [ ] Política `01-product-legal-policy.md` revisada y versionada.

### Infraestructura y Operaciones

La implementación preparada incluye la migración
`2026-09-06_contextual_reputation_staging_worker.sql`, el worker Haskell apagado
por defecto y el ensayo reproducible
`npm run test:contextual-reputation-worker-migration`. Esto no marca ninguna
casilla: aún requiere provisionamiento real en staging, evidencia operativa y
aprobación de responsables.

- [ ] Outbox, cola durable y worker idempotente disponibles en staging.
- [ ] `event_id`, `run_id`, correlación, versión de fórmula y deduplicación
  verificadas.
- [ ] Dashboards, alertas, DLQ y on-call establecidos.
- [ ] Backfill/simulación y rollback de flag ensayados sin pérdida de evidencia.
- [ ] Runbook `02-operations-observability-runbook.md` aprobado.

### Moderación, Seguridad y Administración

- [ ] RBAC de evidencia individual y motivos obligatorios probados.
- [ ] Taxonomía oficial ES/EN y criterios de categorías prohibidas aprobados.
- [ ] Flujo de fraude, apelación, exclusión provisional y auditoría ensayado.
- [ ] Reglas de badges/rankings, consentimiento y muestras mínimas aprobadas.
- [ ] Playbook `03-moderation-admin-playbook.md` aprobado.

### QA, accesibilidad y piloto

- [ ] Suite funcional de staging y datos sintéticos reproducibles aprobados.
- [ ] Teclado, lector de pantalla, movimiento reducido, contraste e idiomas ES/EN
  validados contra WCAG 2.2 AA.
- [ ] Pruebas E2E en dispositivo móvil físico y conexión lenta completadas.
- [ ] Grupo piloto consentido, soporte y escalamiento definidos.
- [ ] Protocolo `04-staging-accessibility-pilot.md` aprobado.

## Puertas de decisión

| Puerta | Autoriza | Requiere |
| --- | --- | --- |
| G0 | Staging interno | cuatro áreas completas, snapshot y rollback |
| G1 | Piloto privado consentido | G0, observabilidad estable y soporte on-call |
| G2 | Visibilidad pública limitada | dos semanas estables, muestra/consentimiento y revisión de seguridad |
| G3 | Expansión gradual | métricas, apelaciones y fraude dentro de umbral |

Una puerta denegada debe registrar motivo, dueño, fecha de nueva revisión y
acciones correctivas. No sustituir una puerta faltante con una aprobación oral.

## Pausa y rollback

Pausar en cualquier fase ante fuga de identidad, rankings privados en agregados,
error de escritura sostenido superior a 1%, variación no explicada superior a
10 puntos, DLQ sin dueño, fraude sin revisión humana o bloqueador de
accesibilidad. Apagar el flag, preservar evidencia, ocultar proyecciones
afectadas y abrir incidente antes de reiniciar.

## Registro de decisión

Para cada puerta: fecha/hora UTC, ambiente, versión de fórmula, versión de
política, aprobadores, enlaces a dashboards/evidencia, grupo afectado, decisión
(`aprobar`, `pausar`, `rechazar`) y condición de rollback.

### 2026-09-06 — Producto/Legal

- **Decisión:** `aprobar` la política completa
  `01-product-legal-policy.md`.
- **Evidencia interna:** [acta de aprobación](rollout-evidence/2026-09-06-product-legal-approval.md).
- **Efecto:** documenta la aprobación declarada, pero no cierra Producto/Legal
  para G0. Siguen obligatorios los plazos por jurisdicción, copy publicado,
  pruebas auditadas de derechos y el resto del checklist; tampoco sustituye las
  puertas técnicas y de seguridad restantes.

### 2026-09-08 — Validación técnica de esquema en staging

- **Decisión:** `aprobar` únicamente la evidencia de compatibilidad de esquema
  y arranque controlado en staging.
- **Evidencia:** [validación técnica de staging](rollout-evidence/2026-09-08-staging-schema-validation.md).
- **Límite:** no cierra G0 ni ninguna casilla de Operaciones, Moderación,
  Producto/Legal o QA. Las banderas de reputación pública y worker continúan
  desactivadas.
