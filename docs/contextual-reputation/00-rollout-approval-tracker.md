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
| Flag por defecto | Completado | `CONTEXTUAL_REPUTATION_ENABLED=false` | Operaciones |
| Producto/Legal | Pendiente | Política aprobada, retención y copy ES/EN | Producto + Legal |
| Worker/observabilidad | Pendiente | Cola, worker, dashboards, DLQ y staging | Infraestructura |
| Moderación/RBAC | Pendiente | Roles, taxonomía, apelaciones y auditoría | T&S + Admin |
| QA/accesibilidad/piloto | Pendiente | Evidencia WCAG, dispositivo físico y staging | QA + Accesibilidad |

## Checklist de aprobación

### Producto, Legal y Privacidad

- [ ] Jurisdicciones y base de tratamiento definidas.
- [ ] Consentimiento granular, retiro y copy ES/EN aprobados.
- [ ] Retención, exportación, eliminación/anominización y excepciones de
  seguridad aprobadas.
- [ ] Términos, ayuda y proceso de apelación aprobados.
- [ ] Política `01-product-legal-policy.md` revisada y versionada.

### Infraestructura y Operaciones

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
