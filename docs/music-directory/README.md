# Directorio y Clasificados Musicales TDF

Este directorio contiene la especificación y la evidencia de la experiencia principal de búsqueda,
perfiles profesionales y clasificados musicales de TDF.

## Documentos canónicos

- [`initial-audit.md`](initial-audit.md): estado inicial comprobado contra código, SQL y pruebas.
- [`rich-profile-audit-2026-08-17.md`](rich-profile-audit-2026-08-17.md): auditoría incremental
  del contrato de escritura de perfiles y su cierre en backend, web y móvil.
- [`functional-spec.md`](functional-spec.md): alcance funcional, contratos de experiencia y criterios.
- [`formal-model.yaml`](formal-model.yaml): estados, transiciones, permisos e invariantes verificables.
- [`permissions.md`](permissions.md): matriz de autorización y alcance de registros.
- [`threat-model.md`](threat-model.md): amenazas, abuso, menores y mitigaciones.
- [`migration-and-backfill.md`](migration-and-backfill.md): despliegue incremental y reconciliación.
- [`data-and-flow-diagrams.md`](data-and-flow-diagrams.md): modelo lógico y flujos principales.
- [`operations-runbook.md`](operations-runbook.md): despliegue, observabilidad y rollback.
- [`verification-evidence.md`](verification-evidence.md): resultados automatizados y capturas
  reproducibles con fixtures sintéticos.
- [`manual-gates.md`](manual-gates.md): acciones que no pueden completarse sin infraestructura,
  credenciales o revisión humana independiente.

La decisión de arquitectura está en
[`docs/adr/0105-music-directory-classifieds.md`](../adr/0105-music-directory-classifieds.md).

## Convenciones

- `Party` es la persona u organización. Una credencial autentica una cuenta asociada a un `Party`.
- `directory_profile` es una proyección pública administrable; no es una cuenta ni concede acceso.
- IDs UUID son relaciones canónicas. Slugs son alias estables de URL, no claves foráneas.
- Las respuestas públicas nunca contienen PII, direcciones exactas ni coordenadas residenciales.
- El ranking patrocinado, si se habilita en el futuro, se entrega en una colección separada.
- El término heredado `service_escrow` no representa custodia regulada de fondos y no forma parte de
  los flujos nuevos.
