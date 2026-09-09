# Runbook: reputación de merch

Este runbook no autoriza producción. Los pasos de activación sólo se ejecutan después de aprobación explícita y con los criterios legales, privacidad, riesgo, soporte y producto cerrados.

## Preflight

1. Confirmar entorno y base de datos por nombre; detenerse si apunta a producción sin autorización.
2. Verificar que los nueve flags estén apagados.
3. Aplicar la migración dos veces en una copia sintética y ejecutar `npm run test:merch-reputation-migration`.
4. Regenerar OpenAPI/clientes y exigir diff vacío.
5. Ejecutar fórmula, backend, web, móvil, E2E y axe; registrar SHA y resultados reales.
6. Reconstruir proyecciones y comparar eventos/checkpoints/agregados.

## Rollout

Activar un flag por cohorte en el orden documentado. Separar cuentas de comprador, vendedor y moderador. Nunca habilitar `search_influence` durante la primera publicación de evaluaciones. Las notificaciones requieren opt-in y un transporte en modo sink en pruebas.

Salida por fase:

- cero acceso cruzado, autoevaluación o duplicación confirmados;
- cero divergencia entre evento y agregado;
- colas de moderación y apelación con SLA operativo;
- accesibilidad WCAG 2.2 AA verificada;
- concentración/visibilidad nueva dentro del rango aprobado;
- soporte y cumplimiento capaces de explicar fórmula y debido proceso.

## Incidentes

- Agregación fallida: apagar influencia en búsqueda e insignias; mantener reviews; revisar `merch_reputation_projection_alerts`; reintentar el worker idempotente.
- Manipulación: apagar creación si hace falta, preservar evidencia, abrir caso tipado; no castigar automáticamente por score.
- PII/contenido peligroso: ocultar provisionalmente con motivo, evidencia y permiso; notificar salvo excepción de seguridad; permitir apelación.
- Notificación sensible: apagar `notifications`, bloquear outbox afectada y revisar sólo `safe_payload`; no reenviar automáticamente.
- Ranking concentrado: apagar `search_influence`, conservar espacios de exploración y analizar `merch_reputation_exposure_daily`.
- Imagen insegura: apagar `review_images`, marcar asset oculto/rechazado; no borrar revisión textual ni evidencia.

## Rollback

1. Apagar los nueve flags y confirmar que no hay workers enviando notificaciones.
2. Preservar un snapshot y exportar conteos de evidencia/checkpoints.
3. Si existe cualquier dato durable, no ejecutar rollback destructivo: corregir hacia adelante.
4. El SQL de rollback sólo debe aceptar una instalación prístina; su rechazo es el comportamiento seguro esperado.
5. Revalidar perfiles, órdenes, búsqueda y reputación contextual después del cambio.

## Consultas operativas

```sql
SELECT * FROM merch_reputation_projection_alerts ORDER BY recorded_at;
SELECT environment,flag_key,enabled,version FROM merch_reputation_feature_flag ORDER BY environment,flag_key;
SELECT subject_kind,publication_state,formula_version_id,count(*) FROM merch_reputation_aggregate GROUP BY 1,2,3;
SELECT * FROM merch_reputation_metrics;
```

No incluir comentarios, direcciones, correos, teléfonos, pagos ni tracking en tickets, dashboards o previews.
