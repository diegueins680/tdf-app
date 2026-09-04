# Reputación contextual v1

## Estado y alcance de esta entrega

Esta entrega introduce el núcleo seguro y auditable para evolucionar las
reseñas verificadas existentes. La experiencia interactiva se gobierna con el
flag de despliegue `CONTEXTUAL_REPUTATION_ENABLED`, expuesto a clientes
autenticados con el mismo nombre. Debe mantenerse apagado hasta completar la
aplicación de migración, el backfill y el piloto descritos abajo. Las lecturas
públicas ya publicadas siguen siendo compatibles durante la transición. No
altera `experience_review`, `directory_review` ni convierte estrellas
históricas en rankings que nunca sucedieron.

## Auditoría del estado anterior

- `tdf-hq` ya tenía `experience_review` para eventos, marketplace, reservas y
  storefronts. Su elegibilidad se verifica en PostgreSQL contra la interacción
  completada, aplica idempotencia y límite diario, y nunca expone `source_id`
  públicamente.
- El directorio tiene su propia abstracción de perfiles, reseñas, reportes,
  rate-limit y moderación. No se duplican esas tablas.
- Web (`tdf-hq-ui`) y móvil (`tdf-mobile`) consumen contratos OpenAPI generados
  y muestran estrellas; esas vistas se mantienen durante la transición.
- Las reservas, órdenes, entradas y checkout son los puntos de entrada de
  interacción existentes. Los proyectos, clases, colaboraciones y asistencia
  confirmada requieren adaptadores de elegibilidad por fuente antes de quedar
  activos; no se infieren desde datos incompletos.
- El riesgo principal encontrado es mezclar una opinión personal con una señal
  pública. Por eso `reputation_private_ranking` no tiene ninguna relación de
  lectura con `reputation_public_aggregate`.

## Arquitectura

```text
interacción completada/verificable
  -> reputation_interaction (evidencia privada)
  -> reputation_evaluation (una dirección por interacción)
  -> ranking por categoría / score absoluto contextual
  -> cola idempotente de agregación
  -> reputation_public_aggregate (proyección pública agregada)

ranking privado -> reputation_private_ranking -> sólo su propietario
```

Una evaluación se puede modificar antes de `edit_deadline`; las revisiones,
apelaciones y accesos administrativos se deben registrar en
`reputation_audit_log`. Las comparaciones sólo se habilitan cuando el adaptador
de contexto devuelve 3--10 perfiles comparables. Con uno o dos, se muestra una
evaluación individual estructurada; con más de diez, se segmenta por rol,
servicio, ciudad o página. La lista inicial se aleatoriza por borrador y se
persiste para permitir reanudarla sin cambiar el orden durante el flujo.

## Modelo y explicabilidad

Las preferencias de categorías usan **rank-order centroid (ROC)**. Para una
lista ordenada de `n` categorías, el peso de posición `r` es:

`w(r) = 100/n × Σ(1/j), para j=r..n`.

Es determinista, monotónico y la última cifra recibe el remanente para que la
suma sea exactamente 100. El modo avanzado acepta pesos no negativos que
respeten el orden y los normaliza igualmente. La implementación pura está en
`TDF.Reputation`, con pruebas de estos invariantes.

La reputación pública no usa esos pesos personales. Por categoría y contexto
usa las señales verificadas y moderadas, normalizadas a 0--100, con prior
bayesiano centrado en 50 y fuerza 8 en `public-bayes-roc-v1`. El worker debe
aplicar un decaimiento moderado (semivida 365 días), tope de contribución por
evaluador (25%), exclusión provisional de señales disputadas y un intervalo
conservador. Se muestra `Reputación en formación` hasta tres señales
verificadas; después se expone nivel de confianza bajo/moderado/alto en vez de
fingir precisión. Los rankings ordinales se agregan como comparaciones
emparejadas con empates, y se combinan con el score absoluto sólo dentro del
contexto comparable; el modelo seleccionado es un Bradley--Terry bayesiano con
prior, no una conversión de posición a estrellas.

## Privacidad, equidad y abuso

- Las posiciones y autorías individuales son privadas. La API pública sólo
  devuelve agregados, conteos, confianza, categorías y versión de fórmula.
- El acceso administrativo a la evidencia individual exige RBAC, motivo y
  auditoría. No se devuelven identidades de evaluadores por endpoints públicos.
- No se permiten categorías sobre atributos sensibles ni contenido ofensivo;
  propuestas son privadas hasta deduplicación semántica, adopción mínima y
  aprobación humana.
- Alertas de frecuencia, cuentas relacionadas, ciclos, brigading y volumen
  anómalo alimentan revisión humana; no aplican sanciones automáticas.
- La visibilidad pública, badges y rankings geográficos requieren consentimiento
  y tamaño de muestra mínimo. Ninguna puntuación causa restricciones, precios o
  pérdida de acceso.

## Contrato y UX previstos

Los endpoints v1 serán versionados bajo `/reputation`: elegibilidad, borrador,
orden de categorías, posiciones/empates/exclusiones, envío, vista personalizada,
perfil público, comparación, apelación y administración. Todos los writes
requieren `Idempotency-Key` y revisión de versión optimista. Los contratos
OpenAPI se publicarán antes de conectar clientes.

La tarjeta accesible tiene avatar o iniciales, nombre, nombre profesional,
rol, ciudad pertinente, contexto y verificación. Drag-and-drop es opcional:
botones subir/bajar/asignar posición, foco visible, anuncios `aria-live` y
atajos de teclado son obligatorios. El borrador ofrece guardado automático,
deshacer/restaurar, empates, exclusión por información insuficiente y estado de
error recuperable. La misma semántica se implementará para web y móvil, con
español e inglés y `prefers-reduced-motion`.

## Migración, despliegue y rollback

1. Ejecutar `2026-09-01_contextual_reputation.sql` con el manifiesto de
   producción checksum-pinned, primero en staging.
2. Ejecutar un backfill idempotente que cree sólo `reputation_interaction` para
   evidencia verificable; conservar `experience_review` como señal heredada
   diferenciada. Emitir reporte de omitidos/ambiguos.
3. Activar solamente lectura interna, validar conteos, duplicados y que ningún
   agregado contiene ranking privado o señal no verificada.
4. Piloto consentido y controlado; pausar ante >1% de errores de write, una
   fuga de identidad, variación inexplicable >10 puntos o alerta de fraude sin
   cola de revisión. Expandir gradualmente tras dos semanas estables.
5. Rollback: apagar el flag y lectores/escritores; aplicar el rollback sólo si
   es necesario tras preservar una exportación auditada. Las reseñas heredadas
   no se eliminan.

## Pendientes externos reales

- Decisión de Producto/Legal sobre consentimiento, retención, derechos de
  exportación/eliminación y texto de términos por jurisdicción.
- Provisión del worker/cola y métricas de observabilidad para agregación,
  reintentos y simulaciones. No es seguro ejecutar recalculados pesados en el
  request web.
- Aprobación de roles administrativos y del catálogo traducido por moderación.
- Pruebas de accesibilidad con usuarios de lector de pantalla y pruebas E2E en
  dispositivo físico antes de activar el flag.
