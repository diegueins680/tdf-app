# Runbook de operaciones y observabilidad — Reputación contextual v1

> **Estado:** borrador para Infraestructura y Operaciones. No habilita el
> feature flag ni autoriza ejecutar migraciones en producción.

## 1. Objetivo y límites

Este runbook define cómo operar el cálculo asíncrono de reputación pública sin
mezclarlo con rankings privados o preferencias personales. La API puede ofrecer
una vista previa inmediata, pero la proyección pública consolidada debe ser
procesada por un worker idempotente. No se ejecutan recálculos pesados dentro de
un request web.

Quedan fuera la aprobación legal, la taxonomía de Moderación y la activación de
participantes reales. Este documento no autoriza el piloto.

## 2. Arquitectura operativa mínima

```text
write de evaluación verificada
  -> transacción: evaluación + outbox de reputación
  -> cola durable
  -> worker idempotente por sujeto/categoría/contexto
  -> reputación pública agregada versionada
  -> métricas, trazas y auditoría

rankings privados / preferencias personales
  -> almacenamiento privado separado
  -> nunca publican eventos de agregación
```

El productor escribe el evento en la misma transacción que la evaluación o
señal moderada. El relay publica desde outbox y puede reintentarse. El
consumidor tolera eventos duplicados, fuera de orden y reentregas.

## 3. Contrato de evento e idempotencia

| Campo | Regla |
| --- | --- |
| `event_id` | UUID estable, único y trazable |
| `event_type` | `evaluation.submitted`, `evaluation.edited` (solo si sigue `submitted`), `evaluation.invalidated` (`submitted -> under_review|void`), `signal.moderated`, `appeal.provisional_opened`, `appeal.resolved`, `interaction.invalidated`, `category.applicability_changed`, `public_consent.changed` o `recalculation.requested` |
| `occurred_at` | Hora UTC de la mutación fuente |
| `subject_id` | Usuario cuya proyección puede cambiar |
| `context_key` | Clave canónica única de rol, interacción/servicio y segmento comparable |
| `category_id` | Categoría versionada o `null` para recálculo global acotado |
| `source_version` | Revisión de la evaluación o señal fuente |
| `algorithm_version` | Fórmula que debe calcularse |
| `correlation_id` | Une request, outbox, worker y auditoría |

La clave de idempotencia recomendada es `event_id + algorithm_version`. El
worker debe confirmar resultado, estado de publicación y proyección en una sola
transacción, o usar un outbox de publicación durable que permanezca pendiente
hasta su confirmación. Un duplicado nunca puede devolver “éxito” si la
proyección no llegó a estar comprometida. Si ya existe una versión más nueva de
la fuente, descarta el evento obsoleto o recalcula desde la fuente canónica.

Una evaluación ordinal puede afectar a varios perfiles. Al crearla o editarla,
el productor debe comparar la versión anterior con la nueva y escribir, en la
misma transacción, un evento por cada `subject_id` de la unión de sujetos
anteriores y actuales. Esto incluye perfiles retirados de
`reputation_evaluation_rank.compared_party_id`, que necesitan invalidación para
no conservar un agregado obsoleto. Como alternativa, un evento versionado puede
contener esa unión completa si el consumidor garantiza el mismo fan-out
idempotente antes de confirmar el mensaje.

`context_key` debe mapearse de forma uno-a-uno al selector de contexto de la
API pública. Cada respuesta de reputación selecciona una única clave comparable
y nunca suma ni lista categorías de contextos distintos; sin selector válido se
devuelve una respuesta contextual no publicada, no un agregado combinado.

Las mutaciones que quitan elegibilidad (`eligible -> disputed|void|expired`),
cambian una evaluación de `submitted` a `under_review|void`, abren una apelación
con exclusión provisional, archivan/fusionan categorías o cambian sus
roles/contextos aplicables deben escribir el evento de invalidación
correspondiente en el mismo outbox transaccional. Así se recalcula o retira la
proyección existente aunque no haya una evaluación posterior.

Cada retiro o nueva concesión de consentimiento de visibilidad pública o
rankings debe persistir `public_consent.changed` en la misma transacción. El
retiro invalida las proyecciones afectadas y cierra de inmediato el gate de
lectura para esa persona/superficie. Una nueva concesión mantiene ese gate
cerrado, programa el recálculo determinista y solo lo abre cuando la proyección
vigente está confirmada y supera sus demás umbrales. La API verifica el
consentimiento y el gate en cada lectura: el evento asíncrono repara la
proyección, pero no es la barrera que contiene una exposición.

## 4. Reglas de procesamiento

1. Cargar solo señales verificadas, vigentes, `submitted` y no excluidas
   provisionalmente. Borradores, autosaves y evaluaciones privadas nunca
   publican un evento de agregación ni satisfacen este predicado.
2. Verificar aplicabilidad de categoría y comparabilidad de roles/contexto.
3. Calcular con fórmula y parámetros versionados, prior bayesiano, límite por
   evaluador y decaimiento temporal aprobados.
4. Guardar score, intervalo/confianza, muestra, conteo verificable, versión de
   fórmula, parámetros y hora de cálculo.
5. Publicar solo con consentimiento vigente para la superficie/contexto, gate
   independiente de lectura pública abierto y el umbral de evidencia
   configurado, medido por interacciones verificadas distintas y por
   evaluadores elegibles distintos; no se suman conteos de filas por categoría
   de una misma interacción. Un estado `forming` no expone score global, score
   por categoría, intervalo, conteo ni tendencia en la proyección o API pública;
   esos datos permanecen internos hasta superar el umbral.
6. Auditar entradas, exclusiones, resultado y error sin almacenar PII extra.

Rankings privados y preferencias son explícitamente inelegibles. El worker debe
exigir interacción verificable solo a eventos que aportan una evaluación o
señal al cálculo, y rechazar esas contribuciones si son privadas/no verificadas.
Eventos administrativos de invalidación, consentimiento, categoría o recálculo
no requieren una interacción propia: disparan un cálculo desde la fuente
canónica, que vuelve a filtrar exclusivamente evidencia elegible.

## 5. Concurrencia, reintentos y recuperación

- Bloquear de forma acotada por `subject_id + context_key + category_id`, o usar
  versión optimista de proyección; nunca bloquear una cola completa. Un
  recálculo global (`category_id = null`) adquiere todas las llaves de categoría
  afectadas en orden estable o una barrera compartida de sujeto/contexto; nunca
  puede sobrescribir con un snapshot anterior un update de categoría.
- Reintentar transitorios con backoff exponencial y jitter, conservando
  `event_id`.
- Enviar a DLQ al superar el máximo aprobado. Cada ítem requiere alerta y
  resolución humana antes de descartarse.
- Reprocesar desde la fuente canónica ante cambio de algoritmo, apelación o
  reparación de datos.
- Ejecutar un recálculo periódico, o un mecanismo equivalente de vencimiento de
  proyección, para cada `context_key` publicado. La cadencia aprobada debe ser
  como máximo la necesaria para reflejar la semivida de 365 días y registrar su
  última actualización; la ausencia de nuevas mutaciones no congela score ni
  confianza indefinidamente.
- Backfill y simulación usan `run_id` persistente y una clave única de auditoría
  por fuente/run/versión; una segunda ejecución no duplica proyecciones ni
  auditorías semánticas. No usar el insert histórico no versionado como entrada
  replay-safe hasta que tenga esa garantía y prueba explícita.
- La versión activa de fórmula debe residir en configuración persistida y ser
  consultada por el lector; no se codifica de forma fija en la API. Un cambio de
  algoritmo calcula primero un conjunto completo de proyecciones inactivas para
  la nueva `formula_version_id`, valida cobertura, fixtures de referencia y
  métricas, y solo entonces cambia atómicamente el selector activo. Ninguna
  lectura mezcla versiones. El rollback vuelve a seleccionar atómicamente la
  versión anterior, sin borrar sus filas, mientras la nueva se investiga.

## 6. Métricas, trazas y alertas

| Métrica | Segmentación mínima | Alerta propuesta |
| --- | --- | --- |
| Eventos recibidos/procesados/fallidos | tipo, versión, contexto | Error sostenido >1% en 15 min |
| Edad y profundidad de cola | cola, prioridad | Edad por encima del SLO aprobado |
| Reintentos y DLQ | tipo de error, versión | Cualquier crecimiento no atendido de DLQ |
| Duración de cálculo | versión, tamaño de muestra | p95 excede presupuesto acordado |
| Duplicados idempotentes | productor, tipo | Aumento repentino o ratio anómalo |
| Proyecciones `forming`/publicadas | rol, contexto no identificable | Desviación relevante tras rollout |
| Frescura de proyección | versión, contexto no identificable | Más antigua que la cadencia de decaimiento aprobada |
| Apelaciones/exclusiones provisionales | categoría/contexto agregado | Pico que requiera revisión humana |

Registrar solo agregados mínimos necesarios: cobertura verificable, abandono por
paso, categorías no aplicables, estabilidad del score, confianza y errores de
accesibilidad. No usar atributos sensibles ni contenido individual como
dimensiones de analítica. Todas las trazas incluyen `correlation_id`; logs
redactan identificadores y nunca incluyen autores, texto libre, tokens o
evidencia adjunta.

## 7. Dashboards y responsables

Antes de staging, Operaciones debe publicar dashboards de salud de cola,
calidad de cálculo, seguridad/fraude y producto sin PII. Definir un on-call para
cola/worker, un responsable de datos para recalcular y un responsable de
Moderación para señales disputadas. Los dashboards no sustituyen auditoría de
accesos administrativos.

## 8. Validación en staging

1. Aplicar el manifiesto checksum-pinned en una base aislada.
2. Cargar datos sintéticos con roles, ciudades, empates, exclusiones, muestras
   pequeñas/grandes y señales antiguas.
3. Ejecutar backfill de señales heredadas y verificar omitidos/ambiguos; estas
   señales no pueden alimentar el agregado público.
4. Repetir e invertir eventos y provocar fallo transitorio. Confirmar una única
   proyección final y auditoría coherente.
5. Ejecutar simulación de fórmula sin publicar y comparar versión propuesta con
   vigente.
6. Resolver una apelación sintética; confirmar exclusión, recálculo acotado y
   ausencia de identidad en API pública.
7. Medir latencia, cola y error antes de abrir el piloto.

## 9. Criterios de pausa y rollback

Pausar el flag y la entrada de nuevas evaluaciones contextuales ante error de
write >1%, fuga de identidad, variación no explicada >10 puntos, DLQ no
atendida, fraude sin revisión humana o incumplimiento de privacidad.

Para rollback: **primero desactivar y verificar el gate independiente de lectura
de proyección pública** (no solo `CONTEXTUAL_REPUTATION_ENABLED`) para retirar
agregados existentes de perfiles, búsqueda y rankings; luego apagar el flag,
detener consumidores y escritores nuevos, preservar evidencia/auditoría y
congelar la versión de fórmula afectada. El ensayo debe probar que la API no
sirve proyecciones antiguas una vez cerrado el gate. No borrar reseñas heredadas
ni aplicar rollback SQL destructivo como primera respuesta.

## 10. Criterios de salida para el pendiente 2

1. Cola durable, outbox y worker idempotente provisionados en staging.
2. Dashboards, alertas, DLQ y on-call aprobados.
3. La batería de staging pasa con evidencia reproducible.
4. Recalculo histórico/simulación usa `run_id`, versión de fórmula y reporte
   auditable.
5. Se ensaya rollback de flag y recuperación sin pérdida de evidencia.
6. Producto, Seguridad y Moderación aprueban umbrales de alerta y pausa.

## 11. Control de versión

- **Versión:** 0.1 (borrador)
- **Propietario:** Infraestructura + Operaciones
- **Próxima revisión:** antes de provisionar staging con datos de piloto
- **Cambios materiales:** algoritmo, reintentos, umbrales, retención de
  auditoría o alcance de recálculo requieren revisión cruzada.
