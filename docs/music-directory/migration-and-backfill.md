# Migración y backfill

## Principios

- expandir antes de migrar y contraer;
- migración SQL transaccional e idempotente;
- índices pesados `CONCURRENTLY` en paso operacional separado cuando el volumen lo exija;
- sin borrar ni sobrescribir registros históricos;
- procedencia, correlation ID, conteos y revisión de ambigüedades;
- writers antiguos permanecen válidos durante el dual-read.

## Extensión del writer enriquecido (2026-08-17)

No requiere DDL ni backfill: el esquema incremental del directorio ya contenía todas las columnas y
tablas normalizadas. El despliegue es compatible en orden backend -> clientes porque los campos
nuevos son optativos y los campos históricos permanecen obligatorios. El rollback consiste en
revertir backend/clientes; los datos enriquecidos permanecen preservados y los writers antiguos no
los borran al omitirlos.

## Proyección de imágenes de perfil (2026-08-18)

La migración incremental `2026-08-18_music_directory_profile_images.sql` mantiene como fuente de
verdad la primera entrada pública de tipo `image` del portafolio y la proyecta como `imageUrl` en la
búsqueda. Solo acepta URLs HTTP(S) o rutas relativas seguras ya admitidas por el writer, reindexa los
perfiles existentes de forma idempotente y actualiza la imagen en futuros refrescos. Su rollback
restaura la función anterior sin borrar URLs ya publicadas.

## Etapas

1. **Expand:** extensiones opcionales seguras (`unaccent`, `pg_trgm`), catálogos nuevos, perfiles,
   managers, taxonomías, ubicaciones públicas/privadas, clasificados, matching, claims, moderación,
   reputación, búsqueda, idempotencia, auditoría y telemetría.
   La migración histórica `2026-08-14_music_directory_core.sql` permanece byte a byte inmutable;
   índices, restricciones y agregados de reseñas verificadas se despliegan mediante la migración
   incremental `2026-08-16_music_directory_verified_reviews.sql`.
2. **Seed:** definiciones append-only y valores ES/EN con aliases ES/EN/PT; Ecuador, Pichincha y Quito
   solo desde referencias/procedencia existentes o fuentes oficiales versionadas.
3. **Backfill seco:** contar `artist_profile`, `social_artist_profile`, `party`, bandas y venues;
   resolver matches deterministas; registrar ambiguos, huérfanos y conflictos de slug.
4. **Backfill aplicado:** crear perfil por `Party`/entidad, managers únicamente para vínculos de cuenta
   demostrables, aliases y `directory_legacy_link`; no inventar verificaciones ni personas.
5. **Dual read:** búsqueda consume la proyección y muestra registros legacy publicados a través de
   links; cambios refrescan documento.
6. **Cutover:** web/móvil usan OpenAPI común; publicar los artefactos y habilitar tráfico por la
   configuración de despliegue/edge del entorno. No se presupone un runtime feature flag que el
   repositorio no tenga.
7. **Convergencia futura:** retirar columnas o rutas heredadas solo en otra migración, después de
   reconciliación completa y ventana de compatibilidad.

## Conteos obligatorios

- fuente total, elegible, mapeada, creada, ya existente, ambigua, rechazada;
- links por tipo (`artist_profile`, `social_artist_profile`, `band`, `venue`);
- slugs preservados y conflictos;
- perfiles sin manager explícito;
- ubicaciones públicas por precisión;
- referencias antes/después de cada merge.

## Recuperación

La migración incremental de reputación tiene un rollback que retira triggers, funciones e índices,
pero conserva las filas y los discriminantes aditivos `review` para no invalidar auditoría. Solo se
usa después de revertir la aplicación y congelar writes. Una vez que existen writes, el rollback de
aplicación retira el tráfico y conserva tablas; no se hace `DROP`. El backfill es reversible mediante
`backfill_run_id` solo mientras sus perfiles no tengan writes posteriores: elimina las proyecciones
nuevas, conserva intactas las fuentes legacy y mantiene el ledger con disposición `reversed`,
evidencia y conteos. Si detecta contenido o una versión nueva, aborta antes de borrar.

## PostGIS

La migración no falla si PostGIS no está disponible. El adaptador base usa Haversine sobre `double
precision` e índices btree/trigrama. Un paso posterior puede crear `geography(Point,4326)` e índice
GiST, comparar resultados y activar el adaptador por configuración.
