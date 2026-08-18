# Auditoría incremental: escritura de perfiles profesionales enriquecidos

Fecha de corte: 2026-08-17. Base: `origin/main` en
`a91e68753e8c1f4bec6b71953fc9293c412caf64`, después del merge del directorio principal.

## Hallazgo comprobado

La base y la lectura pública ya almacenaban o proyectaban experiencia, créditos, portafolio,
enlaces, equipo, tarifas, disponibilidad, detalle por profesión, nivel por instrumento, idiomas y
varias áreas de servicio. Sin embargo:

- `DirectoryProfileUpsert` solo aceptaba nombre, bio, cuatro arrays de taxonomías, una ciudad y
  modalidad;
- `GET /directory/profiles` devolvía una proyección mínima que no permitía reconstruir un editor;
- `POST`, `PUT` y transición de estado podían devolver formas parciales distintas de OpenAPI;
- web y móvil solo creaban perfiles básicos y no ofrecían edición;
- idiomas no se entregaban en el bundle público de taxonomías del directorio.

La evidencia se obtuvo de `2026-08-14_music_directory_core.sql`,
`TDF.API.Directory`, `TDF.Server.Directory`, `docs/openapi/directory.yaml`, los tipos generados y
los formularios `DirectoryManagePage.tsx` / `app/directory/manage.tsx`. No se añadió otra tabla ni
un perfil paralelo porque el esquema normalizado existente ya era la autoridad adecuada.

## Reconciliación aplicada

- El writer sigue exigiendo los campos básicos históricos, por lo que clientes anteriores continúan
  siendo válidos.
- Los campos enriquecidos son optativos. En un `PUT`, omitirlos conserva el valor anterior; string
  vacío, array vacío o `clearRates=true` expresa borrado deliberado.
- Las membresías estructuradas deben describir exactamente el mismo conjunto que sus arrays de IDs.
  Las claves primarias existentes impiden duplicados.
- Las áreas explícitas reemplazan el conjunto solo cuando se envía `serviceAreas`; requieren una
  única primaria y una jerarquía geográfica activa y coherente. La ruta histórica de una ciudad y
  los editores que aún no muestran áreas regionales/nacionales conservan esas áreas sin pérdida.
- El backend deriva coordenadas públicas únicamente del centroide del catálogo de ciudades. El
  writer no acepta dirección exacta ni coordenadas privadas.
- Las respuestas de creación, actualización, transición e idempotent replay usan una sola proyección
  privada, limitada por el grant activo de manager.
- La proyección adapta portafolios/enlaces históricos (`kind`/`source`) al DTO cerrado actual sin
  modificar el JSON de procedencia almacenado; admite rutas same-origin seguras para conservar media
  legada. Las ediciones generan auditoría de intención sin duplicar el contenido del perfil.

No fue necesaria una migración de esquema ni un backfill nuevo. La estrategia más segura fue usar
las columnas y relaciones ya desplegadas y verificar su compatibilidad mediante la prueba
PostgreSQL/API aislada.
