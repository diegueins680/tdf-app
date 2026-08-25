# ADR 0105: Directorio público sobre Party y dominios existentes

Estado: aceptado para implementación. Fecha: 2026-08-14.

## Contexto

TDF tiene identidad `Party`, perfiles de artista en dos dominios, bandas, venues/eventos sociales,
chat, catálogos gobernados y varias rutas comerciales. Ninguna pieza aislada permite administrar
varios perfiles profesionales, publicar clasificados ni buscar anónimamente con privacidad
geográfica.

## Decisión

1. `Party` permanece como raíz de persona/organización y dueño de PII.
2. `directory_profile` es la proyección pública unificada. Cada perfil pertenece a un `Party`, puede
   representar persona, artista, banda, proyecto, empresa, venue, estudio, sello, agencia, escuela u
   organización y puede enlazar IDs heredados.
3. `directory_profile_manager` concede capacidades explícitas a cuentas (`Party` autenticado). No se
   deriva ownership de membresía, profesión ni rol textual.
4. Profesión, categoría de clasificado y compensación se agregan como catálogos tipados al gobierno
   existente. Instrumento, género, servicio, país, subdivisión, ciudad e idioma se reutilizan.
5. Eventos y venues no se duplican. Una vista/proyección pública sanitizada participa en búsqueda; el
   registro social sigue siendo fuente de fechas, RSVP y ticketing.
6. Chat no se duplica. `directory_conversation_context` enlaza el DM existente con anuncio,
   postulación, invitación o perfil.
7. Clasificados modela matching. Servicios comprables siguen en service marketplace/storefront y se
   vinculan por ID opcional.
8. Búsqueda usa PostgreSQL `unaccent`, `pg_trgm`, `tsvector` y Haversine. PostGIS es una optimización
   intercambiable, no una precondición de desarrollo.
9. Direcciones/coordenadas exactas viven en una tabla privada. La proyección pública solo usa
   centroides de ciudad/sector o coordenadas comerciales expresamente publicables.
10. El mapa implementa `DirectoryMapAdapter`; el adaptador inicial usa OpenStreetMap sin credencial y
    siempre ofrece lista equivalente. Geocodificación es otro puerto configurable.

## Compatibilidad

- Los slugs de artista existentes se preservan como aliases.
- Los IDs heredados se guardan en `directory_legacy_link`; no se reemplazan ni eliminan.
- Backfill usa upserts idempotentes, procedencia y conteos. Las filas ambiguas quedan en revisión.
- Writers antiguos pueden seguir operando durante el backfill; triggers/refresh sincronizan la
  proyección en el período dual.
- El writer enriquecido mantiene obligatorios los campos históricos. En actualización, omitir
  experiencia, créditos, portafolio, enlaces, equipo, tarifas, idiomas o áreas conserva el dato;
  valores vacíos y `clearRates` son órdenes explícitas de borrado. La ruta histórica de una ciudad
  no elimina áreas secundarias.
- La edición no necesita un esquema paralelo: reutiliza las columnas y relaciones normalizadas de
  `directory_profile`. Creación, actualización, transición y replay idempotente devuelven la misma
  proyección privada limitada por `directory_profile_manager`.
- No se activa PostGIS, email, push, geocoder ni monetización sin gate explícito.

## Alternativas rechazadas

- Convertir `Party` en DTO público: expone riesgo de PII y acopla CRM con SEO.
- Reutilizar roles internos como profesiones: viola separación de autorización.
- Generalizar clasificados como `ServiceAd`: no representa demanda, empleo, colaboración ni
  convocatorias y acoplaría matching a pagos.
- Crear un chat nuevo: duplicaría participantes, bloqueos, inbox y notificaciones.
- Depender de Google Maps: requiere credencial y no satisface el arranque abierto.
- Llamar `escrow` al match convertido: la infraestructura actual no acredita custodia legal.

## Consecuencias

Hay una nueva proyección pública y nuevas relaciones, pero no otra identidad. El costo es mantener
links y backfill de dos perfiles de artista mientras convergen. La ventaja es una frontera clara para
privacidad, SEO, búsqueda, ownership y expansión internacional.
