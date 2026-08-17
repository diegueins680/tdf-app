# Auditoría inicial basada en evidencia

Fecha de corte: 2026-08-14. Rama base: `origin/main` en
`91f8a5878d633fe0b1719898e99b16ed1dfc9946`.

La documentación histórica se usó únicamente como pista. Los hallazgos siguientes se comprobaron
contra código, migraciones, contrato OpenAPI y pruebas de la rama base.

## Línea base y seguridad del trabajo

- El checkout principal tenía cambios locales no relacionados y `main` local estaba cinco commits
  detrás. La implementación se aisló en un worktree limpio y en
  `feat/music-directory-classifieds-20260814`; no se copiaron cambios del checkout contaminado.
- `tdf-mobile` es un submódulo y `main` fija el commit
  `181031ebc03147f300d7cf64a18837cd6ea9cdfd`.
- El historial reciente incluye la reversión del merge automático accidental y la recuperación del
  flujo Domo en PR #150. La rama nueva parte del resultado recuperado.

## Identidad, perfiles y autorización

| Capacidad | Evidencia autoritativa | Estado comprobado |
|---|---|---|
| Cuenta | `tdf-hq/src/TDF/Models.hs`, `UserCredential` y `ApiToken` | Una credencial y sus tokens apuntan a un `Party`; no existe una entidad de cuenta separada con perfiles múltiples. |
| Persona/organización | `tdf-hq/src/TDF/Models.hs`, `Party`; `TDF.Server.partyServer` | `Party` contiene además PII de CRM y su API exige módulo CRM. No es seguro devolverlo como perfil público. |
| Roles internos | `TDF.Auth.AuthedUser`, `security_role`, `party_security_role` | Roles y módulos gobiernan autorización. Algunos nombres heredados (`Artist`, `Producer`, etc.) mezclan ocupación y acceso; la nueva profesión debe permanecer fuera de estas tablas. |
| Perfil de artista TDF | `TDF.Models.ArtistProfile` | Está unido 1:1 a `Party`; conserva slug, bio, ciudad, enlaces, géneros y Stripe Connect. |
| Perfil de artista social | `TDF.Models.SocialEventsModels.ArtistProfile` | Es otra tabla (`social_artist_profile`) cuyo `party_id` es texto opcional. Eventos la referencian. No se puede eliminar sin migración. |
| Perfil social de Party | `SocialPartyProfileDTO` y `/social/profiles` | Es una proyección autenticada de comunidad, no un perfil profesional público administrable. |
| Venue | `TDF.Models.SocialEventsModels.Venue` | Guarda dirección y coordenadas exactas junto con datos públicos; requiere una proyección sanitizada. |

Conclusión: `Party` debe seguir siendo la raíz de identidad y PII. Se necesita una proyección pública
general, enlazada de forma no destructiva a perfiles de artista, venues, bandas o registros heredados,
y una relación explícita de administradores de perfil.

## Catálogos

El catálogo central ya cubre workflows, estados, transiciones, traducciones ES/EN, alias de búsqueda,
slugs, revisiones, auditoría, importaciones, propuestas, merges, defaults y métricas. Sus entidades
`genre`, `instrument`, `service_category`, `service_offering`, países, subdivisiones, ciudades,
idiomas y locales son reutilizables. La lista de adaptadores tipados vive en
`TDF.Server.Catalog.catalogTableSpec` y las definiciones seed son append-only en
`TDF.Catalog.Seed.seedCatalogDefinitions`.

No existen catálogos tipados para profesiones públicas, categorías de clasificados ni tipos de
compensación. Esas tres familias se añaden al gobierno existente; no se crean listas constantes en
web o móvil.

## Búsqueda y geografía

- No existe búsqueda universal pública de entidades.
- `city_reference` ya normaliza país/subdivisión/ciudad y admite centroide, zona horaria y procedencia.
- No existe área metropolitana ni áreas de servicio de perfil.
- El backend ya usa PostgreSQL. No hay dependencia PostGIS garantizada.
- La exploración móvil de venues calcula distancias en el cliente y consume una ruta autenticada;
  además expone `address`, latitud y longitud del DTO del venue.
- Web usa Google Maps únicamente en logística de eventos y requiere clave. La nueva búsqueda no debe
  heredar esa dependencia.

## Eventos y venues

`SocialEventsAPI` implementa eventos, venues, artistas, RSVP, invitaciones, momentos, transmisiones,
ticketing, presupuesto, finanzas y logística. Todo el árbol está dentro de `ProtectedAPI`; por ello un
navegador anónimo no puede leer eventos o venues. El modelo tiene `event_type_id`,
`workflow_state_id` y capacidad pública derivada del catálogo, pero no tiene ownership/claims ni una
proyección pública que excluya dirección y coordenadas precisas.

## Comunidad, chat y notificaciones

- `party_follow` implementa conexiones.
- `chat_thread` solo modela un DM único por par de parties; `chat_message` valida participante en los
  handlers. No existe contexto de anuncio/postulación, bloqueo ni preferencias de contacto.
- `notification` existe y tiene rutas de bandeja interna, pero no hay alertas de búsqueda idempotentes.
- Correo y push existen como infraestructuras parciales; no deben declararse enviados sin evidencia
  del proveedor.

## Comercio

- Marketplace general tiene carrito, checkout y órdenes.
- Service marketplace tiene `ServiceAd`, slots, bookings y órdenes, pero usa el término `escrow` para
  asientos nominales que no prueban custodia ni payout. La auditoría de ingresos de 2026-08-13 ya lo
  identifica como representación incorrecta.
- Service storefront ofrece paquetes y órdenes públicas con idempotencia.

La nueva oferta profesional puede enlazarse a `service_ad`/`service_offering` o generar una reserva u
orden, pero nunca inferirá pago, custodia o cumplimiento. Los clasificados de demanda y colaboración
permanecen separados de servicios comprables.

## Web, móvil y descubribilidad

- Web: `/inicio` renderiza `FanHubPage`; no tiene búsqueda principal. Las rutas públicas incluyen
  artista, marketplace, cursos y reservas, pero los eventos permanecen protegidos.
- Móvil: la primera pestaña es eventos y el layout completo redirige al login; no existe destino
  anónimo de búsqueda.
- `tdf-hq/assets/feature-registry.json` es la autoridad compartida para rutas, navegación, permisos,
  favoritos, accesos rápidos, móvil y telemetría; el registro móvil se genera desde ella.
- El OpenAPI canónico para clientes es `tdf-hq/docs/openapi/api.yaml` (no el archivo histórico
  `tdf-hq/api/openapi.yaml`). CI regenera los tipos web y móvil y exige diff limpio.

## Pruebas y huecos

- Backend usa Hspec y QuickCheck; web usa Jest/Testing Library; móvil usa Jest y Testing Library.
- Hay pruebas SQL de migración y cutover, auditorías de features y contratos de clientes.
- Los `501` encontrados están en DDEX incompleto y en catálogos sin adaptador tipado. No hay un
  handler 501 de eventos/venues, pero sus lecturas públicas no existen.
- SEO dispone de `useMetaTags`, pero no hay integración general de sitemap/JSON-LD para perfiles,
  anuncios, eventos y venues.

## Decisiones derivadas

1. Reutilizar `Party`, catálogos, chat, eventos, venues, bookings, service offerings y órdenes.
2. Añadir una proyección `directory_profile` con managers explícitos y links heredados auditables.
3. Mantener ubicación exacta en tablas privadas; indexar únicamente centroides públicos y precisión
   declarada.
4. Añadir clasificados y postulaciones como dominio de matching, no como marketplace paralelo.
5. Implementar búsqueda con `unaccent` + trigramas y distancia Haversine; PostGIS queda detrás del
   mismo adaptador para entornos que lo habiliten.
6. Separar siempre resultados patrocinados del ranking orgánico.
