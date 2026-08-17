# Especificación funcional

## Propósito

La acción dominante de TDF es encontrar personas, proyectos, organizaciones, venues, eventos,
servicios y oportunidades de la industria musical por texto, taxonomía y proximidad. Buscar y leer
contenido público no exige cuenta. Crear, responder, contactar, guardar, denunciar o convertir una
coincidencia sí exige autenticación.

Idiomas iniciales: español e inglés. El esquema y los fallbacks aceptan portugués sin cambiar las
relaciones canónicas. Mercado inicial: Ecuador, con Quito como ciudad sugerida, sin fijar el modelo a
un país.

## Entidades y límites

- **Cuenta:** credencial/token que autentica al `Party` de cuenta.
- **Party:** persona u organización con datos privados de CRM.
- **Perfil público:** presentación profesional administrable de un `Party`; una cuenta puede
  administrar varios perfiles mediante grants explícitos.
- **Proyecto/organización:** perfil público de tipo artista, banda, proyecto, empresa, venue, sello,
  agencia, escuela o estudio. La pertenencia no implica administración.
- **Rol interno:** autorización proveniente de `security_role`/`party_security_role`.
- **Profesión pública:** término de catálogo relacionado con un perfil; jamás se consulta para
  autorizar endpoints.
- **Clasificado:** demanda, colaboración u oportunidad con ciclo de vida propio.
- **Oferta profesional:** perfil/servicio ofrecido. Puede enlazarse a un `service_offering` o
  `service_ad`, sin copiar la orden comercial.

## Búsqueda pública

Ruta canónica web: `/buscar`. Destino móvil principal: `/(tabs)/directory`. API pública:
`GET /directory/search`.

Entradas:

- texto libre normalizado;
- entidad o pestaña;
- ciudad, área metropolitana, región, país o alcance mundial;
- coordenadas consentidas y radio;
- profesión, servicio, instrumento, género;
- fecha, disponibilidad, modalidad, viaje;
- precio/compensación, experiencia, verificación y reputación;
- cursor opaco y límite acotado.

Salidas:

- `organicResults` ordenados de forma estable por puntuación, fecha e ID;
- `sponsoredResults` separados y etiquetados, vacío mientras no exista producto aprobado;
- facetas eficientes;
- siguiente cursor;
- ciudad/alcance efectivo y precisión geográfica pública;
- sugerencias y categorías relacionadas cuando no haya coincidencias.

`GET /directory/taxonomies` es la autoridad pública localizada para profesiones, instrumentos,
géneros, servicios, categorías, compensaciones, monedas y ciudades. Web y móvil consumen esos IDs;
no mantienen listas editoriales divergentes. Los filtros de servicio usan el mismo
`service_offering` que el marketplace.

Ranking orgánico normalizado a `[0,1]`:

`0.40 texto + 0.15 taxonomía + 0.15 proximidad + 0.10 completitud + 0.08 actividad +
0.05 disponibilidad + 0.07 reputación verificable`.

Un factor ausente aporta cero y los pesos activos no se redistribuyen silenciosamente. La
explicación devuelve como máximo tres razones no sensibles. Popularidad y pago no alteran el ranking
orgánico. Objetivo de rendimiento: p95 menor a 500 ms para 100 000 documentos y 20 resultados, con
plan sin N+1 y paginación estable.

## Privacidad geográfica

- Selección manual funciona sin permiso del dispositivo.
- La geolocalización se solicita solo por una acción explícita y no se conserva sin consentimiento.
- Perfil/anuncio puede tener múltiples ciudades, radio, remoto y viaje.
- La API pública devuelve ciudad, sector autorizado, precisión y distancia aproximada redondeada;
  nunca dirección exacta ni coordenada residencial.
- Venues comerciales pueden autorizar coordenadas de entrada pública. Domicilios usan centroide de
  ciudad/sector.
- El mapa consume un adaptador abierto OSM. Geocodificación es un puerto configurable con caché,
  atribución, límites y revisión de términos.

## Perfiles

Un perfil tiene nombre, slug, tipo, bio, experiencia, créditos, portfolio, tarifas opcionales,
idiomas, equipo, disponibilidad, modalidad, viajes, enlaces, reputación y verificaciones. Las
relaciones a profesiones, instrumentos, géneros, servicios, idiomas y ciudades son conjuntos sin
duplicados.

Estados públicos: `draft`, `pending_review`, `published`, `paused`, `archived`, `suspended`,
`merged`. Solo `published`, visible y no suspendido se indexa. Archivar/suspender retira anuncios
dependientes y documentos de búsqueda en la misma transacción.

## Clasificados

Categorías provienen del catálogo `classified-categories`; los requisitos de cada categoría viven en
metadatos validados por backend. Expiración predeterminada: 30 días. Renovación manual crea auditoría
y no cambia el autor original.

Estados y transiciones están en `formal-model.yaml`. Un clasificado admite título, descripción,
perfil autor, profesiones/instrumentos/géneros buscados, ciudades/radio, modalidad, ventana, nivel de
experiencia, compensación, presupuesto, adjuntos permitidos, expiración y moderación.

Los formularios web y móvil leen `requirements.required` de la categoría elegida y bloquean el
borrador hasta completar los campos contextuales. Los importes se convierten a unidades menores con
la precisión de la moneda canónica; una oferta reservable sólo enlaza el `service_offering`
existente.

## Postulaciones, invitaciones y contacto

- Una cuenta postula únicamente un perfil que administra con permiso de contacto.
- Una postulación contiene mensaje, portfolio seleccionado, disponibilidad y propuesta económica.
- El autor y managers autorizados del anuncio pueden verla; el postulante y managers de su perfil
  también; moderadores solo por permiso explícito y finalidad auditada.
- Una invitación directa exige que la persona elija explícitamente cuál de sus perfiles actúa como
  remitente. Respeta preferencias, completitud mínima, bloqueos bidireccionales, edad, límites y una
  clave idempotente; si enlaza un clasificado, éste debe pertenecer al perfil remitente y seguir
  publicado, permitido y vigente.
- La aceptación es explícita: una postulación o invitación pendiente no puede saltar directamente a
  `conversation_open`. Una invitación vencida pasa a `expired`; bloquearla crea además el bloqueo de
  perfil en la misma transacción.
- Abrir contacto exige que el par remitente/destinatario coincida exactamente con los participantes
  de la postulación o invitación aceptada. Entonces crea/reutiliza el DM canónico y añade
  `directory_conversation_context`; no crea otro sistema de mensajes. Esa aceptación específica
  aporta consentimiento para el contexto aun si el perfil deshabilitó contacto general, pero nunca
  anula un bloqueo entre perfiles.
- Una coincidencia puede enlazarse a booking, service order, marketplace order o checkout existente.
  El enlace no modifica estados de pago ni cumplimiento.

## Búsquedas guardadas y alertas

La consulta se guarda en forma canónica sin coordenadas exactas del dispositivo. Un job evalúa solo
contenido nuevo/modificado. `(saved_search_id, result_kind, result_id, result_version)` es único: un
match no genera notificación duplicada. Email/push se registra como `pending` hasta confirmación real
del adaptador; la notificación interna puede marcarse creada transaccionalmente.

## Claims, verificación y reputación

- Claim: `draft -> submitted -> under_review -> approved|rejected|withdrawn`.
- `approved` crea el grant de administración; enviar evidencia nunca concede ownership.
- Verificaciones independientes: identidad, organización, venue, administración y crédito.
- Crédito: declarado, reclamado o verificado; puede enlazar contributor/release/recording existente.
- La lectura pública de reseñas es paginada por cursor y solo proyecta autor público, puntuación,
  texto, fecha y tipo de interacción; nunca expone `Party`, `external_id` comercial ni notas internas.
- La elegibilidad privada enumera únicamente interacciones `completed` con `verified_at`, entre el
  perfil autor administrado explícitamente y el otro perfil público. Crear una reseña exige seleccionar
  esa interacción y usa `Idempotency-Key`; hay una sola por interacción/autor/perfil y límite diario.
- Los clientes no crean `directory_interaction`: booking, órdenes o colaboraciones confirmadas deben
  registrarlas adaptadores internos confiables cuando exista evidencia real de cumplimiento.
- El promedio y conteo visibles se recalculan transaccionalmente desde reseñas publicadas todavía
  respaldadas por la interacción y por perfiles públicos actuales. Ocultar/eliminar una reseña o
  cancelar la elegibilidad actualiza el agregado; una fusión resuelve el perfil canónico sin borrar
  referencias históricas.
- Solo métricas resistentes a manipulación se publican; tiempos y tasas se agregan con umbrales.

## Moderación, menores y abuso

- Texto se normaliza, limita y muestra escapado; URLs permiten solo HTTP(S); archivos usan MIME,
  extensión, tamaño, checksum y análisis antes de publicar.
- Rate limits por cuenta/IP/acción; detección de duplicado por fingerprint; bloqueo y denuncia.
- Menores no pueden publicar, postular ni aceptar contacto de forma independiente. Requieren estado de
  edad verificado, representante con consentimiento vigente y restricciones de contacto.
- Las reglas laborales, privacidad infantil, consentimiento y retención requieren validación jurídica
  local antes de producción; no se afirma cumplimiento legal automático.

## SEO, accesibilidad y distribución

- URL estable para perfil, anuncio, evento y venue; aliases antiguos responden redirect permanente.
- canonical, Open Graph, Twitter/WhatsApp preview, JSON-LD apropiado y sitemap.
- `noindex` para borradores, pausados, vencidos, moderados, privados o con riesgo de privacidad.
- Enlaces profundos web/móvil; compartir usa Web Share con fallback de URL.
- WCAG 2.2 AA: navegación por teclado, nombres accesibles, foco visible, contraste, targets de 44 px,
  estados anunciados, mapa con equivalente en lista y respeto a movimiento reducido.

## Telemetría y funnel

Eventos permitidos, sin texto libre ni coordenadas exactas:

`directory_search_submitted -> directory_result_opened -> directory_contact_requested ->
directory_contact_accepted -> directory_match_converted`.

También se miden filtros, cero resultados, perfil completado, anuncio publicado, postulación,
invitación, primera respuesta, guardado, alertas deduplicadas, reportes y decisiones. IDs de perfil se
pseudonimizan en exportaciones analíticas.
