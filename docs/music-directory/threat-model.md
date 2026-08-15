# Modelo de amenazas y abuso

## Activos

PII de `Party`, ubicación residencial, evidencia de claims/verificación, postulaciones, mensajes,
datos de menores, tokens, reputación, orden de búsqueda y vínculos comerciales.

## Fronteras de confianza

- navegador/app ↔ API pública;
- sesión autenticada ↔ `ProtectedAPI`;
- API ↔ PostgreSQL;
- workers ↔ email/push/geocoder;
- moderación ↔ evidencia privada;
- links de conversión ↔ checkout/booking/orden existentes.

## Amenazas y controles

| Amenaza | Control requerido |
|---|---|
| Enumeración/IDOR | IDs UUID públicos, scopes por participantes/managers, 404 no revelador y pruebas cruzadas. |
| Inyección SQL | parámetros SQL; nunca interpolar texto/filtros/orden. |
| XSS/Unicode engañoso | límites, rechazo de controles/format marks peligrosos, render escapado, sanitización de rich text. |
| Geolocalización de domicilio | tabla privada separada, centroide público, distancia redondeada, mínimo de precisión. |
| Spam masivo | límites por IP/cuenta/perfil/acción, fingerprint, cooldown, reputación interna y challenge configurable. |
| Invitaciones no deseadas | preferencias opt-in/out, bloqueos bidireccionales, idempotencia y límite diario. |
| Duplicados/suplantación | similitud de nombre/ubicación/link, claim con evidencia, cola de merge, alias preservados. |
| Fraude laboral o bienes robados | categorías/reglas, reportes, moderación, trazabilidad y retención de evidencia. |
| Discriminación/contenido ilícito | política visible, taxonomía de reportes, revisión y apelación; validación jurídica antes de producción. |
| Archivos maliciosos | allowlist MIME/extensión, tamaño, checksum, nombre seguro, cuarentena y escáner antes de publicar. |
| URL peligrosa | solo HTTP(S), longitud, host normalizado, sin credenciales embebidas; rel seguro en UI. |
| Manipulación de ranking | pesos documentados, señales verificables, patrocinado separado, auditoría de cambios. |
| Reseñas falsas | interacción elegible, unicidad y separación del estado de pago/cumplimiento. |
| Replay | Idempotency-Key + fingerprint; mismo key/payload retorna original, payload distinto es 409. |
| Exfiltración analítica | no texto libre, PII ni coordenadas exactas en telemetría; retención y acceso acotados. |
| Elevación por profesión | autorización no importa tablas de profesiones; prueba de propiedad I01. |

## Menores

Estados de edad: `unknown`, `adult_verified`, `minor_restricted`, `guardian_pending`,
`guardian_approved`. `unknown` y estados de menor no pueden publicar, postular ni contactar de forma
independiente. El consentimiento del representante debe ser verificable, revocable, limitado por
finalidad y con auditoría. Contacto con menores usa controles más restrictivos y revisión según
política.

La política final, edades aplicables, conservación, verificación del representante y reglas laborales
requieren revisión jurídica por jurisdicción. El software implementa controles y gates; no afirma por
sí solo cumplimiento legal.

## Respuesta a abuso

Reporte → triage → revisión → acción/dismiss → apelación → cierre. Toda decisión conserva actor,
motivo, timestamps y referencia del objeto sin copiar contenido privado a logs generales. Suspensión
retira superficies públicas en transacción y no borra evidencia.
