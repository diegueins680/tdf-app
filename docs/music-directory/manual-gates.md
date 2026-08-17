# Gates manuales y externos

Estas acciones no se simulan ni se declaran completas:

1. Revisión jurídica por país de menores, consentimiento, empleo, discriminación, privacidad,
   retención, moderación, publicidad y términos de clasificados.
2. Backup/restauración verificados y aprobación de migración de producción.
3. Disponibilidad y aprobación de extensiones PostgreSQL/PostGIS en cada entorno.
4. Selección de proveedor de geocodificación, revisión de términos, attribution y rate limits.
5. Configuración real y validación de entrega de email/push; sin credenciales, las entregas quedan
   `pending` o `disabled`, nunca `sent`.
6. Políticas operativas, personal y SLA para claims, verificaciones, moderación y apelaciones.
7. Proveedor/verificación legal antes de cualquier producto que custodie fondos. La implementación no
   llama escrow a los enlaces nuevos.
8. Revisión y aprobación de productos premium, anuncios destacados y disclosures patrocinados.
9. Firma/revisión de tiendas y credenciales EAS para distribuir la aplicación móvil.
10. Importación externa de eventos/venues solo con fuente autorizada, procedencia e idempotencia.
11. Validación E2E nativa en binarios Android/iOS, incluyendo TalkBack/VoiceOver, gestos, deep links y
    flujos autenticados con dos cuentas de prueba aisladas.
12. Revisión humana de las comprobaciones Axe marcadas `incomplete` y de contraste/foco en hardware
    representativo antes del release candidate.

Contenido existente de Ecuador/Quito puede migrarse solo desde registros reales del repositorio/base
autorizada. No se inventan personas, eventos, verificaciones, domicilios ni credenciales para poblar el
directorio.
