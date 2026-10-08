# Modelo de amenazas

## Activos

Másteres y arte bajo embargo, derechos y evidencia, PII mínima, credenciales, órdenes/asientos, entitlements, paquetes DDEX, métricas y URLs canónicas.

## Límites de confianza y controles

- Navegador → API: autenticación existente, permiso por operación, rate/quota, UUID no predecible, Idempotency-Key y validación de tamaño/tipo/hash.
- Navegador → storage: URL SigV4 HTTPS de 5 minutos limitada a bucket/objeto/part/upload ID; nunca credenciales ni listado. La firma pública consulta `music_public_asset_accessible` por UUID opaco y vuelve a comprobar estado, embargo, territorio y política; el rol `master_audio` falla cerrado.
- Edge → origen: `CF-IPCountry` solo se confía con origen inaccesible directamente; de otro modo territorios específicos fallan cerrados.
- API → pagos: estado paid exige intento y binding verificables; webhook/capture idempotente; secretos y payload cifrado son server-only.
- Worker → storage/DB: principal separado, mínimo acceso por prefijo, claim `SKIP LOCKED`, renovación y comprobación transaccional de propietario/intento/expiración, reintento y dead-letter. La pérdida de reserva interrumpe el grupo del comando activo.
- Export DDEX: admin estricto, DPID con evidencia, snapshot hash, XSD `--nonet`, rutas relativas y ZIP reproducible.
- Analítica: sesión ligada a identidad, evento/secuencia a solicitud exacta,
  locks transaccionales y evento/historial atómicos. El visitante puede inventar
  otra identidad y el cliente todavía declara tiempos/deltas: no es prueba de
  escucha humana ni contabilidad de regalías. Ver [ADR-019](playback-identity.md).

## Amenazas principales

| Amenaza | Mitigación | Riesgo residual/acción |
|---|---|---|
| Suplantar artista/equipo | `music_can` exige verificación + permiso backend | auditar altas/bajas de equipo y sesiones comprometidas |
| Filtrar embargo | única vista pública + checks en firma + buckets privados | prueba externa de cache/CDN antes de producción |
| Polyglot/corrupción/zip traversal | `ffprobe`, decodificación, whitelist, SHA antes/después, URI normalizada | antivirus/escáner de malware pendiente para recursos futuros |
| Reusar URL firmada | TTL 300 s y un objeto/operación | el portador puede compartirla durante TTL; reducir según UX |
| Comprar desde territorio falso | territorio del edge, snapshot inmutable, server verification | Cloudflare/origin ACL obligatorio |
| Webhook replay/doble cobro | claves únicas, binding proveedor, entitlement único | conciliación programada y alertas de mismatch |
| Manipular plays | UUID+secuencia, umbral continuo, dedupe y flags de ráfaga | antifraude avanzado y device attestation fuera de fase |
| Alterar publicado/DDEX | triggers append-only/inmutables y hash snapshot/package | proteger backups y roles DB contra superusuario operativo |
| Carrera o caída al encolar DDEX | transacción única export/job; lock actor-clave y versión; binding completo y 409 ante conflicto | no mezclar APIs antiguas; inspeccionar huérfanos legados, no reiniciar intentos terminales; vigilar contención y timeouts de DB |
| Supply-chain XSD/tools | URL HTTPS, hash fijado, imagen versionada | revisar hash/licencia en cada upgrade |
| Worker antiguo tras recuperación/cancelación | orden de bloqueo versión→job, número de intento y TTL en cada escritura; claves de salida con SHA-256 | un PUT ya en vuelo puede dejar objeto sin fila: reconciliar inventario, no borrar a ciegas ni mezclar versiones de worker |
| Multipart del worker alterado o abandonado | payload SHA-256 firmado por parte, hash integral antes de completar, XML estricto, aborto del ID conocido al fallar/TERM | SIGKILL/respuesta de creación perdida requiere lifecycle; no hay checkpoint durable del worker; probar retención y archivos grandes con el proveedor |
| Secretos del cargador en procesos/logs/XML remoto | configuración curl por stdin, sin `.curlrc` ni redirects, TLS verificado, errores genéricos y rechazo DTD/NUL | la identidad del worker sigue necesitando aislamiento del host y permisos mínimos; pruebas locales no certifican IAM |

No registrar IP cruda: usar hash rotatable cuando sea necesario, aplicar retención y borrado de datos personales sin borrar asientos/auditoría legalmente requeridos. Los periodos finales requieren revisión legal ecuatoriana y de los territorios ofrecidos.
