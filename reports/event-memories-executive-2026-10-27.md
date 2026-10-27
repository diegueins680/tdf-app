# TDF Records: recuerdos de eventos — actualización ejecutiva

Corte: 27/oct/2026 05:06 UTC. **Implementado y probado en aislamiento; pendiente de integrar y desplegar mediante las protecciones del repositorio. Sin invitaciones reales.** Esta actualización conserva las identidades del [informe anterior](event-memories-executive-2026-10-26.md).

## 1. Diagnóstico y reutilización

Se mantienen EventMoment, Party, eventos/RSVP/entradas, artistas, almacenamiento, autenticación, notificaciones internas e interacciones canónicas. El [diagnóstico inicial](event-memories-2026-10-25.md) recoge las diferencias respecto de las especificaciones. No se añade otro sistema de publicaciones, identidades o comunicaciones.

Evento141: PATCH CULTURE Vol.1;24/oct19:00UTC–25/oct02:00UTC, America/Guayaquil. Su celebración y finalización están confirmadas por el usuario. La consulta pública04:42UTC todavía devuelve `on_sale`: no se cambia antes de desplegar y verificar la lectura de eventos finalizados. Los registros históricos relacionados y los RSVP no prueban asistencia ni consentimiento actual.

## 2. Cambios implementados

Publicación y recuperación duradera de texto, fotografías, video y audio; edición/borrado propios; enlaces, comentarios, reacciones y moderación; entrada pública con contexto conservado en login, registro y recuperación; preferencias y campañas posteriores configurables por el organizador, con agregados. Se conservan los controles de organizador NULL, revocación, concurrencia, versiones, replay y sesión.

La nueva composición incorpora las protecciones de main: nombres públicos neutrales, conciliación de seguimientos y privacidad de analítica. Registro, participación profesional o seguimiento no habilitan comunicaciones automáticamente.

## 3. Arquitectura y decisiones

El servidor conserva la autoridad. Los borradores y respuestas pertenecen a la cuenta/evento de origen; solo una confirmación remota válida acredita publicación sincronizada. Se reutiliza la bandeja interna con consentimiento explícito, clasificación de evidencia, unicidad evento/cuenta/paso, transacciones y locks. La automatización requiere evento finalizado y campaña habilitada; publicar suprime recordatorios y se permite como máximo uno. Correo, push y WhatsApp no se habilitan implícitamente.

Los objetos de medios tienen URL pública. La interfaz informa ese alcance; ocultar o borrar una publicación no revoca copias externas. Galerías mixtas, etiquetas estructuradas, medios privados/revocables y previsualizaciones públicas individuales siguen pendientes.

## 4. Commits, PRs e integración

Aplicación probada: **67f72f8dd6a2b03cbf6e7bfbda1768ea31b376d4**, merge normal de2a4 y main protegido1129112f89d3c00d33b8c86043a6bd197031dcff. Conserva los150 objetos normativos de main+4 originales=**154**, y las227 migraciones y SQL originales exactos. Catálogo estricto:1350 candidatos. El commit posterior de evidencia no cambia aplicación, contratos, migraciones ni dependencias.

El [PR raíz608](https://github.com/diegueins680/tdf-app/pull/608) concentra la entrega. Las aprobaciones independientes5661958818/5661961095 corresponden a2a4; no se atribuyen a esta nueva composición. Las fuentes615/632/635/641 y sus evidencias se preservan.615 conserva su base534 archivada y sus dos hilos abiertos; no se resuelven como autor. La integración final debe satisfacer la revisión, CI, conversaciones y admisión exigidas por main. No se evade la cola ni se presenta una rama de preparación como main.

## 5. Pruebas ejecutadas

- Web local y [CI40450116519](https://github.com/diegueins680/tdf-app/actions/runs/40450116519): **3635 pruebas/316 suites**, lint, tipos y build;5preloads/336562bytes gzip.880 archivos de entrada exactos.
- Compilación local canónica Stack/GHC9.10.3: **exit0**,986 entradas exactas antes/después; cuatro pruebas Hspec de nombre público neutral satisfactorias. El CI general terminó12 jobs satisfactorios/2 omitidos (staging y Android instalado), sin fallos. Hspec:4045 ejemplos,0 fallos,21 pendientes; los pendientes no son pases.
- Recorridos web de CI: **608 satisfactorios/22 omitidos**, más10 controles de activación y25 de descubrimiento. Las omisiones no son pases.
- API/PostgreSQL/navegador nuevos: **127 peticiones/20 grupos/277 hashes actuales y de Git exactos/13 comprobaciones de navegador**. Ledger real227/227; controles de IDOR, autoría, moderación, revocación, replay y persistencia. Base desechable y procesos retirados después.
- Ocho controles de historial,11 comandos de especificación, catálogo JSON/CSV estricto y calidad general local satisfactorios. El intento inicial ENOSPC y los fallos anteriores conservan su alcance; no cuentan como pases.

El informe formal verifica1406 archivos actuales y de Git exactos y ejecuta satisfactoriamente solver/modelos/controles acotados. Mantiene **154 obligaciones globales UNPROVEN, releaseBLOCKED/executionAllowedfalse**, sin riesgos aceptados. No demuestra aptitud de despliegue.

## 6. Evidencia web y móvil

API real67f/build04:56:30UTC; ejecutable Darwin x86_64 SHA256`85dac8cbb8f1bc43df9f58446c7d399b24ee23285645919385da707aa315d051`, PostgreSQL16 y Chrome150 de escritorio/Pixel7. La cuenta creada por interfaz publica un PNG sintético con bytes duraderos y nombre público neutral. La recuperación conserva el destino y publica texto. Ambos recuerdos sobreviven al reinicio real del API, sin roles concedidos por el fixture.

El desafío se consume una vez: replay400, sesión/contraseña anteriores401, nueva contraseña200, evento privado404. SMTP está deshabilitado; el puente privado usa el token realmente emitido. **No acredita entrega de correo.** [Fotografía sintética](event-memory-source-evidence-20261025/main-1129/signup-first-memory-phone.png) y [publicación tras recuperación](event-memory-source-evidence-20261025/main-1129/recovery-memory-phone.png).

El ejecutable Linux probado por CI también se descargó y contrastó: artefacto12711376058, ELF SHA256`0a798c04f3689b1642ce67c08965c2958d682508e8b28a46e22252bc3a1aa49a`, ZIP SHA256`4df80478b7e7dfdd85bf132e61fd4c6fc21ab3949e5b7903865668987bfabc20`. Conserva su identidad separada del binario Darwin; no es una imagen desplegada.

El pin móvil7000 mantiene **653 pruebas/98 suites** en CI. Las ampliaciones172/173/[174](https://github.com/diegueins680/TDF-mobile/pull/174) conservan sus fuentes/evidencias separadas.174 tiene builds Android/iOS Simulator inspeccionados y759 pruebas; no está integrado aquí ni acredita recorrido instalado, dispositivo físico, firma productiva o tiendas. Los flujos Maestro existentes cubren interacciones generales, no este recorrido de recuerdos.

## 7. Despliegue

**Cero despliegues o escrituras productivas de esta entrega.** La web pública04:45UTC contiene main112, entry SHA256`dbaa3e2566c0c68deab4edcf807f7e7171ad2904b8065be00507e5e4cb24fb08`; el API público04:42UTC sigue4dac/build24oct01:24:48, saludable. Esto identifica versiones; no acepta en producción la nueva funcionalidad.

Siguen pendientes las protecciones del PR final, correspondencia de imagen, recuperación coordinada, canary y verificación canónica. El procedimiento requiere drenar todos los escritores durante el sufijo completo de migraciones y ante un fallo parcial. La API anterior comparte autoridad SQL: no se reabre tráfico a mitad de la instalación ni se considera suficiente un rollback de imagen.

## 8. Invitaciones reales141

| Indicador de esta entrega | Observado |
| --- | --- |
| Elegibles autorizados / excluidos | No establecidos; valores desconocidos |
| Intentadas / enviadas / entregadas / fallidas | **0 /0 /0 /0** |

Las entregas verificadas son exclusivamente sintéticas. Tras el despliegue y aceptación se corregirá el estado141 y se comprobarán destinatarios, evidencia y consentimiento actuales antes de un envío controlado e idempotente.

## 9. Métricas y límites

Recibos internos, aperturas atribuidas, publicaciones posteriores y agregados de campaña están instrumentados, junto con comentarios/reacciones existentes. Una notificación persistida no demuestra lectura humana, email o push entregados. Adquisición/conversión, retornoD7/D30 y efectos causales de retención del nuevo flujo todavía no se miden en producción. No se atribuyen mejoras a escenarios sintéticos.

## 10. Prioridades siguientes

P0: integrar con revisión/CI actuales; calificar recuperación, almacenamiento e imagen; desplegar y aceptar permisos/persistencia en producción. P1: corregir estado141 después de esa aceptación, verificar cohort autorizado y registrar invitaciones permitidas. P2: integrar y ejecutar móvil extendido; completar galerías, etiquetas, privacidad de archivos y previews. P3: medición restante y optimización con evidencia real.

[Recibo agregado de esta aplicación](event-memory-source-evidence-20261025/main-1129/qualification.json). Implementado, probado, integrado, desplegado y verificado en producción son estados distintos; el objetivo integral sigue pendiente.
