# Recuperación de cuenta sin perder el recuerdo del evento

Corte: 27/Oct/2026 00:25 UTC (26/oct en Ecuador). Fuente ejecutada `8b2bc818bc2a1eff130e105af0e94464133692a8`, sobre#635 `29c9f4556`. **Prueba real aislada satisfactoria; CI nuevo y revisión independientes pendientes; sin integración protegida, despliegue ni comunicaciones productivas.**

El recorrido real anterior cubría registro, pero no recuperación de contraseña. Se amplió el mismo runner y su base desechable: visitante → evento público → publicar → recuperar acceso → solicitud real de token → confirmación real → mismo compositor atribuido → publicación → lectura por otra cuenta → reinicio real de la API. Se reutilizan los handlers, rutas, sesión, preferencias y publicaciones existentes. El código de aplicación/API/móvil, dependencias y las226 migraciones conservan exactamente los bytes del padre.

## Resultado y arquitectura de la prueba

La [ejecución completa](event-memory-source-evidence-20261025/recovery-http-results.json) pasa **127 peticiones /20 grupos /276 hashes actuales y committed /13 comprobaciones de navegador**. [Calificación](event-memory-source-evidence-20261025/recovery-qualification.json), [resultado de navegador](event-memory-source-evidence-20261025/recovery-browser-results.json), [captura Pixel7 emulado](event-memory-source-evidence-20261025/recovery-memory-phone.png).

La solicitud y confirmación HTTP200 son reales. Un puente privado de la prueba lee mediante SELECT el token recién emitido para el actor sintético en la base propia; no inserta challenges ni fabrica respuestas. Se sigue `/reset`, la ruta del generador de correo existente, con el destino completo y `lang=es`. SMTP está deshabilitado: se verifica su señal real y la ausencia de intentos SMTP. El puente no demuestra envío ni recepción de correo. El token y las credenciales se excluyen de artefactos.

El navegador comprueba eliminación del token de la URL, identidad recuperada, retorno al compositor con query/fragmento, texto publicado y recarga por autor/organizador. PostgreSQL y API verifican después del reinicio: token consumido/replay400, sesión anterior401, contraseña anterior401, contraseña nueva200, evento privado404 y publicación persistida. La preferencia explícita de invitación del actor permanece true; la cuenta nueva de la prueba anterior conserva false. Recuperar acceso no añade roles ni autoridad de campaña; el número de entregas sintéticas existentes permanece uno.

La API conserva su identidad real Darwin262295679/build05:36:20Z/SHA256f01ef34b75f1f1c1da4a41cdbb541de9b620fd4c1f4b24aba64025e7c1b6e17d. No se compiló otro backend. Node22.14/Chrome150 y PostgreSQL16 propios; no es prueba Docker/Linux alojada, dispositivo físico, proveedor de correo ni aceptación productiva. El árbol tenía únicamente dos enlaces propios de dependencias sin seguimiento; no se acredita una release limpia.

Para conservar evidencia positiva de SMTP deshabilitado, la salida real de la API se captura con un PTY privado. Esto evita perder stdout bufferizado al terminar el proceso; se retienen las aserciones de seguridad y no se inventan líneas de log. El runner de CI existente ejecutará el mismo recorrido y solo añade la captura posterior a la publicación a su lista explícita de artefactos seguros.

## Intentos y validación

Los [intentos originales](event-memory-source-evidence-20261025/recovery-attempt-history.json) se mantienen separados:

| Intento | Resultado |
| --- | --- |
|1/a51 | Falló una expectativa del fixture:204 frente al200 que declara el contrato |
|2/8bd | Falló una ruta del fixture: `/reset-password` frente a la canónica `/reset` |
|3/a5b | Navegador y controles de credenciales satisfactorios; ejecución completa fallida por señal stdout no capturada |
|4/8b2 | ENOSPC al crear la base; cero peticiones de negocio, sin ejecución del navegador |
|5/8b2 | PASS completo127/20/276/13, con captura real de stdout y aserciones conservadas |

Un preflight rechazó correctamente traceability obsoleta y se regeneraron los mapas antes de ejecutar. Un scan intermedio coincidió con una corrección de fuente y se considera no calificado para la entrega final; se repitieron JSON/CSV estrictos con la fuente congelada. Los doce controles de credenciales/aislamiento, nueve de declaraciones, once de superficies, ocho de historial de catálogo y traceability pasan. El catálogo conserva1347 candidatos y todos los objetos de decisiones del padre. Los149 requisitos conservan sus campos normativos: solo dos requisitos propios reciben asociaciones de pruebas.

La falta de espacio se resolvió comprimiendo únicamente una caché propia antigua sin archivos abiertos:394681061→74301191 bytes. Se verificaron todos sus bytes antes de retirar la copia sin comprimir; el archivo sigue completamente recuperable, SHA2564748b4af4b16b46e1329ab4ceff60b92d7be8755d54d4e8bb6bacc83f4781c1d. Se preservaron ejecutables sellados, fuentes, evidencia y archivos de otros operadores.

El [padre29 completó CI](event-memory-source-evidence-20261025/signup-parent29-hosted-complete.json):31 resultados satisfactorios/8 jobs omitidos;566 escenarios con22 omisiones,10 de activación,25 de descubrimiento y su runner real122/19/269/11. Conserva su API Linux cacheada8b9 y su identidad independiente. No califica automáticamente este sucesor. La batería general local de este sucesor no se repitió tras el bloqueo previo del stand-in SSH; la ejecución Linux nueva sigue siendo necesaria.

## Integración, producción y límites

La API, Vite, navegador y base desechable propios se retiraron; el cluster propio se detuvo tras comprobar que solo quedaba postgres. Los enlaces de caché de dependencias se retiran al finalizar validaciones, conservando sus destinos. La evidencia pública excluye tokens, credenciales y logs privados.

Esta rama está apilada sobre#635. Las revisiones de#615/#632/#635 y la cadena móvil siguen pendientes, incluidos los hallazgos de autorización. Los149 requisitos globales continúan UNPROVEN y el informe formal conserva releaseBLOCKED; las pruebas acotadas no son prueba completa de implementación/refinamiento ni autorización de producción. Faltan composición revisada con main vigente, CI/protecciones/FIFO, correspondencia de artefactos y recovery/canary/despliegue y aceptación canónica.

Evento141: el usuario confirmó que se celebró y finalizó. No se cambió su estado ni se efectuaron escrituras/despliegues de esta entrega. Invitaciones reales intentadas/enviadas/entregadas/fallidas0/0/0/0; elegibilidad con consentimiento y exclusiones no establecidas. No se atribuyen mejoras de adquisición, conversión o retención a las cuentas sintéticas. Medios privados/revocables, galerías mixtas, tags, idempotencia general, previews por publicación y mediciónD7/D30 conservan los límites del [informe ejecutivo](event-memories-executive-2026-10-26.md).


## Corrección del primer control alojado de especificaciones

El primer head publicado d1dc0cf4f recibió un fallo real del job121933306203: el índice de descubrimiento no incluía el helper nuevo ni las huellas actualizadas. Ese fallo permanece en40426454893 y no se considera pase. Se regeneraron inventory y traceability y se revisó su diff: un archivo nuevo registrado, huellas de los helpers/workflow y enlaces a los dos requisitos propios; los campos normativos y `semanticConformance=not-established-by-discovery` permanecen sin promoverse.

El [preflight local completo](event-memory-source-evidence-20261025/recovery-specification-preflight.json) ejecutó los once comandos originales del workflow con parada ante cualquier código distinto de cero; todos finalizaron0. Incluye seis regresiones de inventario y diez de conformidad. Esta corrección solo afecta metadatos/informes; los276 archivos de la ejecución8b2 siguen exactos. Se requiere CI alojado nuevo del head corregido.

La previsualización auxiliar de Vercel informó `build-rate-limit`. La consulta de protección vigente muestra20 contextos obligatorios, sin Vercel. El fallo del proveedor se mantiene separado de la validación del código; no se modificaron plan ni controles. El informe formal global calificado del padre conserva149 requisitos UNPROVEN/releaseBLOCKED; el primer control fallido del sucesor no acredita ejecución formal completa.
