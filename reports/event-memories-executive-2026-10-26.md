# TDF Records — Informe ejecutivo de recuerdos de eventos

Corte de esta continuación: 26 de octubre de 2026, 18:42 UTC. **Implementado y probado parcialmente, publicado para revisión; sin integración protegida, despliegue ni invitaciones reales de esta entrega. El objetivo integral continúa pendiente.**

## 1. Diagnóstico y capacidades reutilizadas

Se reutilizan eventos, RSVP, identidades Party, perfiles/artistas, EventMoment, almacenamiento del evento, sesión, API/OpenAPI y las interacciones canónicas: comentarios, reacciones, permisos, denuncias, moderación y auditoría. No se crea otro feed ni otra autoridad social. [Diagnóstico inicial](event-memories-2026-10-25.md) y [reparación de autorización](event-memory-authorization-repair-2026-10-25.md) detallan las diferencias con la especificación.

La [lectura pública de 16:48 UTC](event-memory-source-evidence-20261025/public-event141-after-native-author-artifacts.json) confirma el evento 141, **PATCH CULTURE Vol. 1**, público, inicio 24/oct 19:00 UTC, fin 25/oct 02:00 UTC, zona America/Guayaquil y estado registrado `on_sale`. El usuario confirmó que se celebró y finalizó. Se mantiene ese estado hasta desplegar y verificar la lectura de eventos finalizados. Un RSVP expresa intención, nunca acredita presencia.

El último inventario privado acreditado, 03:22 UTC, observó siete cuentas relacionadas/cinco activas: cinco vínculos profesionales y tres RSVP con un solapamiento. No son siete asistentes ni destinatarios autorizados; ese inventario no se actualizó mediante esta lectura pública. Consentimiento y exclusiones reales siguen sin establecerse.

## 2. Cambios implementados

- #608: publicación/recuperación duradera de imágenes, video, audio y texto, continuidad de autenticación e invitaciones internas posteriores con configuración y agregados para el organizador.
- #615: moderación de la fuente mediante el sistema existente; reparación aditiva de autorización con organizador NULL, revocaciones concurrentes, locks hasta commit, versiones/replay, auditoría e historial. SQL histórico intacto.
- Móvil #168/#169/#170: publicación remota diferenciada de borradores, confirmación estricta y barreras de cuenta/sesión/evento.
- #172: texto sin archivo y lectura de texto/audio, con capacidad extendida explícita del servidor. Consultar capacidad no cambia preferencias.
- #173: selección y publicación pública de archivos de audio hasta 25 MB; DocumentPicker compatible con SDK54, caché para lectura, validación y recuperación de errores. Runtime aislado; sin grabación ni permiso de micrófono.
- #174: editar descripciones y eliminar publicaciones propias mediante PUT/DELETE existentes. Conserva microsegundos para concurrencia, exige confirmación de borrado y respuesta válida antes de actualizar el feed; descarta callbacks/respuestas retirados. Diálogos ES/EN con Cancelar de 60 × 44 píxeles CSS en la previsualización.

- Corrección de entrada pública: el enlace canónico muestra primero la proyección pública existente; al publicar, comentar o crear una cuenta conserva el destino canónico completo. Eventos privados/no disponibles mantienen el acceso mediante autenticación; un fallo de red ofrece reintento explícito. La ruta pública antigua conserva su sanitizador de parámetros.

## 3. Arquitectura y decisiones

El servidor conserva la autoridad de lectura, autoría, moderación y mutación. Se mantienen barreras de sesión/Party y fuente/parent; ningún nombre visible acredita identidad. Las capacidades nuevas se habilitan explícitamente, sin convertir borradores locales o respuestas inválidas en publicaciones sincronizadas.

Las campañas reutilizan notificaciones internas, preferencias explícitas y señales de elegibilidad diferenciadas. Entrega/recibo se confirman atómicamente, con unicidad por evento/cuenta/paso, límites de frecuencia y como máximo un recordatorio; publicar suprime el recordatorio. Instalación, RSVP, participación profesional o seguimiento no conceden consentimiento comercial. Correo, push y WhatsApp no se activan implícitamente.

Los archivos de este flujo tienen enlaces públicos; no se ofrece almacenamiento privado/revocable sin poder aplicarlo. El borrado elimina la publicación, sin prometer revocar objetos públicos o copias externas. #174 lo explica antes de confirmar.

## 4. PRs, commits e integración

| Entrega | Estado acreditado |
| --- | --- |
| [Raíz #608](https://github.com/diegueins680/tdf-app/pull/608) | `534c5631`, borrador; su aprobación histórica no aprueba una integración posterior |
| [Moderación #615](https://github.com/diegueins680/tdf-app/pull/615) | `31c16bff`, CI técnico satisfactorio; revisión independiente y verificación de dos hallazgos pendientes |
| [Móvil #170](https://github.com/diegueins680/TDF-mobile/pull/170) | `7000df1d`, revisión de la cadena #169/#168 pendiente |
| [Texto móvil #172](https://github.com/diegueins680/TDF-mobile/pull/172) | `f5197c69`, CI satisfactorio; revisión solicitada, auto-merge desactivado |
| [Audio móvil #173](https://github.com/diegueins680/TDF-mobile/pull/173) | `6d0b109`, validación y ambos builds satisfactorios; revisión independiente pendiente |
| [Autoría móvil #174](https://github.com/diegueins680/TDF-mobile/pull/174) | `41a68b7`, 759 pruebas/101 suites y ambos builds satisfactorios; listo para revisión, sin aprobación ni auto-merge |
| [Entrada pública](https://github.com/diegueins680/tdf-app/tree/fix/event-memory-public-entry-20261026) | Core `87c3ed194`, base `069fac6`; calificado localmente, pendiente de revisión independiente e integración |
| Preparación monorepo | Rama `integrate/event-memories-main-78fa-20261026`: merge normal `a95bc6745` con main `78fa351b` (#621 sobre #620); runner calificado `3a17a9f86` |

La nueva preparación incorpora las reparaciones protegidas del reproductor y las identidades/cabeceras contables. Conserva **los 145 requisitos de main exactos más cuatro propios: 149**, el prefijo protegido de 222 migraciones más las cuatro originales: **226**, y todos los bytes SQL/ancestros de introducción. El pin móvil de esta preparación sigue `7000df1d`; las calificaciones posteriores #172–#174 conservan sus identidades separadas y requieren integración nativa revisada. No se alteraron #608/#615 ni ramas ajenas para sustituir revisiones pendientes. El checkout original divergente se conserva.

## 5. Pruebas ejecutadas y resultados

La [corrección de entrada pública](event-memory-source-evidence-20261025/public-entry-qualification.json), core `87c3ed194`, pasa **3540 pruebas / 311 suites**, lint, TypeScript, build (336422 bytes gzip, cinco preloads), `quality:repo`, catálogo estricto JSON/CSV de **1346** entradas y ocho controles de historial. La admisión mapea doce superficies modificadas; mantiene exactos los 145 requisitos protegidos, 149 totales, y el backend/esquema de 226 migraciones. Estos son controles ejecutados; la cobertura estructural no prueba toda la especificación.

La [API/PostgreSQL reales de esta corrección](event-memory-source-evidence-20261025/public-entry-http-browser-results.json) pasan **120 peticiones / 18 grupos / 258 hashes**, con ledger226 exacto. Se conserva la identidad real del mismo binario Darwin `262295679`/SHA f01ef34b: sus inputs siguen iguales y no se atribuye una recompilación al nuevo commit. Los únicos archivos sin seguimiento durante el recorrido eran dos enlaces propios de dependencias; no es una atestación de release limpio.

La siguiente evidencia sobre78fa conserva su ejecución e identidad anteriores; no se presenta como una repetición en87c3ed:

[Recibo de la preparación sobre 78fa](event-memory-source-evidence-20261025/78fa-main-preparation.json):

- Web, Node22.14.0: **3522 pruebas / 310 suites**, lint, tipos y build satisfactorios; presupuesto de cinco preloads y 336424 bytes gzip iniciales.
- `quality:repo`, contratos de los dos clientes, admisión de 57 superficies modificadas, inventario/trazabilidad estructural y catálogo estricto JSON/CSV de **1345 entradas** pasan. Ocho controles de historial de catálogo pasan; no equivalen a refinamiento completo de los 149 requisitos.
- SQL completo de 226 migraciones: instalación/replay, rollback/reaplicación permitida, rechazo de sucesores desconocidos, organizador NULL y **ocho intercalados reales de revocación** pasan. Invitaciones: concurrencia, consentimiento, supresión tras publicar y rechazo de rollback después de evidencia pasan; 160 estados acotados y tres controles negativos, con refinamiento SQL todavía no probado globalmente.
- El esquema combinado pasa las regresiones contables de main: **16 mutaciones de identidad/fuente/auditoría rechazadas y cuatro controles de startup**, con restauración exacta de definiciones/recibos.
- API/PostgreSQL reales: **120 peticiones / 18 grupos / 250 hashes**; el ledger observado coincide exactamente con las **226** migraciones antes y después del recorrido. [Resultado](event-memory-source-evidence-20261025/78fa-http-browser-results.json).
- La API usa el binario Darwin previamente compilado `262295679`/build05:36:20Z/SHA f01ef34b, con **278 inputs de compilación byte por byte iguales**, incluidos los embeds y autoridades Cabal/Stack. [Correspondencia](event-memory-source-evidence-20261025/78fa-input-correspondence.json). No es una compilación del merge ni una imagen Linux. Los 4041 ejemplos/21 pendientes del backend anterior conservan su ejecución original; los pendientes no cuentan como pases nuevos.

Los intentos iniciales fallidos de preparación/harness están registrados; no se cuentan como éxitos ni se debilitaron las comprobaciones para aprobarlos.

## 6. Evidencia web y móvil

El [nuevo recorrido integrado](event-memory-source-evidence-20261025/public-entry-browser-results.json) pasa **ocho comprobaciones**: evento finalizado público en escritorio/Pixel7 sin leer momentos protegidos; fallback privado con destino conservado; fallo de red inyectado/reintento real; enlaces de registro con contexto; dos sesiones reales, publicación, denuncia, cierre, **409 real obligatorio**, ocultamiento/restauración y recarga. [Entrada anónima Pixel7](event-memory-source-evidence-20261025/public-entry-anonymous-phone.png), [autor](event-memory-source-evidence-20261025/public-entry-source-desktop.png) y [lector](event-memory-source-evidence-20261025/public-entry-source-phone.png). Son fixtures sintéticos con API/almacenamiento reales en aislamiento; no es producción, registro por interfaz con una persona ni ejecución nativa.

Dos sesiones sintéticas reales en [escritorio](event-memory-source-evidence-20261025/78fa-source-desktop.png) y [Pixel 7 web](event-memory-source-evidence-20261025/78fa-source-phone.png) verifican autenticación conservando contexto, publicación persistida, denuncia, cierre, ocultamiento por teclado y restauración tras recargar ambas sesiones. La prueba exige **un 409 real** por versión deliberadamente incompatible, conservación del contenido y recuperación mediante cancelar/recargar/revisar. [Cinco comprobaciones del navegador](event-memory-source-evidence-20261025/78fa-browser-results.json). No son teléfonos físicos.

#174, Node20.19.4: **759 pruebas / 101 suites**, release:check y Metro pasan. [Validate40359050881](https://github.com/diegueins680/TDF-mobile/actions/runs/40359050881): mismas pruebas, 13 controles de artefactos ejecutados y un skip macOS en Linux, Doctor17/17. Los 14 controles locales macOS originales conservan su identidad; fuentes de firma/nativas sin cambios.

[Android40359055946](https://github.com/diegueins680/TDF-mobile/actions/runs/40359055946) e [iOS Simulator40359061603](https://github.com/diegueins680/TDF-mobile/actions/runs/40359061603), incluido `pod install --deployment`, pasan. Se descargaron e inspeccionaron ambos artefactos del checkout real **41a68b7**: acciones nuevas en JS, DocumentPicker, runtime y loopback; iOS con OTA desactivado. [CI](event-memory-source-evidence-20261025/mobile-author-all-native-ci-20261026.json), [artefactos](event-memory-source-evidence-20261025/mobile-author-native-artifacts-verified.json) y [verificador reproducible](event-memory-source-evidence-20261025/verify-author-native-artifacts.py). Tres controles negativos rechazan fuente incorrecta y artefactos anteriores. No acredita instalación, ejecución nativa, firma productiva ni tiendas.

El [recorrido Expo Web del cliente móvil](event-memory-source-evidence-20261025/mobile-author-preview-results.json) comprueba edición/cancelación/borrado con API sintética. El worker QR externo preexistente bloqueado produce un error conocido conservado; no se acredita el escáner ni cero errores globales. Sus [capturas](event-memory-source-evidence-20261025/mobile-author-delete-confirmation.png) se distinguen de la prueba web contra API real. Los builds cancelados anteriores de `41505ce` no cuentan como pases de `41a68b7`.

## 7. Despliegue y producción

**Sin integración protegida, despliegue, migración, cambio de preferencias ni escritura productiva de esta entrega.** La lectura pública de 16:48 UTC responde200 para página/evento/salud/versión, DB ok; backend publicado **4dac175d**, anterior a esta funcionalidad. Es disponibilidad y estado público observado, no aceptación productiva de los cambios nuevos.

El despliegue requiere revisión/integración protegidas, imagen canónica, procedimientos vigentes de Hetzner, recuperación/canary y aceptación real. Después podrá corregirse el estado confirmado del evento141 y verificarse su lectura/publicación antes de activar cualquier campaña. No se evaden protección de ramas, aprobaciones ni FIFO.

## 8. Invitaciones reales del evento141

| Indicador de esta campaña | Resultado acreditado |
| --- | --- |
| Elegibles con consentimiento / excluidos | **No establecidos** |
| Intentadas / enviadas / entregadas / fallidas | **0 / 0 / 0 / 0** |

Las entregas de prueba usan exclusivamente identidades sintéticas en bases desechables. No son envíos reales ni acreditan consentimiento de las cuentas productivas. No se extrajeron ni publicaron contactos personales.

## 9. Métricas y límites de medición

Están implementados recibos internos, aperturas atribuidas, publicación posterior y agregados por campaña, junto con interacciones existentes. Aún no hay observación productiva de este flujo ni prueba de recepción en proveedores externos. Adquisición, conversión, primer valor y retornoD7/D30 deben medirse después del despliegue con datos autorizados. **No se atribuye aumento de conversión, retención o conexiones a estas pruebas.**

## 10. Riesgos, bloqueos y prioridades siguientes

1. Obtener la revisión independiente de #615, de la corrección de entrada pública y de la cadena móvil; las solicitudes existentes siguen pendientes. No inferir aprobación a partir de CI o de “Continue”.
2. Integrar la fuente revisada en #608, mantener la base protegida vigente y obtener CI/revisión nuevos del head final, resolviendo las conversaciones y respetando FIFO. Las aprobaciones antiguas no cubren cambios posteriores.
3. Completar imagen/recovery/canary/despliegue y aceptación productiva. Solo entonces verificar elegibilidad y preferencias reales y ejecutar invitaciones trazables/idempotentes del evento141.
4. Resolver brechas explícitas: medios privados/revocables y borrado físico, galerías mixtas por publicación, etiquetado estructurado, idempotencia general de creación, previews específicos de publicaciones, pruebas nativas en dispositivos y mediciónD7/D30. Los archivos públicos nunca se presentan como restringidos.

La API, Vite y las bases de pruebas propias se retiraron; el cluster propio se detuvo tras verificar que solo quedaba `postgres`. Se conservan logs y caches de dependencias; no se detuvieron servicios compartidos. La preparación y su evidencia no constituyen autorización de producción ni finalización de los criterios pendientes.


### Entrada pública reparada y probada; despliegue pendiente

La [observación anónima productiva de17:42 UTC](event-memory-source-evidence-20261025/public-event141-browser-before-entry-repair.json) termina en `/login?redirect=%2Fsocial%2Feventos%2F141`, sin escrituras. La nueva corrección permite explorar la proyección pública existente desde esa ruta y conserva Shell/permisos para usuarios autenticados, con comprobación integrada local satisfactoria. Esta corrección todavía no está verificada en producción. La observación previa no se cambia ni se usa como evidencia de que ya se desplegó.
