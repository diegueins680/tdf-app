# TDF Records — Informe ejecutivo de recuerdos de eventos

Corte: 26 de octubre de 2026, 08:07 UTC. **Implementación parcial probada y publicada para revisión; sin integración protegida, despliegue ni invitaciones reales de esta entrega.** El objetivo completo sigue pendiente.

## Diagnóstico y reutilización

Se reutilizan eventos, RSVP, perfiles/Party, momentos, almacenamiento del evento, sesión, API/OpenAPI y el sistema canónico de interacciones, permisos, reportes y auditoría. No se crea otro feed social ni otra autoridad de moderación. El RSVP expresa intención y no acredita presencia. El diagnóstico detallado y las diferencias entre especificación e implementación están en [el informe inicial](event-memories-2026-10-25.md) y [la reparación de autorización](event-memory-authorization-repair-2026-10-25.md).

El usuario confirmó que PATCH CULTURE Vol. 1, evento 141, se celebró y finalizó. La auditoría DB registrada el 26/oct 03:22 UTC observó estado `on_sale`, siete cuentas relacionadas/cinco activas, cinco vínculos profesionales y tres de intención RSVP con un solapamiento. No son siete asistentes ni siete destinatarios autorizados. El estado se corregirá después de desplegar y verificar la lectura de eventos finalizados. No se recogieron preferencias productivas de esta campaña.

## Implementación y arquitectura

- Base #608: publicación y recuperación duradera de texto, imágenes, video/audio mediante las capacidades del evento; continuidad de autenticación, enlaces, interacciones y activación posterior. Campañas internas con preferencias explícitas, elegibilidad, deduplicación, límites/replay y como máximo un recordatorio, con configuración y agregados para el organizador. Correo/push/WhatsApp no se activan implícitamente.
- #615: moderación de la fuente mediante interacciones existentes y migraciones aditivas. Corrige autorización permisiva ante organizador NULL y la carrera de revocación; mantiene locks de permisos hasta commit, control de versiones/replay, auditoría e historial. No reescribe SQL histórico ni elimina datos para hacer reversible una reparación.
- Cadena móvil #168/#169/#170: carga y publicación remotas diferenciadas de borradores, validación de confirmaciones y barreras de cuenta/sesión/evento para operaciones tardías.
- Nuevo #172: publicación escrita sin archivos cuando el backend confirma compatibilidad, lectura correcta de audio/texto y recuperación del texto ante errores. La lectura de capacidad no cambia preferencias. Ese cambio no añadía dependencia ni selector; #173 completa la selección de archivos de audio.

## Cambios revisables e integración

| Entrega | Head/estado |
| --- | --- |
| [Raíz #608](https://github.com/diegueins680/tdf-app/pull/608) | `534c5631`, borrador; aprobación histórica de este head no aprueba el root final |
| [Moderación #615](https://github.com/diegueins680/tdf-app/pull/615) | `31c16bff`, CI técnico satisfactorio; revisión independiente pendiente |
| [Móvil #170](https://github.com/diegueins680/TDF-mobile/pull/170) | `7000df1d`, apilado sobre #169/#168; revisión pendiente |
| [Texto/audio móvil #172](https://github.com/diegueins680/TDF-mobile/pull/172) | `f5197c69`, CI satisfactorio y revisión solicitada a tdfrecords; auto-merge desactivado |
| [Audio móvil #173](https://github.com/diegueins680/TDF-mobile/pull/173) | `6d0b109`, 722 pruebas y builds APK/iOS Simulator satisfactorios; revisión independiente requerida |
| Preparación monorepo publicada | `39aa065e3` integra normalmente main `a1238fbf6` (#619); driver calificado `e3d3f6521` |

Durante la verificación main avanzó a `3c25a9de`, fusión de #620 (fiabilidad del reproductor). Ese sucesor **no está incluido** en la preparación A123 y requiere reconciliación y checks afectados antes de promover #608. No se actualizaron #608/#615 para sustituir revisión pendiente ni se modificaron ramas ajenas. El checkout original divergente se conserva.

## Pruebas y evidencia

- Preparación web A123 con Node 22.14.0: **3501 pruebas / 309 suites**, lint, tipos y build pasan; cinco preloads, 335777 bytes gzip iniciales. `quality:repo`, catálogo estricto JSON/CSV de 1336 entradas y generación formal pasan. Conserva exactamente los 144 requisitos protegidos más cuatro propios y el manifest de 225 migraciones.
- API/PostgreSQL reales: **120 peticiones / 17 grupos / 24 hashes**, publicación y recuperación de medios, permisos, moderación, reinicio y controles de recuperación. El [recibo](event-memory-source-evidence-20261025/a123-http-browser-results.json) conserva el binario real `262295679`/SHA f01ef34b, cuyos inputs backend/SQL/contrato/dependencias coinciden byte por byte; frontend de aplicación `39aa065e3`, driver `e3d3f6521`. No se presenta como compilación de otro commit ni imagen Linux.
- Navegador: dos sesiones sintéticas en escritorio y Pixel 7 verifican publicación persistida, denuncia, cierre, ocultamiento mediante teclado, recarga y restauración. [Escritorio](event-memory-source-evidence-20261025/a123-source-desktop.png) y [vista móvil](event-memory-source-evidence-20261025/a123-source-phone.png). No es un teléfono físico.
- Móvil #172, Node 20.19.4: **684 pruebas / 98 suites**, lint, tipos, recursos/configuración y Metro offline pasan; bundle Android con cinco fuentes actuales verificadas byte por byte en el source map. [CI remoto exacto](https://github.com/diegueins680/TDF-mobile/actions/runs/40282201755): mismas 684 pruebas, 12 casos de firma/artefactos (11 ejecutados y un skip macOS en Linux) y Expo Doctor **17/17**. Dispositivo físico, iOS y tiendas no verificados.
- La calificación backend limpia anterior conserva **4041 ejemplos, cero fallos, 21 pendientes**; los pendientes no cuentan como éxitos. Las pruebas de revocación cubren ocho intercalados de transacciones reales. Sus evidencias anteriores conservan su identidad y no se suman como nuevas ejecuciones.

## Producción e invitaciones del evento 141

La [lectura pública de 08:07 UTC](event-memory-source-evidence-20261025/public-readonly-after-native-audio-20261026.json) responde HTTP 200 para evento/salud/versión y DB ok. Backend publicado: **4dac175d**, versión anterior. Es disponibilidad; no demuestra la funcionalidad nueva. No hubo migración, despliegue, cambio de estado del evento ni escritura productiva de esta entrega.

| Métrica de campaña real | Resultado acreditado |
| --- | --- |
| Elegibles con consentimiento | No establecido |
| Excluidos | No establecido |
| Intentadas / enviadas / entregadas / fallidas | **0 / 0 / 0 / 0** |

Las pruebas de invitaciones usan identidades y medios sintéticos y no son envíos reales. Los contadores técnicos y agregados de campaña implementados aún no constituyen observación productiva de adquisición, activación o retención. No hay mejora de conversión ni D7/D30 acreditada.

## Bloqueos y prioridades siguientes

1. Revisión independiente del head de #615 y de la cadena móvil; conservar los hallazgos de revisión abiertos hasta su verificación independiente.
2. Reconciliar main vigente, integrar la fuente revisada en #608 y obtener CI/revisión nuevos del root mediante las protecciones/FIFO existentes.
3. Construir/desplegar por el procedimiento autorizado de Hetzner, con recuperación/canary y verificación productiva. Corregir entonces el estado confirmado del evento 141 y establecer preferencias y elegibilidad reales antes de enviar cualquier invitación idempotente.
4. Resolver brechas de producto: medios privados/revocables y borrado físico, galerías mixtas, pruebas con audio en dispositivos físicos, etiquetado estructurado, idempotencia general de publicación, previews específicos y medición D7/D30. La visibilidad restringida no se ofrece como protegida cuando el almacenamiento no puede aplicarla.

Los procesos y bases de pruebas propios quedaron detenidos/retirados; se conservaron logs y destinos de dependencias. No se detuvieron servicios compartidos. Este informe distingue implementación, pruebas, integración y producción y no declara completos los criterios pendientes.


## Avance de esta continuación

#173 completa selección y publicación de archivos de audio de hasta 25 MB en Android/iOS, con aviso/botón de publicación pública, caché para lectura y recuperación de errores. Añade únicamente DocumentPicker compatible con SDK54 y un runtime nativo aislado. No se ofrece carga pública en eventos privados ni se confunde esta opción con consentimiento comercial.

[Android](event-memory-source-evidence-20261025/mobile-audio-android-artifact.json) e [iOS Simulator](event-memory-source-evidence-20261025/mobile-audio-ios-artifact.json) fueron compilados y sus artefactos descargados/verificados: módulo nativo, runtime y árbol exacto del head. El checkout GitHub real f1c0519 tiene el mismo árbol e408ce85 que 6d0b109. Son artefactos aislados de pruebas, sin publicación en tiendas ni ejecución física. Los catorce controles locales pasaron; en CI Linux hubo trece ejecutados y un skip de plataforma. [Detalle CI](event-memory-source-evidence-20261025/mobile-native-audio-ci-20261026.json).

La prueba web reforzada exige respuestas reales antes de declarar ocultamiento/restauración y ejecuta un 409 inyectado, lectura autorizada sin efectos y recuperación explícita. El primer fallo y los errores del harness se conservan; solo v8 califica ese recorrido. [Preparación y límites](event-memory-source-evidence-20261025/a123-main-preparation.json). No hay cambios productivos ni invitaciones reales; siguen pendientes las revisiones, main vigente, CI del root y controles de despliegue.
