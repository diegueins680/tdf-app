# Moderación de recuerdos — continuación del 25 de octubre de 2026

## Resultado y alcance

Se implementó la denuncia y moderación de la publicación fuente reutilizando EventMoment,
interaction_report, interaction_audit e interaction_request. Este cambio sucede al PR raíz
[#608](https://github.com/diegueins680/tdf-app/pull/608) y al cliente móvil
[#168](https://github.com/diegueins680/TDF-mobile/pull/168). El diagnóstico y los diez entregables
originales permanecen en [el informe de la primera entrega](event-memories-2026-10-25.md).
La continuación todavía no está integrada ni desplegada. No se han enviado invitaciones reales.

## Contrato, permisos y recuperación

El lector autenticado puede denunciar contenido ajeno. El organizador vigente puede ocultar y
restaurar recuerdos de su evento; la capacidad existente de moderación de plataforma permite
retirar y restaurar contenido dentro del acceso autorizado al evento. Ser autor, colaborador
logístico o tener RSVP no concede moderación. Un administrador sin acceso a un evento privado
no obtiene acceso por este cambio. El organizador no puede revertir un retiro de plataforma.

Las lecturas actuales y heredadas, comentarios, reacciones, destinos y notificaciones consultan
el mismo estado fuente. Pausar interacciones no hace visible una publicación oculta. La revisión
ofrece páginas acotadas, motivos e historial sin identidades de denunciantes; permite cerrar
una denuncia sin ocultar. Los cambios de cuenta separan el caché web y cancelan decisiones abiertas.

Cada decisión exige versiones actuales del contenido y del destino; el ledger existente conserva
la clave y el payload. Una decisión antigua no puede volver a ocultar una publicación restaurada.
Solo un cambio real de contenido permite reabrir denuncias resueltas: ocultar/restaurar y las
ediciones sin cambios no cuentan. Los reintentos no duplican reportes ni auditoría.

La migración siguiente al registro 214 conserva los OID de las autoridades públicas, captura sus
implementaciones exactas y detecta sucesores inesperados. La reversión conserva datos y columnas,
restaura las funciones originales antes de evidencia y se niega después de reportes o acciones.
No se modifica ninguna migración histórica. OpenAPI y ambos clientes generados se actualizan;
la API compilada conserva 913 operaciones y añade únicamente el parámetro opcional sourceEvent
al GET /interactions/reports. Sin ese parámetro permanece la bandeja de denuncias de comentarios.

**Límite material:** ocultar publicaciones no revoca URL públicas ni elimina archivos o copias
externas. La interfaz lo explica antes de confirmar. No se ofrece una opción ficticia de medios
privados. Las galerías combinadas, publicación nativa de texto/audio, revocación física de medios,
idempotencia general de creación y medición completa de retención siguen pendientes.

## Evidencia ejecutada

- Stack canónico, GHC 9.10.3/lts-24.42: compilación satisfactoria; Hspec 4.037 ejemplos,
  cero fallos, 20 pendientes (los pendientes no se consideran aprobados).
- PostgreSQL 16 aislado: esquema completo, aplicación/reaplicación, reversión exacta/reaplicación,
  permisos, contenido actualizado, revocación de rol y denuncias. Control negativo: restaurar
  temporalmente el resolver anterior hace visible la fuente oculta; el control detecta el defecto
  y la autoridad corregida vuelve a denegarla. Todas las mutaciones del control se revierten.
- API real autenticada: 112 solicitudes y 15 grupos de aserciones, medios PNG/WAV/MP4/MOV,
  publicación y recuperación tras reinicio, discusión, IDOR, privacidad, concurrencia entre dos
  decisiones (un efecto y un conflicto), replay antiguo y rechazo de reversión con evidencia.
- Navegador Chrome real, sesiones aisladas de escritorio y Pixel 7: publicación de texto,
  persistencia tras recarga en teléfono, denuncia, cierre sin ocultar, ocultación por teclado,
  denegación tras recarga en ambas sesiones y restauración persistida. Todo el contenido y las
  identidades son sintéticos. Se bloquearon 26 solicitudes externas. El servidor web propio
  terminó correctamente. No es una prueba de dispositivo físico ni de producción.
- La verificación web completa, cliente móvil generado, lint, tipos, build y admisión de fuente
  se registrarán con sus resultados finales antes de solicitar revisión.

Los [recibos HTTP](event-memory-source-evidence-20261025/http-results.json) y de
[navegador](event-memory-source-evidence-20261025/browser-results.json) conservan los hashes
reales de los archivos y del ejecutable. Se ejecutaron sobre el árbol local modificado basado en
dc9fb129, no sobre un commit limpio posterior; no se cambia su identidad retrospectivamente.
La inspección de la API compilada es descubrimiento de tipos, no prueba de refinamiento completo.
Capturas: [escritorio](event-memory-source-evidence-20261025/desktop.png) y
[teléfono](event-memory-source-evidence-20261025/phone.png).

## Piloto 141 y comunicaciones

Diego confirmó que PATCH CULTURE Vol. 1 se celebró y ya finalizó. No se vuelve a pedir esa
confirmación. La lectura productiva del 25/oct a las 19:03 UTC seguía mostrando on_sale;
las migraciones de recuerdos y la lectura pública de completed todavía no estaban instaladas.
La corrección de estado debe esperar al soporte desplegado y verificado para evitar ocultar
el evento. La confirmación no acredita presencia de cada RSVP ni consentimiento de comunicaciones.

La consulta de solo lectura identificó siete cuentas relacionadas, cinco activas, cinco por
participación profesional y tres por RSVP, con un solapamiento. No hay entradas/admisiones
verificadas ni preferencias recogidas para esta campaña. No hay un total de destinatarios
consentidos acreditado. Invitaciones reales intentadas/enviadas/entregadas/fallidas: **0/0/0/0**.
No se deduce una cifra de exclusiones definitivas a partir de relaciones o pruebas sintéticas.

## Integración y siguientes pasos

Mantener revisión independiente, CI obligatorio y orden de integración. El PR #607 se integró
el 25/oct a las 19:40:53 UTC; #608 se actualiza mediante merge normal del main protegido y
requiere aprobación independiente del nuevo head. Esta continuación se publicará en un PR
apilado y un PR móvil del contrato. No se evade ninguna protección.

Después de integrar: imagen inmutable, ensayo canónico, respaldo y recuperación conforme al
runbook de Hetzner; migraciones revisadas; verificación productiva con identidades sintéticas;
corrección autorizada del ciclo de vida; comprobación de preferencias reales; campaña controlada
solo cuando todos los requisitos de elegibilidad, consentimiento y entrega se cumplan.
Las métricas disponibles siguen siendo entregas internas persistidas, aperturas atribuidas y
publicaciones posteriores. No hay evidencia de mejora de conversión ni retorno a 7/30 días.
