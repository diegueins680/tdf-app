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
- Web: 3.388 pruebas en 300 suites aprobadas durante la composición; después del merge,
  84 pruebas enfocadas en siete suites repitieron moderación, caché entre cuentas, perfil y
  carga por cookie. Tipos, lint y build pasan (334.201 bytes gzip de JavaScript inicial).
  La nueva prueba de caché falló primero por un texto de estado vacío incorrecto en el
  test; se corrigió el texto esperado y se mantuvo la aserción de ausencia entre cuentas.
- Mobile 3b455ef90df5dd54a3fe45ec0f8ad5b9e90068b4: 631 pruebas en 98 suites, tipos, lint,
  release:check y arranque Metro /status aprobados sobre Node 20.19.4. Solo cambió el
  contrato generado; [PR #169](https://github.com/diegueins680/TDF-mobile/pull/169),
  apilado sobre #168. Mobile Validate 40227879953 se sigue por separado; no se cuenta
  una ejecución en curso como aprobada.
- Auditoría de catálogos: siete regresiones aprobadas; se conserva la cadena completa
  de aprobaciones y fuentes del registro de migraciones. Los tres candidatos nuevos
  corresponden al registro de ejecución, los discriminantes técnicos de acciones y los
  estados de seguridad cerrados por SQL, revisados explícitamente.
- quality:repo terminó satisfactoriamente. La comprobación adicional del índice detectó
  documentación posterior sin regenerar; se regeneraron los índices y se repitieron satisfactoriamente sus
  comprobaciones, sin omitir el control.

Después de registrar las 215 migraciones, se repitieron satisfactoriamente el runner
de invitaciones (incluida concurrencia y tres controles negativos), el runner de
moderación (incluido control negativo) y el recorrido real HTTP/navegador: otra vez
112 solicitudes y 15 grupos. Se retienen los nuevos [recibos HTTP](event-memory-source-evidence-20261025/http-215-results.json)
y de [navegador](event-memory-source-evidence-20261025/browser-215-results.json), basados
en fa468b98 con hashes de ejecución y árbol local modificado.

Los [recibos HTTP](event-memory-source-evidence-20261025/http-results.json) y de
[navegador](event-memory-source-evidence-20261025/browser-results.json) conservan los hashes
reales de los archivos y del ejecutable. Se ejecutaron sobre el árbol local modificado basado en
dc9fb129, no sobre un commit limpio posterior; no se cambia su identidad retrospectivamente.
La primera prueba integrada de navegador falló porque el driver esperaba botones de
interacciones antes de desplazar la tarjeta de carga diferida a la vista; se corrigió
el desplazamiento del driver, sin reducir las aserciones de persistencia o permisos.
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

La [lectura pública de las 19:52 UTC](event-memory-source-evidence-20261025/production-public-read-only.json)
confirmó HTTPS 200 para la página, health y version, con DB ok y API todavía 4dac175d.
Esto prueba disponibilidad y versión, no publicación productiva ni autenticación.
El lector web remoto no accedió al sitio y el Python local no encontraba su CA;
la comprobación posterior utilizó curl del sistema con validación TLS activa.

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


## Continuación, 25/oct 21:40 UTC

Los runs iniciales 40227718934 (#608) y 40228461354 (#615) fallaron en el único
caso WebKit desktop: el test iniciaba otra navegación mientras seguía entrando
al login desde el evento. Se conserva el fallo; no se presenta como aprobado.
El recorrido ahora pulsa «Crear cuenta general» en la pantalla renderizada y
comprueba el contexto al abrir el registro. Además, GET /session se representa
como 200/null para visitantes anónimos, conforme al contrato y handler reales,
manteniendo 401 como caso separado de sesión caducada. Ambas variantes conservan
publicación, error/reintento, recarga y auditoría de accesibilidad. No se añadieron
skips ni retries. Las dos variantes pasaron en ocho casos Chrome de cuatro tamaños
y dos casos Firefox; previamente pasaron los 28 recorridos de acceso/publicación.
WebKit Linux sigue siendo obligación del CI del nuevo head.

Se incorporó normalmente el main protegido 000cf8ad3 (#609) y el padre #608
baa1f8cc5. Los 141 requisitos del padre se preservan íntegros; hay 142 con moderación.
El registro mantiene sus 215 entradas y las dos migraciones originales de recuerdos
sin cambiar sus bytes ni introducedBy. La revisión independiente aprobó 7b16d44
antes de esta actualización; esa aprobación no autoriza los nuevos heads.

La compilación canónica Stack/GHC 9.10.3 sobre la composición financiera pasó;
Hspec volvió a ejecutar 4.037 ejemplos, cero fallos y 20 pendientes. Se ejecutaron
además los cuatro grupos musicales obligatorios, todos no vacíos: 25/10/22/4 casos
sin fallos. El runner financiero ejecutó el store real, detectó los tres mutantes
de reloj y pasó los 308 casos de proveedor. Es evidencia sintética e independiente
de la publicación de recuerdos; no son operaciones o pagos productivos.

Los dos runners SQL de recuerdos volvieron a pasar. El [HTTP actual](event-memory-source-evidence-20261025/http-current-main-results.json)
y el [navegador actual](event-memory-source-evidence-20261025/browser-current-main-results.json)
pasaron con 113 solicitudes y 16 grupos, incluida la respuesta real de sesión
anónima 200/null. El ejecutable tiene SHA256
830082822b64ef620152d1e5632cd9a2fd0c4a06f92cb7197bb682fefcbf3d1c.
Los recibos conservan 98ddfb6a como revisión base y el árbol local modificado;
no se les atribuye retrospectivamente el head limpio de publicación. La captura
compilada de API y la admisión de fuente se verifican aparte con sus herramientas.

Los intentos iniciales de verificación local fallaron por invocación de Stack
desde un directorio sin proyecto, puerto de arranque PostgreSQL omitido y ruta
relativa del binario. Esos logs se conservan como fallos de preparación. Se corrigió
el contexto explícito tdf-hq/stack.yaml, se reinició solo el clúster propio en
127.0.0.1:55526 y se ejecutó el test desde tdf-hq; únicamente las ejecuciones
posteriores satisfactorias sustentan las cifras anteriores.

La consulta productiva de solo lectura de las 21:36 UTC sigue mostrando siete
cuentas relacionadas, cinco activas, cinco profesionales y tres RSVP, con un
solapamiento; 141 continúa on_sale, sin esquema de campaña ni completed público.
Envíos intentados/enviados/entregados/fallidos: 0/0/0/0. No hay consentimiento
recogido para esta campaña. La confirmación de Diego sobre lo ocurrido se mantiene.

El defecto real de restauración de sesión tras un logout fallido, detectado por
el flujo de activación, bloquea una aceptación con personas reales hasta que su
reparación separada esté integrada y verificada. Esta rama no modifica la sesión
ni despliega esa reparación. CI, revisión independiente y FIFO siguen vigentes;
no hubo escritura de datos productivos, despliegue ni comunicaciones reales.


## Cierre verificable de esta continuación — 25/oct, 23:00 UTC

**Implementado y probado localmente; pendiente de CI/revisión del nuevo head;
no integrado ni desplegado.** Se incorporó main b732efd65 (#610) y el padre
#608 2c3b0017a mediante merge normal. Sus 142 requisitos permanecen intactos;
con moderación y propiedad de sesión hay 144. Las 215 migraciones y sus bytes
históricos no cambian; no hay nuevas dependencias ni servicios.

La regresión de sesión produjo tres contraejemplos antes de la corrección:
un envío del borrador, un opt-in y una decisión podían adoptar las credenciales
nuevas antes de actualizarse React. Se trasladó la autoridad genérica existente
del editor de perfiles a useSessionOwner/useOwnedSessionDraft, manteniendo su
entrada antigua como fachada. Cada intento conserva propietario, época y evento;
los resultados tardíos no borran el borrador ni cambian el caché de otra cuenta.
La selección de archivo anterior tampoco puede salir por la nueva sesión.

La prueba adicional utiliza SessionProvider, credenciales imperativas, API de
preferencias y cliente HTTP reales con red sintética: cero PUT del checkbox
anterior, y un PUT autenticado tras una acción explícita de B. El control negativo
omitiendo únicamente esa protección detectó un PUT con token sintético B y JSON
true; se restauraron exactamente los bytes originales. No es un incidente de
producción. La corrección del logout fallido sigue siendo una dependencia separada.

También se rechazan confirmaciones HTML o malformadas de publicación, edición,
eliminación, preferencias y campañas. Un opt-out debe confirmar el valor solicitado;
un HTML 200 no puede mostrarse como unsubscribe exitoso. La creación requiere ID
persistido y forma de medio compatible, y se comprueba el evento/fuente cuando se
incluye. Se admiten respuestas heredadas sin eventId. Un resultado incierto conserva
el editor y recomienda comprobar el evento antes de reintentar; no implementa
idempotencia general de creación ni garantiza un commit solamente por recibir JSON.

Resultados separados, sin sumar casos que se solapan:

- Suite completa tras propiedad de sesión: 3.415 pruebas/303 suites, cero fallos.
  Precede a las últimas validaciones SDK y al merge de menús; CI final debe repetir
  la suite completa sobre el commit limpio de publicación.
- Composición de menús/propiedad: 70 pruebas/8 suites, cero fallos.
- Límites de sesión, archivos y respuestas tardías: 35 pruebas/5 suites, cero fallos.
- SDK/acknowledgements, proveedor de sesión y página: 51 pruebas/4 suites, cero fallos.
- Tipos, lint, auditoría estricta de catálogos y metadatos/admisión se comprueban
  sobre la fuente final; los fallos iniciales de preparación/lint se conservan.
- [API y DB finales](event-memory-source-evidence-20261025/http-session-safe-results.json):
  113 solicitudes/16 grupos, incluida sesión anónima real 200/null, concurrencia,
  persistencia y rechazo de rollback con evidencia. [Navegador final](event-memory-source-evidence-20261025/browser-session-safe-results.json):
  escritorio y teléfono reales sobre API/DB sintéticas, publicación/denuncia/cierre/
  ocultación/restauración y recarga, sin salida a proveedores. El recibo conserva
  la revisión dd2d48ab2 con árbol modificado y el ejecutable 830082822b64…;
  sus hashes críticos deben seguir coincidiendo con los archivos actuales.

El [recibo de regresiones](event-memory-source-evidence-20261025/session-boundary-regression.json)
retiene hashes de los logs negativos y positivos y sus límites. Las cifras no son
usuarios adquiridos ni métricas productivas. Última consulta real de solo lectura:
21:36 UTC, 141 on_sale, siete cuentas relacionadas/cinco activas, sin esquema
instalado, sin consentimiento recogido. Intentadas/enviadas/entregadas/fallidas:
**0/0/0/0**. Exclusiones finales y destinatarios consentidos no están acreditados.

La aprobación previa de un head diferente no autoriza estos cambios. Permanecen
CI obligatorio, revisión independiente, FIFO, recuperación y ensayo canónico,
despliegue y comprobación productiva. Siguen pendientes medios privados/revocables,
galerías combinadas, audio/texto nativo, previsualizaciones públicas específicas,
idempotencia de creación y retención D7/D30. No se afirma cumplimiento total.


Última validación de la fuente congelada: 69 pruebas en siete suites pasan,
incluyendo selección de archivo sin confirmación, conservación/reintento del
archivo, sesión real y DTOs; tipos y lint pasan. La auditoría de repositorio termina
satisfactoriamente. Las cifras anteriores conservan sus ámbitos y no se suman.
El [recibo definitivo de API/DB](event-memory-source-evidence-20261025/http-release-candidate-results.json)
y el [recibo definitivo de navegador](event-memory-source-evidence-20261025/browser-release-candidate-results.json)
repiten 113 solicitudes/16 grupos después de esos cambios; retienen ad56c1896 y
el árbol local modificado, con hashes de los SDKs, cliente HTTP y autoridad de sesión.
Una carga con respuesta incompleta no se guarda como URL confirmada ni bloquea el
reintento del archivo. No hay nuevas migraciones ni cambios en el ejecutable backend.

#608 2c3b0017a obtuvo aprobación independiente posterior a su publicación, a las
22:39:34 UTC, y sus 19 controles técnicos requeridos pasaron. El único control
pendiente era main-integration-admission, en espera de #611 sobre main b732efd65.
Ese permiso no autoriza el nuevo head de #615. Ambos se mantienen en borrador:
la entrega base debe incorporar la corrección P0 revisada antes de una publicación
con usuarios. No se usa una revisión de otro commit ni se cambia el orden de cola.
