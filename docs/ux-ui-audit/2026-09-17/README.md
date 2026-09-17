# Auditoría UX/UI TDF — checkpoint del 17 de septiembre de 2026

**PARTIALLY COMPLETE. La auditoría inicial integral sigue abierta.** Este checkpoint
no reduce el encargo a los hallazgos ya observados ni acredita cobertura de las
superficies pendientes. Todos los hallazgos confirmados, incluidos los menores,
sus dependencias y las regresiones introducidas siguen dentro del alcance.

## Estado y procedencia

- Workspace original `main` en `17a33eca11d585d84435af85340beece9b51d14e`, con cambios
  ajenos web/backend/móvil y memoria: conservado. Trabajo aislado en
  `/private/tmp/tdf-ux-ui-20260917`, rama `audit/ux-ui-complete-20260917`.
- Baseline raíz actualizado: `d6244296925e20ccb6b2261290204415201843f3`.
  El remoto avanzó después a `6f77a7eec`; debe conciliarse antes de merge.
- `.gitmodules` enlaza `tdf-mobile` a `diegueins680/tdf-mobile`; GitHub lo identifica
  como `diegueins680/TDF-mobile`. Baseline raíz fija `88751192c758b49edad88131f4f90094d150e7ee`.
  Se calificó móvil main `092785f`; parche móvil publicado `a1791d0` (seguimiento pendiente).
- Se consultaron 72 PRs raíz, 15 móviles y las incidencias #128/#130. Se inspeccionaron
  especialmente #359/#368 (onboarding con dependencias de eventos y gates rojos),
  #406/#87 (activación de artista) y el trabajo concurrente de recuperación crítica.
  No se importaron stacks completos ni se atribuyeron sus pruebas a este parche.
- Protección raíz: una aprobación independiente, descartada con nuevos cambios.
  `enforce_admins=false` no autoriza saltarse esa revisión. Móvil respondió 404 a la
  consulta de branch protection; se siguen exigiendo los gates y validación del encargo.

## Cobertura, roles y límites

[coverage.csv](coverage.csv) inventaría 409 entradas: 155 funciones registradas,
188 declaraciones de rutas (incluidos wrappers/aliases), pantallas Expo y HTML
estático. **No son 409 recorridos probados ni 409 pantallas únicas.** Por fila se
registran origen, acceso, dispositivos, idiomas y estados requeridos. El inventario
se reproduce con `node scripts/ux-audit-inventory.mjs` desde la raíz.

[role-access.json](role-access.json) ejecuta el evaluador real del cliente sobre
31 roles individuales, todos sus pares, anónimo y autenticado sin grants (498
combinaciones). Agrupa resultados idénticos sin omitir combinaciones. Los módulos
proceden de `modulesForRole` como baseline semilla; **el servidor usa grants
canónicos de la base de datos**, reglas por registro, flags y revisiones. La unión
de roles no implica ampliar todo permiso: strict-admin limita combinaciones.
La matriz no sustituye pruebas HTTP ni cubre todas las combinaciones de tres o más
roles, datos privados, scopes por registro o configuraciones dinámicas de producción.

Evidencia actual de navegador: fixtures sintéticos aislados de login, signup,
recuperación, búsqueda, reflow 320px, entradas, reservas, Domo y marketplace; desktop,
teléfono y tablet Chromium, recorrido crítico Firefox/WebKit. No usuarios reales,
OAuth real, cobros, correos ni modificaciones de clientes. Operaciones internas,
administración y variantes de cada ruta siguen con cobertura runtime pendiente.
No afirmar WCAG 2.2 AA integral por los checks axe de estos recorridos.

## Hallazgos y plan completo

[findings.json](findings.json) es el registro estable con evidencia, impacto,
severidad/confianza, causa, cambio, dependencias, aceptación y método de verificación.
La prioridad de ejecución es:

1. UX-001: coordinar recuperación del backend con el despliegue concurrente.
2. UX-002/003/007: readiness veraz, JSON UTF-8 y acceso sin almacenamiento del navegador.
3. UX-004/005: recuperar consentimiento móvil y coordinar la referencia compatible.
4. UX-006: completar coherencia ES/EN en llegada, acceso y navegación pública.
5. Revalidar cada hallazgo histórico del informe de 2026-09-05 y sus seguimientos;
   conservar IDs históricos al reconciliarlos, sin asumir resuelto por un informe o PR.
6. Completar todos los recorridos pendientes de coverage.csv: permisos múltiples,
   privacidad/bloqueos, reservas, pagos, colaboración, operaciones, administración,
   iOS y Android. Confirmar hipótesis antes de añadir cambios obligatorios.
7. Resolver cada hallazgo confirmado, ejecutar gates sobre la revisión final,
   obtener revisiones requeridas, mergear, actualizar submódulo, staging, despliegue,
   observación y distribución/publicación móvil. Un flag apagado no equivale a rollout.

Hipótesis pendientes: efecto sobre abandono/engagement y rendimiento real. No se
inventaron tasas, entrevistas, p75 ni mejoras causales. Las ideas de nuevas funciones
ajenas al alcance permanecen separadas; no se implementó ninguna.

## Capacidades y evidencia ejecutada

- Dependencias Node web/móvil instaladas. Navegadores Chromium/Firefox/WebKit
  disponibles. Varias acciones requirieron salir del sandbox para red, sockets,
  metadatos Git, Stack y simuladores; no se interpretaron fallos de sandbox como
  fallos del producto. `/usr/bin/java` no funciona; se usó OpenJDK 21.0.12.1 temporal.
- Web baseline: 210 suites / 2.005 pruebas; build y presupuesto inicial pasan,
  357.180 bytes gzip; análisis estático sin hallazgos. Esto no mide LCP/INP/CLS de campo.
- Navegador baseline: 28 aprobadas / 4 omisiones explícitas. Logs preservados en
  [evidence/](evidence/). El storage-denial agregado antes del fix falló 2/2, con
  capturas antes; tras el fix dirigido pasó 2/2. Regresión final ampliada pendiente.
- Móvil viejo: 326/327, timeout de 5 s en TicketCheckout anónimo. Main móvil: 456/456.
  Parche de consentimiento: 457/457 y lint. Al iniciar Metro se generaron tipos Expo
  más estrictos y se descubrió otro bloqueo TS2322 en navegación externa; corrección
  explícita de la rama `web` y revalidación final pendientes. No se borraron tipos
  generados para ocultar el error ni se ampliaron timeouts.
- Readiness: `stack exec -- runghc -isrc -itest /private/tmp/tdf-ux-startup-spec.hs`
  ejecutó dos ejemplos Hspec, cero fallos. El runner temporal importa
  `TDF.StartupResponseSpec`; la suite normal también lo importa desde `test/Spec.hs`.
  Esto no sustituye compilación/gates completos del backend.
- `npm run quality:repo` pasó antes de las correcciones finales. Sus fixtures que
  imprimen git push/release no son operaciones del producto. El análisis heurístico
  llamado `verify:formal` no se cuenta como model checking.
- Metro arrancó en `localhost:8187`; ningún dispositivo se conectó. No había simulador
  arrancado ni dispositivo adb conectado. No hay evidencia VoiceOver/TalkBack ni
  hardware físico. Disco local con ~2,3 GiB libres al comprobarlo: evitar builds
  nativos grandes sin capacidad suficiente.

## Verificación formal real

Se ejecutaron herramientas fijadas existentes, no solo el audit de texto:

```sh
JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
bash scripts/verify-event-operations-formal.sh

TDF_SOCIAL_JAVA=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
TLA_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
TDF_SOCIAL_RESULTS=/private/tmp/tdf-ux-social-models bash scripts/social/check-models.sh
```

Ambos terminaron con código 0. Eventos: ocho configuraciones TLC positivas, tres
controles negativos que detectaron exactamente la propiedad esperada; Alloy una
situación SAT y ocho comprobaciones UNSAT. Social: cuatro modelos positivos y doce
controles negativos esperados. Logs del runner en evidence; los logs detallados
sociales están en `/private/tmp/tdf-ux-social-models`.

Los límites exactos constan en los `.cfg` y las relaciones Alloy de
`formal/event-operations` y `formal/social`, junto a sus documentos de supuestos.
Se modelan estados finitos, operaciones atómicas y fairness explícita cuando se
exige progreso. No se demuestra red exactamente-una-vez, concurrencia ilimitada,
proveedores reales, accesibilidad ni experiencia subjetiva. **Falta cerrar la
trazabilidad/conformidad de autenticación, onboarding, recuperación y todos los
handlers auditados.** El mero éxito de modelos existentes no verifica el producto
completo ni el despliegue de esos modelos.

## Producción y distribución

Fly autenticado tras intervención del usuario. Estado observado: dos máquinas
`tdf-hq`, imagen `sha256:a480d2cd98ab4475277bbc2362e8f0ca4202b044bc80f23d4fdc8269c36d55ba`,
SOURCE_COMMIT `954e995f1cd08f363afe2b0e34ccf02dcd80ea8c`; fallos de arranque por
cuatro capacidades RSVP. SQL de solo lectura confirmó las filas anunciada/en
venta/en vivo/pospuesta. No se alteraron datos, flags, secretos ni máquinas.

El usuario confirmó **otro despliegue en curso**. La intervención de producción
queda bloqueada hasta su finalización o traspaso. No se debe competir por rollout,
rollback ni leases. Staging real `tdf-hq-studio-audit-staging` existe, pero su última
comprobación muestra JSON `starting` aunque Fly la marca passing: falta validación
actual de servicio y cuentas sintéticas controladas. La app errónea
`tdf-studio-audit-staging` no existe; no se creó infraestructura nueva.

EAS autenticado y proyecto `218aca4d-c096-4892-a353-c1dd7df23448` consultable. Últimos
artefactos observados incluyen iOS interno 1.0.1(17), ID
`5c01ac50-e944-4b8e-80dc-32180115d170`, fuente `43185bb4…`; Android STORE 1.0.1(8), ID
`d785bc2f-f5ce-4193-a4e0-0cb22711a49c`, fuente `7f0a42b6…`. Son históricos; **no prueban
publicación ni contienen este parche**. Submit Android está configurado en
`internal/draft`; iOS usa ASC `6779786470`. No se confunde TestFlight, build STORE,
subida o revisión pendiente con disponibilidad pública.

## Recuperación y siguiente acción exacta

Los cambios de storage y consentimiento no requieren migración; se revierten por
commit o artifact previo compatible. El cambio readiness debe conservarse al
reparar disponibilidad; revertirlo restaura la falsa salud. No revertir la base de
datos ni deshabilitar capacidades para adaptar un ejecutable antiguo sin un plan
revisado. Antes de cualquier backend rollout: snapshot, preflight, imagen inmutable,
lease, canary, smoke fijado por máquina, rolling y observación. Preservar flags
vigentes mediante el canal existente; los informes históricos no autorizan cambiarlos.

Pendientes de cierre: terminar pruebas/revisión exactas; resolver contrato de artista
antes de actualizar referencia móvil; completar cobertura y hallazgos abiertos;
confirmar entrega del despliegue concurrente; validar staging y dispositivos;
construir/subir/enviar/revisar/publicar móviles y verificar URLs/versiones. No hay
merge, despliegue o publicación de esta entrega acreditados en este checkpoint.

## Fuentes primarias consultadas el 2026-09-17

- [WCAG 2.2](https://www.w3.org/TR/WCAG22/): estados identificables, idioma,
  contraste, reflow, foco visible/no oculto; controles automáticos no certifican AA.
- [React Native accessibility](https://reactnative.dev/docs/accessibility): alertas
  y live regions para recuperación; revisar por plataforma con lector real.
- [Expo submission](https://docs.expo.dev/deploy/submit-to-app-stores/) y
  [automated submissions](https://docs.expo.dev/build/automate-submissions/):
  build, upload, TestFlight/internal, revisión y publicación son estados distintos.

## Cierre de verificación del primer incremento

Web final: 210 suites / 2.005 pruebas, cero fallos. Navegador final estable: 38 aprobadas y cuatro omisiones, incluyendo denegación de acceso/operaciones de storage en cinco perfiles, axe tras fuentes/animaciones asentadas y capturas revisadas. El primer recorrido falló antes de la corrección. Móvil PR [#89](https://github.com/diegueins680/TDF-mobile/pull/89), commit `74d784ceb366a3200c1f68ceb6082ef0ba9e2b7c`, gates hospedados validate y Datadog SUCCESS. EAS aceptó build de simulador iOS `acce0957-ec70-40a6-8f9e-8192db3b7839`; pendiente, no firmado para tienda ni publicado. Se retiraron únicamente node_modules temporales móviles de esta copia después de pruebas y subida para recuperar espacio; reinstalar con npm ci para repetir gates.


## Checkpoint de continuación

- PR web [#421](https://github.com/diegueins680/tdf-app/pull/421): checks aún en curso; el control de listas señaló la nueva pareja de sesiones sintéticas del inventario. Se clasifican como constantes técnicas de auditoría, sin cambiar permisos, umbrales ni exclusiones. El submódulo local candidato difiere del gitlink publicado: sus cuatro decisiones de catálogo nuevas y cuatro antiguas no deben confundirse con el único fallo observado en CI del padre.
- EAS iOS simulador **FINISHED**, build `acce0957-ec70-40a6-8f9e-8192db3b7839`, fuente `74d784ceb366a3200c1f68ceb6082ef0ba9e2b7c`, versión 1.0.1 (17), canal preview / INTERNAL. [Build y artefacto](https://expo.dev/accounts/cuco.saa/projects/tdf-mobile/builds/acce0957-ec70-40a6-8f9e-8192db3b7839). Es un ejecutable para simulador, no una publicación en tienda. Instalación/recorrido pendientes: el disco local volvió a 555 MB libres; se pidió liberar 10 GB sin tocar copias de trabajo.
- Dependencias móviles temporales reinstaladas con npm ci. Verificación de repositorio posterior a integrar main aprobada.
- Producción sigue bajo coordinación pendiente con el otro despliegue confirmado por el usuario. No se realizó ninguna mutación productiva de esta entrega.
- Segundo incremento en `/private/tmp/tdf-ux-locale-20260917`, rama `fix/auth-locale-20260917`: traducciones de acceso/registro/recuperación y navegación pública, todavía en implementación y sin evidencia de cierre.

### Ampliación de UX-260917-007

El caso anterior de navegador usaba catálogos vacíos: no cubría la aplicación de preferencias después de recibir idiomas/monedas. El componente real se reprodujo con catálogos sintéticos no vacíos y dos fallos `SecurityError` (getter / escritura); tras proteger la caché opcional, ambas pruebas y las dos de normalización pasan. `import.meta.env` se lee opcionalmente para ejecutar el componente fuera de Vite. Los primeros intentos del harness fallaron por un mock incompleto y por env ausente; esos errores no se cuentan como reproducciones del defecto de storage. Comandos: `npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/contexts/LocalePreferencesContext.storage.test.tsx src/contexts/LocalePreferencesContext.test.ts`. Se añadió el escenario de catálogo no vacío al recorrido de navegador: 10/10 aprobados en los cinco perfiles, con axe y conservación del formulario.

El usuario volvió a confirmar que el despliegue paralelo sigue en curso: continuar sólo trabajo independiente. El artefacto iOS se descargó (49 MB, SHA-256 `e61c0ff2b70bf453405b12324f09d4db2f5a0828bbb258fb8a77e1a220a95b5e`), pero todavía no está instalado/ejecutado. El usuario informó limpieza de disco; la relectura local fluctúa entre 404 MB y 1,6 GB libres, no los 10 GB solicitados.
