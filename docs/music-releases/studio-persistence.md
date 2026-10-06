# Persistencia segura del editor de lanzamientos

Fecha: 2026-09-15. Continuación local; no despliegue ni certificación de todo
el flujo editorial.

## Auditoría y decisión

La página Studio ya crea versiones y usa los endpoints canónicos. Su
autoguardado ejecuta dos transacciones: metadatos y contenido. El contador
`updatedAt` de la primera se conserva aunque la segunda falle. Sin embargo,
el método absorbía el error y sus consumidores continuaban: podían registrar
términos, solicitar una transición o iniciar una carga sobre el estado anterior.
También era posible seguir modificando el grafo mientras una respuesta lo
sustituía, o iniciar una operación durante un guardado pendiente.

Se conserva la API y su control optimista, sin fusionar automáticamente
identificadores, créditos o derechos. El guardado devuelve confirmación
explícita; un fallo o una petición pendiente impiden la acción dependiente.
Durante guardado/carga/validación/transición, los campos quedan bloqueados
mediante controles deshabilitados y un `fieldset`. El estado de guardado
se anuncia con `role=status`. Los errores conservan el borrador en pantalla
y permiten reintentar con la última revisión confirmada. Un guard síncrono
impide operaciones repetidas antes del siguiente render.
Iniciar un guardado cancela también su temporizador de debounce pendiente:
un rechazo no provoca otra escritura automática del mismo snapshot.

La instancia del editor ahora tiene una clave por release/versión: navegar a
otra versión no reutiliza su estado ni los closures de persistencia.

Esta decisión tiene un costo de interacción explícito: no se puede editar
mientras se guarda. No se implementó una cola de edición concurrente ni una
fusión automática de grafos. Tampoco se ofrece persistencia offline o una
garantía de conservar cambios no confirmados al cerrar la pestaña.

Se consultaron las referencias oficiales de React sobre
[refs para estado de operaciones](https://react.dev/learn/referencing-values-with-refs)
y [limpieza y ciclo de efectos](https://react.dev/reference/react/useEffect).
Se usan APIs existentes en React 18 del repositorio, sin nuevas dependencias.

## Prueba conectada nueva

`PW-MUSIC-REAL-03`, en la suite dedicada existente, entra con el artista
sintético, crea un single desde Studio y modifica su pista. El servidor real
rechaza el contenido sin declaración de autoridad después de guardar los
metadatos; la prueba exige que no haya transición ni aceptación de términos.
Completa las dos declaraciones desde la interfaz, espera el autoguardado,
lee el grafo persistido y recarga la página. Comprueba que la URL pública del
borrador devuelve 404.

No sustituye respuestas ni introduce escrituras alternativas desde el test.
Las lecturas de verificación usan la sesión real del navegador. Reutiliza el
aislamiento API/PostgreSQL/MinIO/worker descrito en
[la integración existente](real-browser-integration.md).
El borrador no contiene audio ni portada: no presentar este caso como carga,
revisión humana, aprobación o publicación desde la interfaz.

```sh
npm test --workspace=tdf-hq-ui -- --runTestsByPath \
  src/pages/MusicReleaseStudioPage.persistence.test.tsx \
  src/pages/MusicReleaseStudioPage.test.ts \
  src/utils/musicPreviewRange.test.ts --watch=false --reporters=default
npm run test:music-browser-integration
npm run build --workspace=tdf-hq-ui
```

## Evidencia de esta continuación

- Preflight: 15 correctos, 3 advertencias, 0 errores; árbol sucio y main
  detrás del remoto. No se hizo pull, cambio de rama ni activación del loop.
- Regresión inicial del componente: 4 fallos / 1 acierto. Reprodujo términos,
  transición y carga ejecutados tras fallo, y campos activos al guardar.
- La siguiente corrida tuvo fallos de inicialización del fixture durante la
  espera de la consulta. Se precargó la consulta sintética en el QueryClient
  del test; no se ampliaron timeouts ni se cambió la consulta productiva.
- Repetición dirigida: 5/5, código 0. Después se añadió un sexto caso que
  exige validación exactamente una vez tras ambas confirmaciones.
- Dos corridas conjuntas posteriores quedaron en 7/11 y 10/11 por tiempos
  agotados (espera inicial y presupuesto de 5 s). Se redujeron búsquedas
  globales del DOM y se esperaron promesas dentro de `act`. El presupuesto
  de esta suite de componente se fijó explícitamente en 15 s por caso,
  siguiendo otros formularios grandes del repositorio. No es una prueba de
  latencia; no se ampliaron debounce, API, worker ni navegador.
- La siguiente corrida quedó en 10/11 y reveló una tercera escritura en el
  caso de reintento: el temporizador anterior seguía vivo después del guardado
  manual. Se corrigió su cancelación, sin relajar la aserción de exactamente
  dos escrituras. Repetición final conjunta: **11/11**, tres suites, código 0,
  18,205 s. Incluye los seis casos de persistencia, dos del grafo inicial y
  tres de rango de preview.
- TypeScript y ESLint dirigidos: código 0. El primer comando ESLint usó una
  ruta de binario inexistente; la repetición usó el binario raíz instalado.
- Configuración e intermediario de navegador: **4/4 Node**, código 0.
- Build anterior a la cancelación del temporizador: código 0, Vite 12460
  módulos / 4m41s, 376937 bytes gzip iniciales / 5 preloads. No se atribuye
  ese build al último ajuste. La repetición sobre el runtime final pasó:
  **código 0**, TypeScript/Vite 12460 módulos / 1m18s, 376938 bytes gzip
  iniciales / 5 preloads. Persiste el aviso de chunks mayores de 500 kB.
- Primera matriz ampliada: **13/15**, código 1, sin skips ni reintentos.
  Informe `/private/tmp/tdf-music-browser-results-BnKxNU/results.json`,
  SHA-256 `23fa45202cd84a576cceb7284bccaa8626e52bfc2bc7a2667f8dcc17d5c0cf16`.
  Inicio UTC `2026-09-15T16:49:34.985Z`, duración navegador 343568,018 ms.
  Fallaron la espera de retorno del login en Firefox (caso nuevo) y la
  apertura de biblioteca en WebKit (caso anterior). Los informes de ambos
  muestran login/API con HTTP 200, sin respuestas HTTP >=400. El snapshot
  de Firefox ya muestra el release; WebKit conserva la página anterior.
  No se atribuye una causa de runtime comprobada a esos tiempos agotados.
  No se alcanzaron los escenarios posteriores de compra/corrección/retiro.
- Se reutiliza ahora el mismo helper de login (espera POST 200) y se espera
  explícitamente la URL de destino después de login y al abrir biblioteca,
  conforme a la [guía de navegación de Playwright](https://playwright.dev/docs/navigations#waiting-for-navigation).
  Estas fases consumen el presupuesto existente de 90 s del caso; después
  se conservan las aserciones de UI de 8 s. No se repite el clic, sustituye
  la navegación ni se amplían los límites globales.
- Repetición completa: **15/15 navegador, 18/18 API/HTTPS/S3 y 8/8 preflight
  Linux**, código 0. Informe
  `/private/tmp/tdf-music-browser-results-aG0Rta/results.json`, SHA-256
  `bad598c0550a6ddeabb5084651bbada568168e0c9767c6aa0998fb23105ca0a2`.
  Inicio UTC `2026-09-15T17:04:26.354Z`; duración navegador 394387,29 ms;
  cero skips, fallos, flaky o reintentos. Incluye los escenarios posteriores
  de descarga del máster intacto, compra sintética, reembolso/revocación,
  corrección y retiro programado idempotente. La limpieza terminó y las
  consultas Docker no encontraron contenedores/redes propios; puerto 4187
  sin listener. Informes conservados.
- La revisión de red encontró **403 en `/catalog/genres/items` y
  `/catalog/countries/items`** en los cinco casos de Studio, antes y después
  de recargar. No son los rechazos esperados 400 por autoridad incompleta y
  404 de privacidad. La inspección posterior los localizó en las consultas
  `Catalogs.listItems` de RadioWidget (radio del shell). Studio utiliza
  `/catalogs/genres/items`, la ruta pública: no atribuir estos 403 a su
  selector. Queda pendiente corregir las consultas de radio; el test nuevo
  no selecciona género. Los cinco casos de biblioteca no registraron HTTP >=400; los
  cinco de visitante solo el 404 esperado del máster privado.

Huellas finales de fuente (SHA-256):

```text
MusicReleaseStudioPage.tsx                  ca76777ef561769dcba015e6c1b8ef6525c5d044289c044a90bb652093d665ee
MusicReleaseStudioPage.persistence.test.tsx 71e131b56818ba186b6670b6eeb21e219fbd773d08010985ee4b0e526de99086
music-player-integration.spec.mjs           01ab833c00417bccdcff0c15cdd57e01364b263dcd663a5a424da9c0f661643a
```

Se revisaron código, resultados y snapshots textuales automáticos de errores.
No se realizó interacción manual con la UI ni prueba en dispositivos físicos.

## Despliegue y límites

No hay migraciones nuevas, cambios de backend, dependencias ni imagen worker.
El runtime modificado es `MusicReleaseStudioPage.tsx`; requiere desplegar el
frontend cuando corresponda. Rollback: revertir únicamente esta modificación,
manteniendo las entregas previas y el trabajo ajeno. No se tocó producción y
no hay datos productivos que revertir.

Siguen pendientes el recorrido editorial completo por navegador, cargas por
UI con cancelación/reanudación, dispositivos físicos y lector de pantalla
humano, proveedores/CDN/pagos externos y las puertas DDEX documentadas.

La inspección también encontró límites anteriores que esta entrega **no
resuelve**: `upsertParty` no actualiza nombre/legalName de una parte existente,
y `contentJson` solo devuelve partes referenciadas por créditos o splits.
Se requiere resolver la edición versionada y la conservación de colaboradores
sin referencias antes de afirmar que todo el editor conserva el grafo completo.

La continuación de [pertenencias de colaboradores](version-party-membership.md)
implementa la conservación y copia a correcciones; la edición de nombres e
identificadores por versión permanece pendiente. Esta evidencia anterior no
se atribuye automáticamente a ese cambio posterior de backend.

La entrega posterior de [datos de partes](versioned-party-details.md) implementa
también esa edición con snapshots de aprobación v2 y evidencia propia. Mantener
separados estos resultados históricos de las pruebas del backend posterior.

Siguiente corte por dependencias: primero conservar y versionar todas las
partes del borrador; después conectar al harness del navegador los archivos
sintéticos y el supervisor Linux ya existentes, y cubrir carga/procesamiento
y actualización de estado en Studio. Solo entonces ampliar a términos,
solicitud de cambios, reenvío, aprobación y programación desde la interfaz.
No arrancar un worker contra una base o un bucket remoto para estos tests.
