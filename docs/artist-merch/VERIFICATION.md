# Evidencia y bloqueos

## Estado de capacidades

| Capacidad | Resultado |
|---|---|
| Leer/modificar repositorio | Disponible; trabajo en worktree aislado |
| Rama | `feat/artist-merch-storefronts`; inició en `269f3784121b8bdca378ec516d5cb446cc818e39` y se integró sobre `849444cdcce0254c3091293edce8a4cc5f8175fa` |
| Backend Haskell | Build/test local disponible; resultado final se registra abajo |
| PostgreSQL aislado | Disponible; PostgreSQL 16 temporal verificado. Docker Desktop no arrancó en la última corrida, por lo que el mismo fixture se ejecutó contra un clúster local desechable |
| Migraciones | Aplicación, reejecución, rollback guardado/limpio y reapply disponibles |
| OpenAPI/clientes | Generador disponible; web y móvil regenerados |
| Web | Typecheck/Jest/build y Playwright disponibles; Chromium desktop/teléfono verificado con datos sintéticos |
| Móvil | Typecheck/Jest disponibles; build debug y runtime Android API 36.1 verificados con API sintética local; Xcode 16.2 disponible, pero `CoreSimulatorService` no permitió descubrir simuladores iOS; iOS/dispositivo físico pendientes |
| Pagos sandbox | Credenciales Datafast/PayPal/manual no disponibles; no se llamaron proveedores |
| GitHub | Push disponible; draft PR raíz `#274` y draft PR móvil `#49` creados, sin merge ni despliegue |

## Ejecutado con resultado verificable

- `./scripts/test-artist-merch-storefronts-migration.sh`: PASS. Cubre re-run, perfil reclamado, aislamiento de propietario/permisos, snapshots inmutables, rechazo de falso paid, evidencia verificada, fulfillment independiente, concurrencia sin sobreventa, expiración, consumo, 10%/0%, privacidad analítica, rollback bloqueado con comercio, rollback limpio y reapply.
- `./scripts/test-artist-merch-runtime.sh`: PASS, 1/1 sobre PostgreSQL 16 temporal mediante su modo de base externa vacía y desechable; también queda conectado como paso obligatorio de `backend-quality`. El escenario ejecuta primero los handlers críticos y luego la aplicación Servant real por HTTP. Cubre autenticación bearer, solicitud y aprobación administrativa de una banda piloto con override de 0%, storefront/producto público, carrito y checkout invitado, cálculo server-side, recuperación idempotente después de convertir el carrito, rechazo de payload conflictivo, capability de orden no enumerable, falso retorno de navegador, cero intentos de pago, liberación exacta de stock, estados independientes, redacción financiera, aislamiento entre vendedores, incidencia operativa y colas seller/admin. Docker Desktop no pudo iniciar localmente, pero el wrapper conserva ese modo autónomo además del modo externo verificado.
- Web `tsc --noEmit -p tdf-hq-ui/tsconfig.app.json`: PASS.
- Móvil `tsc --noEmit -p tdf-mobile/tsconfig.json`: PASS.
- Backend `stack test --fast`: PASS, 2.487/2.487 ejemplos sobre la integración final con `main` (incluye siete reglas unitarias de merch).
- Build web de producción: PASS; presupuesto inicial de JavaScript PASS (413.750 bytes gzip).
- Reglas Haskell focalizadas `stack test --fast --test-arguments=--match=merch`: PASS, 8/8 (siete de merch y una coincidencia preexistente de storefront).
- Jest web `tdf-hq-ui/src/api/merch.test.ts`: PASS, 7/7.
- Jest móvil `tdf-mobile/__tests__/merchDeepLinks.test.ts`: PASS, 2/2.
- Android nativo: `app:assembleDebug` PASS (481 tareas; 60 ejecutadas, 421 `UP-TO-DATE`) para `x86_64`; APK debug instalado y lanzado en `Medium_Phone_API_36.1`. Con API sintética local y sin credenciales se verificaron deep link público, descubrimiento, ficha de producto, aviso de piloto y botón de compra accesible con `enabled=false`. Logcat no mostró excepciones ni muerte del proceso TDF. Se obtuvieron capturas locales reales. Xcode 16.2 está instalado, pero `xcrun simctl list devices available` falló porque `CoreSimulatorService` perdió la conexión y `simdiskimaged` no estaba disponible; no se ejecutó runtime iOS ni dispositivo físico.
- Playwright `artist-merch.spec.mjs` en Chromium desktop y Pixel 7: PASS, 4/4. Cubre piloto cerrado honesto, descubrimiento, storefront, producto, checkout deshabilitado y capturas adjuntas al reporte.
- Axe en esos recorridos: PASS, 0 violaciones `serious` o `critical`.
- Regresión móvil completa: 319/320 en la corrida simultánea; el único timeout preexistente (`TicketCheckout`) pasó aislado 15/15. Se conserva la advertencia `act(...)` preexistente.
- Regresión web completa: FAIL en la suite preexistente `CourseRegistrationsAdminPage.test.tsx` durante una corrida con alta contención; las pruebas específicas de merch y E2E pasaron. Debe repetirse en CI o en aislamiento después del rebase.
- OpenAPI: YAML parseado y tipos generados para ambos clientes.
- Manifiesto/mecanismo de release: PASS, 47/47 pruebas; migración anclada al SHA de introducción.
- Feature registry: generación PASS. Auditoría reporta solo el destino preexistente no relacionado `/reputation/consents`.
- Auditoría de listas/catálogos: PASS, 1.004/1.004 candidatos con decisión vigente; prueba determinista PASS. Se conservaron las decisiones revisadas preexistentes, se retiraron 2 huellas obsoletas y se clasificaron 54 candidatos nuevos.

## Pendiente antes de recomendar lanzamiento

- Repetir los recorridos HTTP autenticados ya verificados localmente con web/app/backend desplegados juntos en staging, observabilidad activa y dos personas adultas con roles separados.
- Runtime iOS/dispositivo físico, lector de pantalla y zoom manual; Android API 36.1 y Playwright responsive/Axe web ya se verificaron con datos sintéticos.
- Adapter de pago de merch: iniciación, retorno no autoritativo, firma/replay, consulta/captura, refund y reconciliación en sandbox.
- Ejecución financiera completa de refund/dispute y evidencia final de settlement. La cancelación inmediata sin pagar y el triage/escalamiento de incidencias ya están implementados; resolver un caso no mueve dinero.
- Worker real de notificaciones opt-in y observabilidad/alertas de staging.
- Validaciones legal, tributaria, privacidad y operación.

## Criterios de aceptación

El esquema/pruebas cubren 1–5 y 10–20 a nivel de dominio/datos en distinta profundidad; UI/contrato cubren recorridos 6–9 y 14–23. La superficie pública se verificó en runtime web y Android con APIs sintéticas, y comprador invitado, vendedor y administrador atravesaron la API HTTP real contra PostgreSQL 16. No se procesó ningún pago: el punto 13 se verificó manteniendo la orden pendiente ante un retorno falso y constatando cero intentos de pago. Staging, adapters sandbox e iOS/dispositivo físico siguen pendientes, por lo que el 24 solo puede declararse parcial. El 25 se cumple manteniendo checkout, pagos y publicación pública en flags `false` fuera del fixture aislado y usando mensajes de piloto.

Por lo tanto, este incremento es candidato a revisión y staging, no a producción ni a afirmar que la venta ya está disponible.
