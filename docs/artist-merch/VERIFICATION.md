# Evidencia y bloqueos

## Estado de capacidades

| Capacidad | Resultado |
|---|---|
| Leer/modificar repositorio | Disponible; trabajo en worktree aislado |
| Rama | `feat/artist-merch-storefronts`; inició en `269f3784121b8bdca378ec516d5cb446cc818e39` y se integró sobre `e2086e25bca856a413433a28f8ae83219b05ed38` |
| Backend Haskell | Build/test local disponible; resultado final se registra abajo |
| PostgreSQL aislado | Disponible; PostgreSQL 16 temporal en Docker verificado en la última corrida; el wrapper también admite una URL de base externa vacía y desechable |
| Migraciones | Aplicación, reejecución, rollback guardado/limpio y reapply disponibles |
| OpenAPI/clientes | Generador disponible; web y móvil regenerados |
| Web | Typecheck/Jest/build y Playwright disponibles; Chromium desktop/teléfono verificado con datos sintéticos |
| Móvil | Typecheck/Jest disponibles; build/runtime Android API 36.1 y build Release/runtime iOS 18.3 Simulator verificados con API sintética local; dispositivo físico pendiente |
| Pagos sandbox | Credenciales Datafast/PayPal/manual no disponibles; no se llamaron proveedores |
| GitHub | Push disponible; draft PR raíz `#274` y draft PR móvil `#49` creados, sin merge ni despliegue |

## Ejecutado con resultado verificable

- `./scripts/test-artist-merch-storefronts-migration.sh`: PASS. Cubre re-run, perfil reclamado, aislamiento de propietario/permisos, snapshots inmutables, rechazo de falso paid, evidencia verificada, fulfillment independiente, concurrencia sin sobreventa, expiración canónica de checkout con liberación de stock, consumo, 10%/0%, privacidad analítica, dos refunds parciales canónicos distribuidos entre líneas, doble control, transición confirmada con evidencia, reverso acumulado exacto de comisión, disputa read-only, settlement ajustado, evidencia privada de liquidación append-only, rollback bloqueado con comercio, rollback limpio y reapply.
- `./scripts/test-artist-merch-runtime.sh`: PASS, 1/1 sobre PostgreSQL 16 temporal en Docker; también queda conectado como paso obligatorio de `backend-quality`. El escenario ejecuta primero los handlers críticos y luego la aplicación Servant real por HTTP. Cubre autenticación bearer, solicitud y aprobación administrativa de una banda piloto con override de 0%, storefront/producto público, carrito y checkout invitado, cálculo server-side, recuperación idempotente después de convertir el carrito, rechazo de payload conflictivo, zona/provincia incompatible, capability no enumerable sin PII del destinatario, falso retorno de navegador, cero intentos de pago, worker de expiración idempotente con checkout/pago/reserva/stock alineados, hash/tamaño del JPEG realmente persistido, bloqueo de edición durante revisión, estados independientes, redacción financiera, aislamiento entre vendedores, rechazo 400 de filtros de fulfillment inválidos, incidencia operativa, colas seller/admin y liquidación manual completa. El incremento financiero añade creación/replay/conflicto de refund, asignación exacta, aprobación independiente sin ejecución, cancelación pre-ejecución, autorización admin negativa y proyección read-only de una disputa sintética sin mutar pago/settlement.
- Web `tsc --noEmit -p tdf-hq-ui/tsconfig.app.json`: PASS.
- Móvil `tsc --noEmit -p tdf-mobile/tsconfig.json`: PASS.
- Backend `stack test --fast`: PASS, 2.489/2.489 ejemplos sobre la integración final con `main` (incluye ocho reglas unitarias de merch).
- Build web de producción: PASS; presupuesto inicial de JavaScript PASS (416.059 bytes gzip).
- Reglas Haskell focalizadas `stack test --fast --test-arguments='--match=merch'`: PASS, 9/9 (ocho de merch y una coincidencia preexistente de storefront).
- Jest web focalizado `MerchAdminPage.test.tsx` + API merch + exportación CSV: PASS, 15/15. Comprueba consola de liquidación en espera/aprobada, refund autorizado pero no ejecutado, contrato idempotente del cliente, disputa read-only, Axe sin impactos serios/críticos, evidencia sin solicitud de transferencia, exportación sin PII, columnas financieras condicionadas y neutralización de fórmulas CSV.
- Jest móvil `tdf-mobile/__tests__/merchDeepLinks.test.ts`: PASS, 2/2.
- Android nativo: `app:assembleDebug` PASS (481 tareas; 60 ejecutadas, 421 `UP-TO-DATE`) para `x86_64`; APK debug instalado y lanzado en `Medium_Phone_API_36.1`. Con API sintética local y sin credenciales se verificaron deep link público, descubrimiento, ficha de producto, aviso de piloto y botón de compra accesible con `enabled=false`. Logcat no mostró excepciones ni muerte del proceso TDF. Se obtuvieron capturas locales reales.
- iOS nativo: Xcode 16.2 + iOS 18.3 Simulator, `iPhone 16-Detox2` (`x86_64`). `pod install` sincronizó el lockfile nativo con las dependencias Expo ya declaradas; `pod install --deployment` pasó después de la sincronización. El build Release con `CODE_SIGNING_ALLOWED=NO`, `ARCHS=x86_64` y API local terminó PASS. El `.app` se instaló y lanzó, y los deep links de descubrimiento, storefront y producto renderizaron contra `scripts/fixtures/artist-merch-mobile.mjs`. La ficha mostró el aviso “Purchases remain disabled during the pilot” y CTA `Add to cart` deshabilitado. El fixture registró solo GET y rechazaba cualquier otro método con 405; `log show` no encontró mensajes `error` o `fault` del proceso TDF. Expo Updates se apagó solo dentro del artefacto de prueba para fijar el bundle local. Capturas reales: [directorio](./media/ios-discovery.png), [tienda](./media/ios-storefront.png) y [producto](./media/ios-product-checkout-disabled.png). Dispositivo físico no ejecutado.
- Playwright `artist-merch.spec.mjs` en Chromium desktop y Pixel 7: PASS, 4/4. Cubre piloto cerrado honesto, descubrimiento, storefront, producto, checkout deshabilitado y capturas adjuntas al reporte.
- Axe en esos recorridos: PASS, 0 violaciones `serious` o `critical`.
- Regresión móvil completa: 319/320 en la corrida simultánea; el único timeout preexistente (`TicketCheckout`) pasó aislado 15/15. Se conserva la advertencia `act(...)` preexistente.
- Regresión web completa: FAIL en la suite preexistente `CourseRegistrationsAdminPage.test.tsx` durante una corrida con alta contención; las pruebas específicas de merch y E2E pasaron. Debe repetirse en CI o en aislamiento después del rebase.
- OpenAPI: YAML parseado y tipos web/móvil regenerados; ambos archivos generados tienen el mismo SHA-256 `aacf791db50990c47d4cd69e796808a22ab0a31179566479cbac757ed3aa654b`.
- Manifiesto/mecanismo de release: PASS, 59/59 pruebas; migración anclada al SHA de introducción.
- Feature registry: generación PASS. Auditoría reporta solo el destino preexistente no relacionado `/reputation/consents`.
- Auditoría de listas/catálogos: PASS, 1.011/1.011 candidatos con decisión vigente. Los nuevos enums/listas de refund se clasificaron como límites financieros cerrados: PostgreSQL/canonical ledger es autoridad, Haskell falla cerrado y OpenAPI es consumidor generado.

## Pendiente antes de recomendar lanzamiento

- Repetir los recorridos HTTP autenticados ya verificados localmente con web/app/backend desplegados juntos en staging, observabilidad activa y dos personas adultas con roles separados.
- Dispositivo físico, VoiceOver/lector de pantalla, Dynamic Type y zoom manual; Android API 36.1, iOS 18.3 Simulator y Playwright responsive/Axe web ya se verificaron con datos sintéticos.
- Adapter de pago de merch: iniciación, retorno no autoritativo, firma/replay, consulta/captura, refund y reconciliación en sandbox.
- Adapter de ejecución de refund y de ingestión de dispute, ambos con firma/replay/consulta/conciliación en sandbox. Solicitud, asignación, saldo reservado, aprobación dual, cancelación pre-ejecución y lectura de evidencia canónica ya están implementados/verificados localmente; `approved` no mueve dinero.
- Ejercicio humano de la liquidación contra extracto real autorizado en staging. El flujo técnico de evidencia final ya está implementado y verificado solo con datos/archivos sintéticos; registrar evidencia no mueve dinero.
- Worker real de notificaciones opt-in y observabilidad/alertas de staging. El worker de reservas ya está conectado al arranque y probado localmente; falta observarlo desplegado.
- Validaciones legal, tributaria, privacidad y operación.

## Criterios de aceptación

El esquema/pruebas cubren 1–5 y 10–20 a nivel de dominio/datos en distinta profundidad; UI/contrato cubren recorridos 6–9 y 14–23. La superficie pública se verificó en runtime web, Android e iOS Simulator con APIs sintéticas, y comprador invitado, vendedor y administrador atravesaron la API HTTP real contra PostgreSQL 16. No se procesó ningún pago ni refund: el punto 13 se verificó manteniendo la orden pendiente ante un retorno falso y constatando cero intentos de pago; el refund se detuvo en `approved` y luego se canceló. Staging, adapters sandbox y dispositivo físico siguen pendientes, por lo que el 24 solo puede declararse parcial. El 25 se cumple manteniendo checkout, pagos, refunds, disputas y publicación pública en flags `false` fuera del fixture aislado y usando mensajes de piloto.

Por lo tanto, este incremento es candidato a revisión y staging, no a producción ni a afirmar que la venta ya está disponible.
