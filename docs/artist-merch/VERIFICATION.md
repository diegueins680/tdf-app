# Evidencia y bloqueos

## Estado de capacidades

| Capacidad | Resultado |
|---|---|
| Leer/modificar repositorio | Disponible; trabajo en worktree aislado |
| Rama | `feat/artist-merch-storefronts`; inició en `269f3784121b8bdca378ec516d5cb446cc818e39` y se integró sobre `714060ffcc1db56448f6d3cb82e055e0085af4e7` |
| Backend Haskell | Build/test local disponible; resultado final se registra abajo |
| PostgreSQL aislado | Disponible; bases temporales creadas por el script |
| Migraciones | Aplicación, reejecución, rollback guardado/limpio y reapply disponibles |
| OpenAPI/clientes | Generador disponible; web y móvil regenerados |
| Web | Typecheck/Jest/build y Playwright disponibles; Chromium desktop/teléfono verificado con datos sintéticos |
| Móvil | Typecheck/Jest disponibles; runtime nativo/emulador pendiente |
| Pagos sandbox | Credenciales Datafast/PayPal/manual no disponibles; no se llamaron proveedores |
| GitHub | Push disponible; draft PR raíz `#274` y draft PR móvil `#49` creados, sin merge ni despliegue |

## Ejecutado con resultado verificable

- `./scripts/test-artist-merch-storefronts-migration.sh`: PASS. Cubre re-run, perfil reclamado, aislamiento de propietario/permisos, snapshots inmutables, rechazo de falso paid, evidencia verificada, fulfillment independiente, concurrencia sin sobreventa, expiración, consumo, 10%/0%, privacidad analítica, rollback bloqueado con comercio, rollback limpio y reapply.
- Web `tsc --noEmit -p tdf-hq-ui/tsconfig.app.json`: PASS.
- Móvil `tsc --noEmit -p tdf-mobile/tsconfig.json`: PASS.
- Backend `stack test --fast`: PASS, 2.474/2.474 ejemplos (incluye cinco reglas unitarias de merch).
- Build web de producción: PASS; presupuesto inicial de JavaScript PASS (413.750 bytes gzip).
- Jest web `tdf-hq-ui/src/api/merch.test.ts`: PASS, 5/5.
- Jest móvil `tdf-mobile/__tests__/merchDeepLinks.test.ts`: PASS, 2/2.
- Playwright `artist-merch.spec.mjs` en Chromium desktop y Pixel 7: PASS, 4/4. Cubre piloto cerrado honesto, descubrimiento, storefront, producto, checkout deshabilitado y capturas adjuntas al reporte.
- Axe en esos recorridos: PASS, 0 violaciones `serious` o `critical`.
- Regresión móvil completa: 319/320 en la corrida simultánea; el único timeout preexistente (`TicketCheckout`) pasó aislado 15/15. Se conserva la advertencia `act(...)` preexistente.
- Regresión web completa: FAIL en la suite preexistente `CourseRegistrationsAdminPage.test.tsx` durante una corrida con alta contención; las pruebas específicas de merch y E2E pasaron. Debe repetirse en CI o en aislamiento después del rebase.
- OpenAPI: YAML parseado y tipos generados para ambos clientes.
- Manifiesto/mecanismo de release: PASS, 47/47 pruebas; migración anclada al SHA de introducción.
- Feature registry: generación PASS. Auditoría reporta solo el destino preexistente no relacionado `/reputation/consents`.
- Auditoría de listas/catálogos: PASS, 1.000/1.000 candidatos con decisión vigente; prueba determinista PASS. Se conservaron las decisiones revisadas preexistentes, se retiraron 2 huellas obsoletas y se clasificaron 50 candidatos nuevos.

## Pendiente antes de recomendar lanzamiento

- Tests de handlers HTTP y autorización con dos vendedores.
- Servir web/app/backend juntos y completar recorridos comprador/vendedor/admin contra PostgreSQL por HTTP.
- Runtime nativo, lector de pantalla y zoom manual; Playwright responsive/Axe web ya se verificó con mocks sintéticos.
- Adapter de pago de merch: iniciación, retorno no autoritativo, firma/replay, consulta/captura, refund y reconciliación en sandbox.
- Handlers operativos completos para cancelación/refund/dispute y evidencia final de settlement.
- Worker real de notificaciones opt-in y observabilidad/alertas de staging.
- Validaciones legal, tributaria, privacidad y operación.

## Criterios de aceptación

El esquema/pruebas cubren 1–5 y 10–20 a nivel de dominio/datos en distinta profundidad; UI/contrato cubren recorridos 6–9 y 14–23. La superficie pública web sí fue verificada en runtime con API simulada; los recorridos HTTP reales de vendedor/admin/pago y el runtime nativo siguen pendientes. El punto 13 está protegido en base. El 24 solo puede declararse parcial. El 25 se cumple manteniendo checkout, pagos y publicación pública en flags `false` y usando mensajes de piloto.

Por lo tanto, este incremento es candidato a revisión y staging, no a producción ni a afirmar que la venta ya está disponible.
