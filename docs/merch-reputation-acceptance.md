# Matriz de aceptación: reputación de merch

Fecha de auditoría: 2026-09-09. Código de implementación evaluado: `d847accab07c294c8e43c1255cd2025e41bedf34`; head vigente del PR base integrado y revalidado: `7e3379e0dfbcb25a2fb658a12959eb15c3f3f0ce` (incluye la implementación merch `159cc2a659c47025b5a10cae39d315de8455b707` y `main` `97254b9b918852254a230281150f5d5d8d9d76bb`); cliente móvil: `77dc27a29d2f2765b827c2c6d61905ca190a4ee3`. Esta matriz no certifica producción: registra evidencia sintética local/CI y separa los controles implementados de las validaciones que necesitan staging, revisión humana o un piloto autorizado.

## Estados de evidencia

- **Automatizado:** existe una aserción ejecutada sobre datos sintéticos o un gate de CI reproducible.
- **Implementado:** contrato, autorización, constraint o flujo existe y fue inspeccionado, pero su resultado operativo todavía necesita validación externa.
- **Pendiente externo:** no se puede demostrar honestamente sin entorno, participantes, credenciales o aprobación ajenos a este cambio.

## Resultado de ingeniería

| # | Criterio | Estado | Implementación y evidencia |
| ---: | --- | --- | --- |
| 1 | Sólo un comprador con una orden elegible puede evaluar. | Automatizado | `merch_review_evidence_is_eligible` deriva comprador, pago y fulfillment de vistas canónicas; `merch_reputation_submit_review` vuelve a comprobarlo en servidor. La prueba de migración verifica reclamo invitado idempotente, cuenta cruzada, preentrega y cancelación en `scripts/test-merch-reputation-migration.sh` (líneas 255–316 y 466–477). |
| 2 | No puede evaluar su propia tienda. | Automatizado | La elegibilidad bloquea propietario, membresías activas, artista titular y miembros conocidos de la banda, sin fingerprinting. La prueba rechaza explícitamente propietario, administrador y miembro de banda (líneas 478–506). |
| 3 | Puede evaluar una vez la tienda y una vez cada línea elegible. | Automatizado | Índices únicos parciales protegen tienda/orden y producto/línea, incluso con concurrencia; producto, línea, orden y tienda se validan juntos. Las aserciones finales comprueban exactamente una de cada clase (líneas 772–779). |
| 4 | Producto y tienda reciben agregados separados. | Automatizado | `merch_reputation_aggregate` y `merch_reputation_dimension_aggregate` usan sujetos tipados `store`/`product`; el rebuild se ejecuta por sujeto. La prueba publica ambos de forma independiente y compara sus proyecciones (líneas 418–420, 445–463 y 583–597). |
| 5 | La reputación comercial no altera la artística o profesional. | Automatizado + implementado | No hay FK, evento, fórmula ni escritura desde merch hacia los agregados contextuales existentes. El API marca tienda con `commercialReputation=true`; el componente web prueba el rótulo comercial y el producto usa otro rótulo en `tdf-hq-ui/src/components/merch/MerchReputationSummary.test.tsx`. |
| 6 | Las ediciones conservan historial. | Automatizado | Cada edición agrega `merch_review_revision` bajo revisión esperada; un trigger impide actualizar o borrar revisiones. La prueba confirma dos revisiones inmutables y una sola evaluación lógica (líneas 318–337 y 413–424). |
| 7 | Una devolución o reembolso no borra evidencia automáticamente. | Automatizado | Reembolso, devolución y disputa permanecen separados de la visibilidad de la evaluación. La prueba reembolsa, registra devolución, edita dentro de la ventana y confirma que evaluación/revisiones siguen presentes (líneas 396–430); fraude confirmado excluye influencia sin destruir evidencia (líneas 440–463). |
| 8 | Los retrasos demostrables del courier no se atribuyen al vendedor. | Automatizado | Las señales guardan `responsibility`; sólo `seller` entra al promedio operativo. SQL prueba que un retraso `courier` deja `operational_average` sin penalización (líneas 538–549); Haskell contrasta courier y vendedor en `tdf-hq/test/Spec.hs` (líneas 824–836 y 873–881). |
| 9 | El valor de compra no aumenta el peso de la evaluación. | Automatizado | La fórmula no acepta precio, subtotal ni cantidad. La proyección persiste `purchaseValueWeighted=false`; SQL lo afirma (líneas 598–601) y las pruebas puras comprueban invariancia por orden de ratings (líneas 819–822 y 883–887 de `Spec.hs`). |
| 10 | Con menos de cinco órdenes evaluables se muestra “Tienda nueva”. | Automatizado | La fórmula versionada exige cinco; un agregado no publicado conserva `public_rating=NULL`. SQL comprueba `new_store` bajo umbral y exclusión de órdenes relacionadas (líneas 623–632); Haskell cubre 4/5 órdenes y ausencia de reviews (líneas 803–808). Web y móvil prueban que no aparece un número artificial. |
| 11 | Cada agregado identifica la versión de fórmula. | Automatizado | `formula_version_id` es obligatorio y forma parte de la PK del agregado y de dimensiones/insignias. SQL exige `merch-commercial-bayes-v1` en la proyección publicada (líneas 583–601). |
| 12 | Los agregados se reconstruyen desde eventos originales. | Automatizado | Evaluaciones, revisiones, fuentes y señales son durables; checkpoints y agregados son derivados. La prueba invoca `merch_reputation_rebuild_aggregate` para tienda/producto, cambia integridad de orden y reconstruye sin perder evidencia (líneas 440–463); el procesador se reejecuta sin checkpoints pendientes (líneas 580–604). |
| 13 | Las señales automáticas provienen de estados confiables del servidor. | Automatizado | Triggers transaccionales capturan orden, fulfillment, shipment e incidencias canónicas en `merch_reputation_source_event`; el worker proyecta idempotentemente. SQL verifica captura/replay (líneas 551–573), rechaza `source_authority=frontend` y el reuso conflictivo de evidencia (líneas 636–655). |
| 14 | Una opinión negativa no produce automáticamente una sanción financiera. | Implementado | No existe transición de evaluación a riesgo o finanzas. La política `merch-risk-v1-draft` declara `automaticFinancialPenaltyFromRating=false`; los únicos tipos de caso son fraude, apropiación, tracking falso, incumplimiento reiterado o abuso. El dominio comercial mantiene fulfillment/pago separados y las reglas Haskell reservan decisiones financieras a staff (`Spec.hs`, líneas 915–937). |
| 15 | Las medidas materiales cuentan con evidencia, auditoría y apelación. | Automatizado + implementado | Política, caso y medida guardan versión, actor, motivo y evidencia; las retenciones requieren otro revisor humano. SQL rechaza una retención sin revisión independiente (líneas 750–768). Moderación y apelación registran auditoría y restauración; el flujo completo está probado en líneas 657–738. La política punitiva continúa en `draft`, por lo que no hay umbral material activo. |
| 16 | El vendedor puede responder sin modificar la evaluación. | Automatizado | Respuesta y revisiones del vendedor viven en tablas separadas; la autorización exige membresía de esa tienda. El flujo de moderación preserva las revisiones de la evaluación y la prueba rechaza una respuesta de otra tienda (líneas 737–746). |
| 17 | El comprador puede reportar contenido y conocer el resultado. | Automatizado + implementado | API/cliente permiten reportar evaluación o respuesta con motivo/evidencia autorizada, y apelar. El caso soporta solicitud de evidencia, ocultamiento provisional, decisión y reversión; las notificaciones son opt-in y de payload seguro. SQL cubre reporte→solicitud→ocultamiento→decisión→apelación→restauración (líneas 657–738). La entrega real de notificaciones queda pendiente de staging. |
| 18 | No se exponen órdenes ni datos personales públicamente. | Automatizado + implementado | `MerchReviewPublic` no contiene orden, email, teléfono, dirección, pago, tracking ni despacho. `publicReviewsSql` construye una lista positiva de campos y respeta la preferencia de nombre/avatar; el claim marca `orderId` como `writeOnly`. SQL verifica redacción del token de capacidad (líneas 290–299) y el outbox rechaza claves sensibles. Falta un recorrido autenticado con servicios de staging para verificar la serialización completa. |
| 19 | La reputación influye de forma limitada y auditable en descubrimiento. | Automatizado | `merch_reputation_search_contribution` exige ambiente y flag, usa la versión activa y limita el aporte a 12 % del score base. SQL prueba fail-closed, aporte positivo acotado y neutralidad de tienda nueva (líneas 606–617); Haskell prueba el cap (líneas 857–868). |
| 20 | Las tiendas nuevas conservan oportunidades reales de exposición. | Implementado; pendiente externo | El ranking base combina relevancia, disponibilidad, categoría y novedad/exploración antes de reputación; tiendas nuevas/recientes reciben exploración acotada y contribución reputacional neutral. `merch_reputation_exposure_daily` guarda impresiones/conversiones y contribuciones agregadas. La oportunidad efectiva y la concentración sólo pueden demostrarse durante un piloto autorizado con dashboards activos. |
| 21 | Backend, web, móvil, OpenAPI, migraciones y clientes permanecen coherentes. | Automatizado | Generación OpenAPI deja clientes web/móvil sin diff; typecheck, pruebas focalizadas/completas, build web, E2E responsive, migración PostgreSQL, feature registry, catálogo y release checks pasaron. El submódulo raíz apunta a `77dc27a…`, el commit móvil probado. |
| 22 | Las pruebas relevantes pasan o sus bloqueos quedan identificados con precisión. | Automatizado + pendientes externos | La evidencia reproducible aparece abajo. Permanecen explícitamente fuera de la certificación: staging con tres identidades, runtime nativo asistido, AV/CDR y storage firmado, WCAG asistido, revisión de sesgo/copy/retención/legal, wiring de dashboards y piloto. |

## Ejecuciones reproducibles

Sobre el código de implementación `d847accab…`:

- `./scripts/test-merch-reputation-migration.sh`: PASS en PostgreSQL 16 efímero, incluyendo reejecución de migraciones, concurrencia, autorización, moderación, reconstrucción y rollback seguro.
- `stack test --fast --test-arguments='--match=merch'`: PASS, 20 ejemplos.
- Jest web focalizado: PASS, 3 suites / 12 pruebas.
- Jest móvil focalizado: PASS, 4 suites / 6 pruebas; suite móvil completa: PASS, 67 suites / 323 pruebas.
- Typecheck web y móvil: PASS; ESLint focalizado: PASS.
- Build web de producción y presupuesto de bundle: PASS; conserva únicamente el warning existente de chunk mayor a 500 KiB.
- Playwright de merch: PASS, 6/6 en desktop, teléfono y tablet, con Axe automatizado en los recorridos cubiertos.
- OpenAPI/clientes, feature registry, catálogo y verificaciones de release: PASS.
- GitHub Actions del commit exacto: [ejecución 34352848358](https://github.com/diegueins680/tdf-app/actions/runs/34352848358), 11/11 jobs exitosos. PR móvil: [ejecución 34322895540](https://github.com/diegueins680/TDF-mobile/actions/runs/34322895540), exitosa.

Todas estas pruebas usan identidades, tiendas, productos, órdenes y evaluaciones sintéticas. Los previews automáticos de los draft PR no son despliegues de producción ni validación con comercios reales.

Después de integrar la base vigente `7e3379e0…`, se repitieron los controles sensibles a la integración antes de publicar el nuevo head:

- migración PostgreSQL de reputación: PASS;
- backend focalizado: PASS, 20 ejemplos;
- Jest web: PASS, 3 suites / 12 pruebas;
- Jest móvil: PASS, 3 suites / 5 pruebas;
- typecheck web y móvil: PASS;
- generación OpenAPI: PASS, sin cambios en los clientes generados;
- auditoría canónica de catálogos: PASS, 1.051 candidatos, cero faltantes y cero decisiones obsoletas;
- pruebas de entrypoint, release y auditoría de staging: PASS, 66/66.

La ejecución alojada citada arriba corresponde al baseline funcional exacto `d847accab…`. El head que contiene la integración con `7e3379e0…` necesita su propia ejecución alojada antes de considerarse nuevamente verde.

## Gates externos pendientes

1. Fusionar primero el dominio canónico de tiendas del PR base #274.
2. Ejecutar en staging checkout invitado → vínculo privado → entrega/cancelación → evaluación, con comprador, vendedor y moderador sintéticos separados.
3. Conectar y validar almacenamiento firmado más antivirus/CDR antes de habilitar imágenes.
4. Completar recorrido nativo, lector de pantalla, teclado, zoom/texto ampliado y revisión WCAG 2.2 AA asistida.
5. Revisar copy, fórmula/sensibilidad, sesgo, retención, anonimización y bases jurídicas con producto, riesgo, soporte, cumplimiento y privacidad.
6. Conectar dashboards/alertas y medir concentración, exposición nueva, abuso, divergencia de proyección y efecto en conversión durante piloto.
7. Mantener todos los flags apagados hasta que el gate correspondiente se apruebe; no habilitar búsqueda ni disponibilidad general desde este PR.

Conclusión: los invariantes de ingeniería están listos para revisión y ensayo sintético en staging. Los criterios que dependen de comportamiento humano, infraestructura externa o efectos reales de exposición siguen abiertos y bloquean cualquier recomendación de producción.
