# Manual de vendedor, soporte y rollout

## Manual breve para vendedores

1. En `Mi tienda`, elige un perfil de artista/banda que administras y envía la solicitud. “En revisión” no significa “activa”.
2. Tras la aprobación, crea políticas de envío/devolución y una zona: retiro coordinado o envío nacional.
3. Crea el producto como borrador. Usa un SKU distinto por variante, precio en centavos USD, peso real y stock disponible.
4. Sube JPEG/PNG con texto alternativo. El archivo se reencoda y queda pendiente de moderación.
5. Envía a revisión. Solo staff puede publicar; luego puedes pausar o archivar.
6. Invita al manager por nombre, nombre artístico o username y entrega únicamente los permisos necesarios.
7. En pedidos, filtra por estado de entrega y revisa los totales del filtro. Los importes de comisión/neto solo aparecen con permiso `finance`. La exportación CSV reproduce el filtro, omite datos personales y neutraliza fórmulas de hoja de cálculo.
8. Prepara solo cuando el pago figure `paid`. Para envío nacional registra transportista y tracking; para retiro usa “listo para retirar”.
9. En `Solicitudes e incidencias`, responde al comprador y resuelve únicamente problemas operativos. Escala a TDF cancelaciones pagadas, reembolsos, disputas y fraude; cerrar un caso nunca significa que el dinero ya se movió.
10. No copies ni compartas más datos del destinatario que los necesarios para despachar. El CSV operativo deliberadamente no incluye nombre, correo, teléfono ni dirección del comprador.

## Soporte y conciliación

- Primero identificar entorno, store/order UUID interno y correlation ID; nunca solicitar tarjeta, contraseña o token privado.
- `pending` no es pagado. Revisar checkout, intento, evidencia y provider event antes de cualquier corrección.
- Ante stock cambiado: pedir refrescar; no ajustar contadores manualmente. Ejecutar expirador de reservas y conciliar.
- Ante envío: conservar tracking, timeline y comunicación pública; notas internas nunca se muestran al comprador.
- Cancelación sin pagar: el comprador puede cancelarla solo antes de procesamiento de pago/preparación; el backend libera la reserva y audita el cambio de forma idempotente.
- Reembolso/disputa: mantener estado financiero separado de la incidencia, cancelación, devolución, fulfillment y settlement. El vendedor escala; staff solo cierra el caso después de verificar evidencia y ejecutar el flujo financiero independiente correspondiente.
- Settlement: filtrar una tienda y seleccionar únicamente órdenes que la consola muestre como elegibles y que hayan sido creadas dentro del período contable elegido (`inicio` inclusivo, `fin` exclusivo). Preparar agrupa snapshots financieros y deja las órdenes `under_review`; no mueve fondos.
- Aprobación: debe realizarla otra persona autorizada después de cotejar órdenes, comisión, ajustes y neto. Un `hold` exige motivo, conserva las órdenes vinculadas en espera y no altera pagos ni fulfillment; un revisor independiente puede resolverlo y aprobar después.
- Registro de pago: después de ejecutar y verificar la transferencia fuera de TDF, una persona distinta del preparador carga un JPEG/PNG, fecha, referencia externa única y notas opcionales. La API reencoda el archivo, guarda checksum y metadatos append-only, y cambia liquidación/órdenes a `paid` idempotentemente. Este paso documenta evidencia; nunca inicia un payout.
- Evidencia: en staging/producción es obligatorio configurar `MERCH_SETTLEMENT_EVIDENCE_DIR` sobre un volumen privado, durable, cifrado, respaldado y legible solo por el servicio/operadores autorizados. No usar `/assets/serve`, enlaces públicos ni nombres suministrados por usuarios. El MVP no expone descarga HTTP; el acceso excepcional se hace por el procedimiento auditado del almacenamiento.
- Conciliación: contrastar referencia, fecha, monto y beneficiario con el extracto autorizado; investigar discrepancias sin editar la evidencia. Cualquier corrección requiere un evento/ajuste hacia adelante, nunca mutar la fila o el archivo original.
- Incidente grave: apagar el flag más específico; para cualquier pago apagar primero `merch.checkout.runtime_ready` y `merch.checkout`.

## Observabilidad mínima antes del piloto

Alertas: errores 5xx/409 anómalos, reserva expirada atrasada, stock negativo (debe ser imposible), intento pagado sin transición, webhook fallido/replay, outbox fallido, órdenes pagadas sin avance, reembolso/chargeback, settlement sin doble control y subida rechazada.

Dashboard de producto con consentimiento: solicitudes/activaciones, tiempo a primer producto, publicados, storefront/product view, add-to-cart, checkout start/complete/abandon, conversión, gross/net, recompra, agotados, refund/disputa y conexión originada desde tienda. Nunca enviar PII, token o datos de pago.

## Rollout propuesto y criterios de salida

1. Local con datos sintéticos: migración/re-run/rollback, dominio, contratos y clientes verdes.
2. Staging: runtime web/móvil, responsive/WCAG, cross-tenant, outbox, observabilidad y adapters sandbox.
3. Prueba interna con dos adultos y roles separados: vendedor/comprador/staff, sin dinero real.
4. Piloto cerrado: Cementerio de Elefantes y otras bandas invitadas, inicialmente con override 0% cuando sea aprobado.
5. Validación de soporte, fulfillment, conciliación, contabilidad y políticas.
6. Activación gradual por flag/tienda/proveedor y monitoreo reforzado.
7. Disponibilidad general solo con todos los criterios de aceptación y asuntos legales cerrados.
No activar publicación pública sin catálogo/políticas moderados. No activar checkout si faltan creación/captura, webhook o verificación independiente, refund y conciliación. No confundir un preview o check automático con producción verificada.

## Rollback

- Funcional: apagar el flag específico; conservar/provider y luego `merch.storefronts` si hace falta. Las órdenes existentes siguen accesibles para soporte.
- Aplicación: volver al release anterior conservando las tablas y workers necesarios para órdenes existentes.
- Base: el rollback SQL funciona solo antes de datos comerciales. Se niega expresamente si hay órdenes, intentos vinculados o settlements; después de eso se corrige hacia adelante.
- Archivos: no borrar objetos referenciados por órdenes/auditoría. Una eliminación de catálogo es soft-delete y la limpieza física requiere job retenido/auditado.

## Checklist legal, tributario y operativo (bloqueante)

Validación profesional requerida, sin que este borrador constituya asesoría legal:

- Quién es vendedor formal frente al comprador y qué muestra el checkout.
- Facturación del producto por el artista y facturación de comisión por TDF.
- IVA, retenciones, comprobantes, contracargos y tratamiento del costo del procesador.
- Acuerdo de vendedor, KYC/KYB aplicable y autoridad de managers.
- Consumidor: información previa, plazos, cancelación, garantía, devolución y soporte.
- Privacidad: base legal, minimización, retención, encargados y derechos del titular.
- Propiedad intelectual, falsificaciones y licencias de imágenes/marcas.
- Productos prohibidos y proceso de retiro/suspensión/apelación.
- Política de refunds/disputas y quién absorbe cada ajuste.
- Cuenta bancaria, segregación de fondos, conciliación y evidencia de settlement.
