# Especificación funcional, datos y UX

## Actores y recorridos

1. El artista elige uno de sus perfiles publicados, reclamados o verificados; solicita una tienda y ve un estado de revisión, nunca un falso éxito de activación.
2. Staff revisa identidad, políticas y operación; aprueba, rechaza o suspende. Durante el piloto puede registrar un override de comisión de 0%.
3. El propietario invita colaboradores mediante `PartySelector`; nadie escribe un Party ID. Cada miembro recibe solo catálogo, stock, órdenes, fulfillment, finanzas o configuración.
4. El manager crea un borrador, variantes y stock, carga una imagen con alt, activa políticas y envía a revisión. Staff publica o rechaza con motivo.
5. El fan llega desde el perfil, marketplace o URL canónica; filtra tiendas, elige variante y usa un carrito exclusivo del artista.
6. El invitado proporciona destinatario y entrega. El servidor vuelve a calcular todo, bloquea variante, comprueba límite por comprador y crea una orden pendiente con reserva.
7. Un adapter verificado —no el retorno del navegador— debe confirmar el pago. Hasta entonces la UI dice “pendiente”.
8. El vendedor prepara, marca retiro listo o registra transportista/tracking. El comprador consulta mediante ID UUID más token privado no incluido en la URL.
9. Antes de que inicie pago o preparación, el comprador puede cancelar idempotentemente y liberar su reserva. Después, abre una incidencia; el vendedor resuelve solo casos operativos y escala cancelaciones pagadas, refunds, disputas o fraude a staff.
10. Staff prepara una liquidación manual; otra persona debe aprobarla. Solo entonces una persona distinta del preparador puede registrar referencia, fecha y comprobante privado. Ese registro concilia la liquidación y las órdenes vinculadas, pero nunca inicia ni afirma una transferencia.

## Modelo de datos

`merch_store` pertenece a un `directory_profile` y a un vendedor `party`. `merch_store_member` delimita permisos. `merch_store_policy` conserva versiones y `merch_shipping_zone` limita el MVP a EC/USD.

`merch_product` contiene el ciclo editorial. `merch_product_variant` contiene SKU, precio, peso y contadores `stock_on_hand`, `stock_reserved`, `stock_sold`. `merch_product_image` conserva object keys durables, checksum, dimensiones, variantes y estados de scan/moderación.

`merch_cart` y `merch_cart_item` nunca mezclan vendedores. `merch_order` referencia el checkout canónico y guarda snapshots de destinatario, zona, políticas y comisión; `merch_order_line` guarda snapshots de producto/variante/precio. Reservas, fulfillment, shipment, issues, review, settlement, evidencia privada de pago, outbox, analítica y auditoría permanecen separados.

## Estados formales

Producto:

```text
draft -> pending_review -> published -> sold_out
  |            |              |            |
  +-> archived +-> rejected   +-> paused <-+
                  |                |
                  +-> draft        +-> published | archived
```

Fulfillment:

```text
pending -> preparing -> ready_for_pickup -> delivered
                     \-> shipped -> delivered -> return_requested -> returned
```

`problem` puede interrumpir la operación y volver solo a una transición admitida; `cancelled` y `returned` son terminales para fulfillment. Checkout, intento, evidencia de pago, orden comercial, inventario, fulfillment, refund, disputa y settlement tienen columnas/filas distintas.

Incidencias:

```text
open -> seller_review -> awaiting_buyer -> seller_review
  |          |                 |
  +----------+-----------------+-> staff_review -> resolved | rejected | cancelled
             +-------------------> resolved | rejected   (solo casos operativos)
```

Los estados terminales no se reabren. Cancelación, refund, disputa y fraude nunca pueden cerrarse desde el rol vendedor. El estado de la incidencia es evidencia de soporte y no modifica por implicación pago, refund, disputa, fulfillment ni settlement.

## Invariantes

- Tienda activa implica solicitud aprobada y fecha de activación.
- Propietario implica membresía aceptada con todos los permisos.
- SKU es único dentro de la tienda, no global.
- Producto publicado requiere revisión staff; la solicitud de revisión requiere variante, imagen decodificada y política activa.
- Un cart y todos sus items pertenecen al mismo vendedor.
- Precio, impuestos, envío, comisión y totales los determina PostgreSQL/backend; el cliente no puede fijarlos.
- `stock_reserved + stock_sold <= stock_on_hand` para stock finito; las reservas se toman bajo locks y expiran.
- Una `Idempotency-Key` solo puede reproducir el mismo fingerprint.
- `paid` exige un intento exitoso con evidencia server-side verificada y binding al checkout.
- Retorno del navegador, fulfillment y entrega no cambian por sí solos el pago.
- Snapshots de líneas y condiciones no se editan. Las políticas nuevas solo afectan órdenes futuras.
- La comisión base es `(subtotal - descuento) × bps / 10000`; 1000 bps por defecto, impuestos/envío/fee del procesador excluidos.
- Una liquidación solo admite órdenes pagadas, entregadas o devueltas y aún no vinculadas. Preparar las marca `under_review`; aprobar exige otra identidad y registrar el comprobante exige estado `approved` y una identidad distinta del preparador.
- La evidencia de liquidación es append-only, reencodada, privada, identificada por checksum y referencia única. Su endpoint no descarga el archivo ni mueve fondos.
- El token privado se almacena hasheado y se exige junto al UUID de carrito/orden.
- Auditoría y timeline son append-only.

## Contrato API

El contrato fuente está en `tdf-hq/docs/openapi/api.yaml`; los tipos generados no se editan manualmente.

- Público: capabilities, storefronts, producto, carrito, checkout/orden pendiente, tracking, cancelación sin pagar e incidencias.
- Autenticado vendedor: solicitud, tienda, miembros, políticas, zonas, catálogo, imágenes, órdenes, fulfillment y triage de incidencias operativas.
- Staff: revisión de tiendas/productos, cola de incidencias y settlements.

Errores significativos usan 400 para input, 403 para alcance, 404 para evitar enumeración, 409 para carrera/idempotencia/estado y 503 para capability no habilitada.

## Decisiones UX/UI

- Mobile-first, controles de al menos 44 px, landmarks/headers, labels accesibles, contraste claro y estados loading/empty/error.
- Español e inglés en las nuevas superficies; el estado del backend nunca se traduce en un éxito prematuro.
- El storefront vive en el ecosistema del perfil y enlaza a seguimiento/contacto/colaboración existentes.
- En móvil se implementan descubrimiento, producto, carrito, tracking y fulfillment esencial. Solicitud, edición de catálogo, permisos y revisión staff quedan en web responsive con handoff a `/merchSeller`.
- No se muestra ningún proveedor si capability combina flags, runtime listo y credenciales.
- Un cambio de stock o precio devuelve 409 y obliga a refrescar; una capability vencida explica que el carrito/orden no está disponible en ese dispositivo.
