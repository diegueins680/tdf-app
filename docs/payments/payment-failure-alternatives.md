# Alternativas después de un problema de pago

Petición de Diego, 18 de noviembre de 2026: en cualquier flujo de pago, mostrar los demás medios posibles cuando falle un intento.

`PaymentAlternativesNotice` muestra los medios disponibles para la compra actual y excluye el medio que falló. La lista procede de la orden o de sus capacidades verificadas, con los requisitos del navegador aplicados. No habilita proveedores ni inicia pagos. Si no existe otro medio ejecutable, indica cómo pedir ayuda con la referencia de la orden. Una consulta pendiente o fallida de disponibilidad no equivale a una lista vacía verificada: se comunica que falta consultar los medios y no se presentan opciones antiguas como verificadas. Hasta recibir una respuesta válida no se inicia un nuevo intento; sigue siendo posible recuperar el intento original con su misma clave de idempotencia.

Después de un retorno de página completa, que PayPal no esté cargado todavía no prueba que sea imposible usarlo. Si la orden lo ofrece y está configurado, se muestra en una sección separada de opciones por comprobar al volver a la orden; no se anuncia como listo ni se afirma que no exista alternativa. La ausencia de configuración sí lo excluye.

Los pagos alojados comparten esta presentación en `HostedProviderCheckout`. Se conserva el bloqueo de recuperación e idempotencia: un timeout, un estado ambiguo o un fallo al consultar no autorizan otro intento. Solo la respuesta autoritativa `canRetryOrFallback` permite liberar el intento. Las pantallas de retorno consultan la orden original y comprueban que sigue correspondiendo al mismo checkout; no crean una orden de reemplazo para mostrar alternativas.

Stripe puede admitir varios medios en el mismo PaymentIntent. `StripePaymentAlternativesNotice` consulta ese intento mediante el SDK público y muestra los medios restantes únicamente cuando permite elegir de nuevo (`requires_payment_method`), excluyendo la opción seleccionada en PaymentElement. Un resultado pendiente, exitoso, incompatible o desconocido exige verificar la orden antes de pagar de nuevo. No se registra el secreto del intento.

## Cobertura

- Cursos: tarjeta/PayPal y proveedores alojados, incluidos errores dentro de sus diálogos.
- Reservas: depósito, transferencia y seguimiento privado.
- Entradas: checkout público, transferencia, proveedores alojados y formulario Stripe del flujo social. La app móvil ya remite sus entradas de pago al checkout público web; no cobra de nuevo en nativo.
- Domo: depósito de cotizaciones y sus diálogos.
- Mezcla y mastering: selección de pago, seguimiento y retorno Datafast.
- Marketplace: checkout, diálogos de proveedor, seguimiento y retorno Datafast.
- Merch: seguimiento de intentos fallidos; no se anuncian ejecutores que esa pantalla no implementa.
- Música digital: errores del único ejecutor implementado (PayPal), sin anunciar métodos que no se puedan aplicar a esa compra.
- Retorno común de proveedores: alternativas de la orden original mediante consultas de solo lectura.

Los formularios administrativos que registran movimientos ya realizados no son intentos de cobro: un error al guardar un asiento no debe inducir al cliente a pagar otra vez.

## Validación

Pruebas de componentes y de consultas de órdenes cubren rechazo verificable, métodos restantes, disponibilidad desconocida, estado pendiente, bloqueo tras timeout y separación entre compras. Las pruebas existentes de cursos, Domo, servicios, Stripe y recuperación siguen siendo parte de la comprobación. La vista local en escritorio y móvil usa datos simulados y el componente real; no sustituye una transacción real ni demuestra un despliegue.
