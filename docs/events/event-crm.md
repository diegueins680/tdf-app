# Invitaciones y CRM por evento

El organizador puede invitar a su equipo y gestionar interesados del evento sin conceder acceso al CRM general de TDF. Cada invitación dura siete días, funciona una sola vez y requiere la cuenta invitada o el correo verificado. El piloto permanece en ensayo hasta la revisión de Diego.

## Generar y revocar un enlace

1. En el evento, abre **Equipo y CRM**. El entorno debe tener las migraciones aplicadas y el CRM habilitado expresamente para ese ID de evento.
2. En **Equipo e invitaciones**, introduce la cuenta o el correo confirmado de la destinataria y pulsa **Resolver cuenta TDF antes de emitir**. Si aparece una cuenta, selecciónala y confirma su identidad con Diego. Si no hay cuenta, usa el correo confirmado para el registro posterior.
3. Elige **Coordinación**, confirma la identidad y el rol y pulsa **Generar enlace privado de siete días**. El enlace se muestra una sola vez. Cópialo y entrégalo a Diego por un canal privado. El sistema no envía la invitación a la destinataria.
4. Para inutilizar un enlace pendiente, pulsa **Revocar invitación pendiente**. Una invitación aceptada se retira con **Retirar acceso activo** sobre su membresía; revocar el enlace no sustituye esa acción.

La destinataria inicia sesión o completa el registro existente. Una invitación por correo exige verificarlo mediante un código de treinta minutos enviado al correo de la cuenta. La pantalla muestra evento, fecha, sede, rol y permisos antes de aceptar. Las invitaciones pendientes dejan de funcionar si quien las emitió pierde su autoridad.

La URL usa un fragmento que no llega a los registros HTTP. Tokens y correos de destinatarios no se incluyen en los recibos de comandos ni en las exportaciones de invitaciones. Las pantallas privadas del CRM y la invitación suprimen analytics. Las pruebas usan identidades sintéticas; la evidencia del evento real se entrega por separado.

## Permisos

| Acceso | Facultades dentro del evento |
| --- | --- |
| Organizador | Administración, invitaciones y retiro de miembros. Conserva publicación, precios, cobros, confirmación manual y devoluciones mediante los controles existentes. |
| Coordinación | Embudo completo, seguimiento, importación revisada, equipo de ventas, órdenes necesarias para operar y tareas de logística. No aprueba pagos ni concede coordinación o verificación financiera. |
| Ventas | Consulta y seguimiento de contactos asignados y sus órdenes. No reasigna contactos ni consulta el embudo completo. |
| Verificador de pagos | Sólo mediante designación expresa del organizador. Revisa evidencia manual y consulta las órdenes necesarias. No recibe administración general ni facultades de devolución. |

El servidor comprueba sesión, evento y membresía actuales en cada lectura, escritura, exportación y reintento. La operación conserva el bloqueo de autorización hasta terminar. La aceptación consume la invitación y crea la membresía en una transacción.

## Contactos e importación

Antes de crear un contacto, el servidor busca el correo normalizado y el teléfono internacional. Los móviles ecuatorianos con formato local se normalizan a `+593`. Las coincidencias ambiguas requieren seleccionar explícitamente un contacto candidato; el flujo no fusiona cuentas ni concede acceso por coincidencia de correo.

El contacto canónico se vincula una vez al evento. La ficha conserva responsable, origen, fecha de contacto, notas, próxima acción, seguimiento, cantidad solicitada, orden y combinación de transporte. Las etapas son interesado, contactado, reservado, pagado y cerrado sin venta. Los cambios de etapa conservan su autor; las proyecciones de órdenes identifican al sistema canónico como origen y mantienen la auditoría financiera existente.

Para importar, elige un CSV privado, asigna columnas, revisa la vista previa y corrige o descarta las filas. Vuelve a validar las correcciones antes de confirmar. Las fechas requieren ISO con zona horaria, por ejemplo `2026-11-10T15:00:00-05:00`. El límite es 500 filas y 1 MB por archivo. Una fila que declara pago o reserva permanece contactada mientras no tenga una orden y evidencia conciliadas. Reimportar conserva contactos, vínculos y seguimiento posterior, sin generar ventas ni plazas nuevas.

Una reserva requiere una orden canónica vigente y vencimiento explícito. La lista de espera reutiliza `EventWaitlist` y añade la combinación de ida y regreso, sin reservar inventario. Ese registro necesita el correo del contacto. Una orden de invitado se relaciona con el contacto sin convertir su correo en autorización sobre una cuenta.

## Ventas y transporte

Las ventas se calculan desde órdenes y checkout, separados de los interesados y las filas importadas. Los totales se agrupan por moneda y conservan importes enteros exactos. Las devoluciones reducen ventas netas y actualizan el inventario mediante los servicios existentes.

Para efectivo o transferencia, selecciona una orden con reserva vigente y registra referencia, monto y fecha del pago. La transferencia debe haber sido seleccionada en el checkout existente. La evidencia queda pendiente hasta la revisión del organizador o su verificador designado. Se mantiene el control existente que impide verificar la propia evidencia. La confirmación, el pago canónico y la emisión de entradas se realizan juntos; un reintento no duplica la venta.

Las combinaciones con transporte permanecen **desactivadas**. El checkout todavía no tiene integración verificada para reservar entrada, ida y regreso conjuntamente. El servidor rechaza esas órdenes y un control de base de datos protege también las rutas anteriores. La disponibilidad vendible es cero mientras esté este bloqueo. Las pruebas de dos intentos simultáneos verifican el rechazo; no demuestran un checkout conjunto operativo. Confirmar capacidades de vehículos no activa ventas por sí solo.

## Inventario y lista de revisión

| Registro existente | Fuente de verdad y revisión necesaria |
| --- | --- |
| Evento y organizador | ID, cuenta organizadora, fecha, zona y sede confirmados por Diego; el nombre no concede autoridad. |
| Contactos | Identidad canónica, coincidencias ambiguas resueltas y responsables vigentes. |
| Reservas y compras | Una orden canónica con vencimiento; el contador existente incluye plazas retenidas o vendidas. |
| Pagos y devoluciones | Evidencia, verificador independiente y recibos existentes. El embudo no añade ventas al total. |
| Viajes | Ida y regreso con capacidad y referencia confirmadas por transporte. Sin cupo verificado, sin ventas. |
| Combinación | Integración conjunta pendiente: venta desactivada aunque existan capacidades; disponibilidad futura mínima entre entrada, ida y regreso. |
| Destinataria | Cuenta resuelta o correo confirmado en privado, registro y verificación; nunca adjuntar dirección o enlace a una PR. |

Antes del uso real: revisar migración y contratos, habilitar sólo el evento del entorno autorizado, verificar SMTP y registro, revisar lista real y conciliación, y probar de nuevo los permisos. La evidencia privada incluye los horarios, precios y capacidades pendientes del primer piloto. Ningún dato del piloto se carga mediante la migración de producción.

## Ensayo y revisión

La migración es aditiva y no crea eventos, cuentas, invitaciones, membresías ni ventas. `event_crm_control.enabled` comienza desactivado por evento. El rollback rechaza eliminar datos operativos o evidencia manual existente.

Las comprobaciones incluyen PostgreSQL con conexiones concurrentes, HTTP con la autenticación y el checkout reales, pagos manuales, reintentos, devolución, vencimiento, exportación y recorridos de escritorio y móvil. El modelo ejecutable acotado comprueba invariantes y detecta ocho variantes inseguras; no constituye una prueba universal ni una demostración de correspondencia entre el modelo y toda la implementación.

```bash
node --test scripts/__tests__/event-crm-model.test.mjs
TDF_EVENT_CRM_TEST_PORT=5486 python3 scripts/test-event-crm.py
TDF_EVENT_CRM_TEST_PORT=5486 python3 scripts/test-event-crm-http.py
```

Los ejecutores crean y eliminan únicamente sus propias bases desechables. La segunda prueba requiere la biblioteca compilada con Stack y las dependencias web y Chromium instaladas. Usa PostgreSQL aislado, sin credenciales de producción. La evidencia privada contiene el inventario y la lista de pendientes del piloto; la PR no autoriza merge, despliegue, publicación, ventas ni emisión del enlace real.

El contrato compartido actualiza únicamente los tipos generados de Mobile en la [PR vinculada #184](https://github.com/diegueins680/TDF-mobile/pull/184). La validación de este flujo es web móvil; no introduce pantallas nativas ni constituye una nueva release de la app.
