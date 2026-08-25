# Guía para Stewart: auditoría funcional y de experiencia del manejo del estudio

## Antes de comenzar

Esta tarea busca mejorar TDF App con observaciones honestas, claras y comprobables. No se mide por cuántos errores encuentres. Se mide por la cobertura, el cuidado, la evidencia y la utilidad de lo que registres.

Una función puede cumplir técnicamente y aun así ser confusa, lenta o difícil de encontrar. Una idea también es valiosa aunque no exista un error. No encontrar una función es un resultado importante. También es válido no encontrar errores cuando la prueba se hizo con atención.

## 1. Objetivo de la tarea

Probar de principio a fin las funciones que permiten operar el estudio de grabación: clientes, reservas, salas, recursos, órdenes, sesiones, participantes, paquetes, cotizaciones, facturas, pagos de prueba, inventario, equipos, mantenimiento, reportes, Live Sessions y dependencias necesarias.

Por cada caso debes registrar un resultado. Por cada hallazgo no relacionado debes crear un reporte separado dentro de TDF App.

## 2. Qué debes y qué no debes probar

Debes probar únicamente los casos visibles en el plan, en el entorno de **staging**, con las cuentas y los datos ficticios indicados.

No debes:

- Usar producción para crear, editar o borrar datos.
- Usar nombres, correos, teléfonos, tarjetas o archivos de personas reales.
- Cobrar, reembolsar o autorizar dinero real.
- Enviar emails, WhatsApp u otras comunicaciones a destinatarios reales.
- Cambiar permisos por tu cuenta.
- Publicar contenido, distribuir música o modificar inventario operativo.
- Intentar reparar el sistema o cambiar datos para ocultar un fallo.
- Compartir contraseñas, tokens, datos personales o secretos en una captura.

## 3. Cómo ingresar al sistema

1. Abre la dirección de staging entregada por Diego.
2. Comprueba que la pantalla indique **staging**. Si dice producción o no puedes reconocer el entorno, detente.
3. Inicia sesión sólo con la cuenta de pruebas indicada.
4. Verifica el nombre y el rol antes de ejecutar cada caso.
5. Nunca copies la contraseña en un reporte o captura.

## 4. Cómo abrir Prácticas

1. Abre el menú principal.
2. Busca **Panel de pasantes** o **Prácticas**.
3. Si no aparece, intenta el buscador de funciones.
4. Si tampoco la encuentras, no pruebes URLs al azar: registra el bloqueo o solicita el permiso necesario.

## 5. Cómo encontrar el proyecto y la tarea

1. En Prácticas, abre el proyecto **Auditoría funcional y de experiencia del manejo del estudio**.
2. Abre la asignación principal.
3. Pulsa **Abrir plan de pruebas**.
4. Comprueba que el plan diga staging, dos semanas, 20–30 horas y que esté asignado a tu identidad.
5. Si ves el nombre de otra persona, detente y avisa a Diego.

## 6. Cómo registrar entrada y salida

1. Antes de trabajar, abre Prácticas y pulsa **Registrar entrada**.
2. En la nota escribe brevemente lo que planeas probar.
3. Al terminar, pulsa **Registrar salida**.
4. Comprueba el tiempo registrado.
5. No dejes una entrada abierta al terminar la jornada.

## 7. Cómo acceder a staging

Usa únicamente el enlace aprobado. Antes de modificar algo, revisa:

- El entorno dice staging.
- Los proveedores dicen sandbox o mock.
- Los correos terminan en `@persona.test` o `@invalid.example`.
- Los registros de la auditoría muestran `AUDIT-2026`.

Si cualquiera de estas señales falta, detente antes de guardar.

## 8. Cómo usar las cuentas y datos ficticios

Cada caso indica el rol y los datos que debes usar. No sustituyas esos datos por información real.

- Cliente: `CUST-STUDIO-001`
- Artista: `ART-STUDIO-001`
- Ingeniero: `ENG-STUDIO-001`
- Sala: `ROOM-AUDIT-A` o `ROOM-AUDIT-B`
- Recurso: `RES-MIC-001`
- Reserva, orden, sesión o pago: siempre con prefijo `AUDIT-2026`

No cambies la identidad de una cuenta. Para probar otro rol, usa únicamente la persona de staging autorizada para ese caso.

## 9. Cómo seguir la lista de casos

1. Filtra por módulo o resultado.
2. Abre un caso y lee objetivo, propósito, condiciones previas y datos.
3. Sigue los pasos en orden.
4. Compara lo visible con el resultado esperado.
5. Recarga para comprobar qué quedó guardado.
6. Revisa notificaciones o efectos esperados.
7. Limpia sólo los datos que creaste con el prefijo de auditoría.

Estados disponibles: **Pendiente, En progreso, Aprobado, Fallido, Bloqueado, No aplicable, Listo para retest y Verificado**.

## 10. Cómo registrar un caso aprobado

Usa **Aprobado** cuando el resultado visible, el estado guardado y los efectos coincidan con lo esperado.

Escribe una confirmación breve, por ejemplo: “Se creó una sola reserva; conservó cliente, sala y horario después de recargar. El buzón de pruebas recibió una confirmación”. En casos críticos añade una captura, video externo seguro o referencia verificable.

## 11. Cómo registrar un caso fallido

1. Marca **Fallido**.
2. Describe exactamente qué ocurrió.
3. Indica qué estado quedó guardado y qué efectos observaste.
4. Guarda evidencia suficiente.
5. Crea un reporte vinculado desde el mismo caso.

Un caso fallido sin reporte vinculado impide terminar la tarea.

## 12. Cómo marcar un bloqueo

Usa **Bloqueado** cuando no puedes continuar por permiso, entorno, credencial, proveedor, dato o riesgo de seguridad. Explica qué falta, qué intentaste y quién puede desbloquearlo. No inventes un resultado.

## 13. Cómo solicitar permisos

1. Abre Prácticas.
2. Busca **Solicitudes de permiso**.
3. Elige la categoría necesaria.
4. Explica qué caso vas a ejecutar y por qué necesitas el acceso.
5. Pide el periodo mínimo necesario.
6. Espera la aprobación de Manager/Admin.

No uses la cuenta de otra persona para evitar este proceso. Un pasante nunca puede aprobar su propia solicitud ni darse roles.

## 14. Cómo crear un reporte

1. Desde un caso, pulsa **Crear reporte vinculado**, o abre **Reportes de pruebas**.
2. Elige el tipo correcto.
3. Escribe un título específico.
4. Completa módulo, función, entorno, pantalla, plataforma, dispositivo, navegador, idioma y rol.
5. Si es un error, incluye pasos, resultado esperado, resultado real y frecuencia.
6. Propón la gravedad con honestidad.
7. Guarda el borrador, revisa posibles duplicados y luego envíalo.

No mezcles problemas no relacionados. Crea un reporte por hallazgo.

## 15. Cómo adjuntar evidencia

- Captura la pantalla completa necesaria para entender el contexto.
- Oculta o recorta secretos, tokens y datos personales.
- Usa PNG, JPG, WebP, PDF o texto pequeño permitido.
- Los videos pesados se guardan externamente y se enlazan con HTTPS.
- Escribe una leyenda que explique qué demuestra la evidencia.
- No cambies el nombre de un archivo para intentar superar una validación.

## 16. Cómo distinguir los tipos de reporte

- **Error:** el sistema hace algo distinto de lo esperado, pierde datos, permite algo indebido o impide un flujo válido.
- **Sugerencia:** el flujo funciona, pero podría ser más claro, rápido, consistente o cómodo.
- **Idea:** una capacidad nueva que podría aportar valor.
- **Pregunta:** necesitas entender una regla, palabra o decisión del producto.
- **Accesibilidad:** barrera de teclado, foco, lector, contraste, escala, movimiento o comprensión.
- **Permisos:** acceso ausente, excesivo o diferente entre interfaz y API.
- **Rendimiento:** espera excesiva, bloqueo, repetición o respuesta lenta.
- **Contenido o traducción:** etiqueta, ayuda, mensaje o idioma incorrecto/inconsistente.

## 17. Cómo comprobar si ya fue reportado

Antes de enviar, revisa las sugerencias de posibles duplicados y busca por título, módulo y función. Si el mismo problema ya existe, añade evidencia o comentario al reporte permitido. No borres tu borrador hasta entender si realmente es el mismo hallazgo.

El equipo puede marcar un reporte como duplicado; seguirá enlazado al reporte principal y conservará su historial.

## 18. Cómo responder cuando pidan más información

1. Abre **Mis reportes**.
2. Filtra **Necesita información**.
3. Lee la pregunta completa.
4. Responde con datos concretos y añade evidencia si ayuda.
5. No repitas pruebas riesgosas sin autorización.

Tu respuesta devuelve el reporte a la cola de revisión sin borrar la conversación anterior.

## 19. Cómo hacer retesting

1. Espera el estado **Listo para retest**.
2. Usa el mismo caso, entorno y datos, salvo que el equipo indique otro cambio.
3. Crea una nueva ejecución; no edites la ejecución original.
4. Registra **Aprobado, Fallido o Bloqueado** y explica qué comprobaste.
5. Añade evidencia nueva y vincúlala al reporte.

## 20. Cómo actualizar el avance

El avance se calcula automáticamente a partir de la ejecución más reciente de cada caso aplicable. No escribas un porcentaje para hacerlo coincidir. Si el número parece incorrecto, crea un reporte; no cambies resultados sólo para aumentar el avance.

## 21. Cómo escribir el resumen de cada jornada

Después de registrar salida, completa:

- Fecha
- Tiempo trabajado
- Módulos probados
- Casos completados
- Reportes creados
- Bloqueos
- Próximo paso

Ejemplo: “21/08/2026 · 2 h 15 min · Calendario y salas · 11 casos · 2 reportes · Bloqueado pago PayPal por credencial sandbox · Mañana: conflictos y retest de reserva”.

## 22. Cómo preparar el informe final

La app genera totales con los datos estructurados. Tú debes añadir conclusiones:

- Módulos y funciones probados
- Casos aprobados, fallidos, bloqueados y no aplicables
- Reportes por tipo y gravedad
- Errores y problemas de uso más importantes
- Mejores ideas
- Funciones que no encontraste o entendiste
- Diferencias entre móvil y web
- Observaciones de accesibilidad
- Riesgos que quedan
- Tus tres recomendaciones de mayor prioridad

## 23. Acciones prohibidas

Está prohibido probar con producción, personas o pagos reales; evadir permisos; compartir secretos; publicar; distribuir música; alterar reservas, sesiones, permisos o inventario real; hacer pruebas destructivas o de carga; y modificar datos para que una prueba parezca aprobada.

## 24. Cuándo detenerte y contactar a Diego

Detente inmediatamente si:

- Ves producción o datos personales reales.
- Puede existir un cobro, reembolso o comunicación real.
- Aparece CAPTCHA, advertencia de seguridad o restricción de cuenta.
- Falta una credencial o verificación de proveedor.
- La prueba requiere borrar datos, generar alta carga o ampliar mucho tus permisos.
- No puedes confirmar qué cuenta, entorno o persona estás usando.

Marca el caso **Bloqueado**, conserva evidencia segura y contacta a Diego.

## Ejemplo completo de error bien escrito

**Título:** La segunda reserva puede confirmar la misma sala y horario

**Tipo:** Error

**Módulo / función:** Calendario / Conflicto de sala

**Entorno:** Staging

**Pantalla:** `/estudio/calendario`

**Rol:** Reception de pruebas

**Caso:** `STU-SCH-010`

**Qué intentaba hacer:** Comprobar que dos recepcionistas no puedan confirmar la misma sala a la misma hora.

**Pasos:**

1. Abrí `ROOM-AUDIT-A` el 27/08/2026 de 15:00 a 17:00 en dos perfiles de Chrome.
2. En el primero confirmé la reserva `AUDIT-2026-CONFLICT-A`.
3. Sin recargar el segundo, confirmé `AUDIT-2026-CONFLICT-B`.
4. Recargué el calendario.

**Esperaba:** La segunda confirmación debía mostrar un conflicto y no crear otra reserva.

**Ocurrió:** Las dos reservas quedaron confirmadas y aparecen después de recargar.

**Frecuencia:** Ocurrió 2 de 2 veces en staging.

**Gravedad propuesta:** Crítica, porque puede causar una reserva doble. La decisión final corresponde al equipo.

**Evidencia:** Captura del calendario y video externo de 28 segundos sin datos personales.

## Ejemplo de sugerencia útil

**Título:** Mostrar el saldo de horas antes de confirmar una reserva con paquete

El flujo funciona, pero el saldo aparece sólo después de guardar. Mostrar “8 h disponibles; esta reserva usará 2 h” antes de confirmar reduciría dudas y cancelaciones. Lo observé con `PKG-AUDIT-10H` en el caso `STU-PAY-002`.

## Ejemplo de idea de producto

**Título:** Lista de preparación automática para el ingeniero

Cuando una sesión se confirma, la app podría generar una lista con sala, lista de entradas, micrófonos reservados, participantes y archivos pendientes. Ayudaría a preparar la sesión y evitar olvidos. Es una idea; no significa que la función actual esté dañada.

## Ejemplo de caso bloqueado

**Caso:** `STU-PAY-008`

**Estado:** Bloqueado

**Motivo:** PayPal muestra una cuenta real y no aparece la palabra sandbox. No hice clic en aprobar. Necesito confirmación del entorno o una credencial sandbox de Diego.

**Evidencia:** Captura recortada sin correo ni token.

## Ejemplo de retest aprobado

**Reporte:** “La segunda reserva puede confirmar la misma sala y horario”

**Estado anterior:** Listo para retest

**Resultado:** Aprobado

**Comprobación:** Repetí `STU-SCH-010` dos veces. La primera reserva se confirmó; la segunda recibió “La sala ya no está disponible” y no creó otra fila. Recargué y consulté desde el segundo perfil.

**Evidencia:** Captura posterior y referencia `AUDIT-2026-RETEST-SCH-010`.

**Acción:** Registré una nueva ejecución; no edité el fallo original.
