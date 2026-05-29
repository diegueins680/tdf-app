# Manual de Eventos y Tickets — TDF Records

**Guía de uso: crear eventos, crear tickets, vender y comprar**
Versión 1.0 | Mayo 2026

---

## 📋 Tabla de Contenidos

1. [Introducción](#introducción)
2. [Cómo llegar a Eventos sociales](#cómo-llegar-a-eventos-sociales)
3. [Crear un evento](#crear-un-evento)
4. [Crear tickets (tiers)](#crear-tickets-tiers)
5. [Guía para vender tickets (organizador)](#guía-para-vender-tickets-organizador)
6. [Guía para comprar tickets (asistente)](#guía-para-comprar-tickets-asistente)
7. [Check-in en el evento](#check-in-en-el-evento)
8. [Reembolsos y cancelaciones](#reembolsos-y-cancelaciones)
9. [Preguntas frecuentes](#preguntas-frecuentes)

> **Nota sobre el estado del módulo**
> El módulo de **Eventos sociales** está marcado como **Beta**. Las pantallas y los botones descritos aquí son los actuales; algunas funciones pueden seguir evolucionando.

---

## Introducción

El módulo **Eventos sociales** permite organizar fiestas, conciertos, festivales y showcases, y gestionar su venta de tickets de principio a fin:

- **Organizadores** crean el evento, definen los tipos de ticket (*tiers*), venden, hacen check-in y gestionan reembolsos.
- **Asistentes** navegan los eventos, compran tickets y reciben su entrada por correo.

Hay dos roles principales en este manual:

| Rol | Qué puede hacer |
|-----|-----------------|
| **Organizador** (creador del evento) | Crear el evento, crear tiers, ver todas las órdenes, hacer check-in, cancelar y reembolsar. |
| **Asistente** (cualquier usuario con sesión) | Ver el evento, comprar tickets y consultar *Mis órdenes*. |

> Las herramientas de organizador solo aparecen para quien creó el evento.

---

## Cómo llegar a Eventos sociales

1. Inicie sesión en TDF Records.
2. En el menú lateral abra la sección **Social**.
3. Haga clic en **Eventos sociales**.

La ruta directa es **`/social/eventos`**.

> Si ve el mensaje **"Inicia sesión para crear eventos."**, primero debe autenticarse.

En la parte superior verá el título **Eventos sociales** (con la etiqueta **Beta**) y debajo el **Calendario de eventos**, donde los eventos aparecen agrupados por fecha.

---

## Crear un evento

> Solo usuarios con sesión iniciada pueden crear eventos. El usuario que lo crea queda como **organizador**.

1. En la página **Eventos sociales**, abra el formulario de nuevo evento.
2. Complete los campos:

| Campo | Descripción |
|-------|-------------|
| **Título** | Nombre del evento (obligatorio). |
| **Venue** | Lugar del evento (lista desplegable; opcional, puede quedar **Sin venue**). |
| **Tipo** | `Party`, `Concert`, `Festival` o `Showcase`. |
| **Estado** | `Planning`, `Announced`, `On Sale`, `Live`, `Completed` o `Cancelled`. |
| **Descripción** | Texto libre con los detalles del evento. |
| **Inicio** | Fecha y hora de inicio. |
| **Fin** | Fecha y hora de finalización. |
| **Precio (centavos)** | Precio de referencia **en centavos** (ej.: `5000` = $50.00). |
| **Capacidad** | Aforo máximo. |
| **Moneda** | Código de moneda (por defecto `USD`). |
| **Ticket URL** | Enlace externo de venta de tickets (opcional). |
| **Evento público** | Casilla: márquela para que el evento sea visible públicamente. |

3. Haga clic en **Crear evento**.

> 💡 **Importante: los importes van en centavos.** El sistema guarda los precios en centavos. Para un precio de **$50.00** escriba **`5000`**.

### Después de crear el evento

- El evento aparece en el **Calendario de eventos** en su fecha.
- Puede subir el cartel del evento con el botón **Afiche**.
- Para vender entradas, continúe en [Crear tickets (tiers)](#crear-tickets-tiers).

---

## Crear tickets (tiers)

Los tickets se organizan en **tiers** (tipos de entrada): por ejemplo *General*, *VIP*, *Preventa*. Cada tier tiene su propio precio y cupo.

> Esta sección está en **Gestión de tickets (organizador)** dentro de la tarjeta del evento, y **solo es visible para el organizador**.

1. Abra la tarjeta del evento y ubique **Gestión de tickets (organizador)**.
2. Complete el formulario del tier:

| Campo | Descripción |
|-------|-------------|
| **Nombre** | Nombre del tier (ej.: *General*, *VIP*). |
| **Código** | Identificador corto del tier (ej.: `GEN`, `VIP`). |
| **Precio (centavos)** | Precio del tier en centavos. |
| **Cantidad** | Número total de tickets disponibles en este tier. |
| **Moneda** | Código de moneda (por defecto `USD`). |

3. Haga clic en **Crear tier**.

El tier creado aparece como una etiqueta que muestra **nombre · precio · disponibles**. Puede crear varios tiers para un mismo evento (preventa, general, VIP, etc.).

> Un tier deja de poder venderse cuando se agota su cantidad o cuando está inactivo.

---

## Guía para vender tickets (organizador)

Como organizador, su flujo de venta es:

### 1. Prepare la venta
- Cree el evento (ver [Crear un evento](#crear-un-evento)).
- Cree uno o más tiers (ver [Crear tickets (tiers)](#crear-tickets-tiers)).
- Opcional: ponga el evento en estado **On Sale**.

### 2. Registre o reciba ventas
Hay dos formas de que se vendan tickets:
- **El asistente compra desde la tarjeta del evento** (ver la [guía para comprar](#guía-para-comprar-tickets-asistente)).
- **Usted registra la compra** llenando el formulario de compra con los datos del comprador.

El cobro se procesa con **Stripe**; la entrada se envía por correo al comprador al confirmarse el pago.

### 3. Consulte las órdenes
En la tarjeta del evento, la sección **Órdenes del evento** (los asistentes ven **Mis órdenes**) lista cada orden con:

- Cantidad: **`N ticket(s)`**.
- Estado de la orden: `paid` (pagada, en verde), `pending`, `cancelled`, etc.
- Monto total y moneda.
- Las entradas individuales como etiquetas **`código · estado`**.

> Si ve **"Cargando órdenes..."** espere un momento; si aparece **"No se pudieron cargar las órdenes de tickets."**, refresque la página.

### 4. Gestione cada orden
En las órdenes con estado **`paid`** el organizador ve dos botones (ver [Reembolsos y cancelaciones](#reembolsos-y-cancelaciones)):
- **Cancelar** — marca la orden como cancelada.
- **Reembolsar** — procesa el reembolso del pago.

---

## Guía para comprar tickets (asistente)

Cualquier usuario con sesión iniciada puede comprar entradas desde la tarjeta del evento, en la sección **Tickets**.

1. Abra el evento y ubique la sección **Tickets**.
2. Complete el formulario de compra:

| Campo | Descripción |
|-------|-------------|
| **Tipo** | Seleccione el tier. Cada opción muestra el nombre y el precio; los tiers inactivos o agotados aparecen deshabilitados. |
| **Cantidad** | Número de entradas a comprar (por defecto `1`). |
| **Nombre comprador** | Su nombre. |
| **Email comprador** | Correo donde recibirá las entradas. |

3. Haga clic en **Comprar**.
4. Complete el **pago con Stripe** (datos de la tarjeta). Si dispone de un **código promocional**, ingréselo para aplicar el descuento antes de pagar.
5. Al confirmarse el pago, recibe la confirmación y **las entradas se envían al correo indicado** (con su código/QR).

### Revisar sus entradas
La sección **Mis órdenes** de cada evento muestra sus órdenes, con la cantidad, el estado (`paid`) y los códigos de cada ticket.

> Si el tier que quiere está agotado, no podrá seleccionarlo. Pregunte al organizador si habilitará más cupo o lista de espera.

---

## Check-in en el evento

El día del evento, el organizador valida la entrada de cada asistente.

1. En **Gestión de tickets (organizador)**, ubique el campo **Código ticket para check-in**.
2. Escriba (o escanee) el **código del ticket** del asistente.
3. Haga clic en **Check-in**.

El sistema marca la entrada como usada. Cada código de ticket aparece en la orden correspondiente como etiqueta **`código · estado`**.

---

## Reembolsos y cancelaciones

> Estas acciones las realiza **el organizador**, y solo están disponibles en órdenes con estado **`paid`**.

1. Abra **Órdenes del evento** en la tarjeta del evento.
2. Localice la orden pagada.
3. Elija una acción:
   - **Cancelar** — marca la orden como **cancelled**.
   - **Reembolsar** — marca la orden como **refunded** y procesa el reembolso del pago.

El movimiento queda reflejado en las **Finanzas del evento** (por ejemplo, como *Reembolso de tickets*).

> ⚠️ El reembolso es una operación sensible: confirme los datos de la orden antes de ejecutarlo, ya que devuelve el dinero al comprador.

---

## Preguntas frecuentes

**¿Por qué no veo la sección "Gestión de tickets (organizador)"?**
Esa sección solo aparece para el **organizador** (quien creó el evento).

**Escribí el precio y salió un valor enorme / minúsculo.**
Los precios se ingresan **en centavos**. Para $50.00 escriba `5000`, no `50`.

**No puedo seleccionar un tier al comprar.**
El tier está **inactivo** o **agotado** (sin disponibilidad). Elija otro tier o consulte al organizador.

**¿Dónde recibe el comprador su entrada?**
En el **correo (Email comprador)** indicado en la compra, una vez que Stripe confirma el pago.

**¿Puedo crear más de un tipo de entrada?**
Sí. Cree tantos **tiers** como necesite (preventa, general, VIP), cada uno con su precio y cantidad.

**¿Cómo aplico un descuento?**
Durante el pago, el comprador puede ingresar un **código promocional** válido para aplicar el descuento antes de confirmar.

**Aparece "Inicia sesión para crear eventos."**
Debe **iniciar sesión** para crear o gestionar eventos y tickets.

---

*Para el manual general de la plataforma, consulte [`Manual_Usuario_TDF.md`](./Manual_Usuario_TDF.md).*
