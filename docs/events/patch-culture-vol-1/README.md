> Actualización 5 oct 2026 UTC: propuesta aprobada; borrador privado TDF **141**, venue **22**, tier **2** (20 × USD 20, inactivo), artistas **42/36/51**. Flyer original subido y hash SHA-256 verificado. Checkout público devuelve 404. El pago oficial sandbox y el flujo completo todavía no están verificados.

La infraestructura de inventario, admisión, QR y transferencias se fusionó en [PR480](https://github.com/diegueins680/tdf-app/pull/480), commit `b06a4d3926e03a0b72a23732e454a33c72eb9705`; la web de ese commit fue verificada en producción. Esto no acredita despliegue del backend ni compra de proveedor. Emisor confirmado por Diego: TDF Records, RUC 1793215092001. El tratamiento tributario sigue pendiente y el total aprobado permanece en USD 20.

La continuación añade un límite reutilizable por orden a la política existente, con validación de servidor y PostgreSQL y exposición en el checkout. El valor aprobado para este evento es cuatro. No se activa una política provisional con tarifa fiscal inventada: configurar cuatro, retención de diez minutos y el límite de transferencia de 13:30 al preparar la política fiscal definitiva, manteniendo el evento privado hasta completar la validación. Este límite no se presenta como cuota acumulada por comprador.

# PATCH CULTURE Vol. 1 — investigación y auditoría inicial

Estado actualizado: condiciones comerciales aprobadas por el organizador («Approved. Continue»). Borrador creado en TDF; **sin publicación, cobros activados ni E2E de proveedor acreditado**. Ver [condiciones aprobadas v1](propuesta-condiciones.md) y `event-candidate.json`.

Confirmados ahora: puertas13:30, jam16:30 incluida para compradores del taller, cierre con showcase de Llama Este Pez a las21:00, participantes Kevin Montenegro/Diego Saá/Emanuele Pilo-Pais, cero compromisos previos, ninguna venta Meet2Go, 18años y USD20 finales repartidos USD15TDF/USD5Andes. Los apartados siguientes conservan la auditoría inicial; sus faltantes ya respondidos se resuelven en el candidato actualizado. La aprobación de la propuesta queda registrada; la activación continúa bloqueada por la validación técnica y fiscal pendiente.

## Fuentes y alcance

- [Post original](https://www.instagram.com/p/DeFRIIplgpU/): HTML público HTTP 200, caption completo en metadata; autor `elemental.zoser`, publicación indicada el 4 de octubre de 2026. La herramienta web inicialmente devolvió cache miss; la consulta HTTP normal recuperó el documento público. No se usaron credenciales, APIs privadas, sesiones extraídas ni mecanismos de evasión.
- [Perfil del autor](https://www.instagram.com/elemental.zoser/): descripción pública «Sound & graphic research unit.». El HTML obtenido no expuso el enlace de inscripción de la bio.
- Video entregado: `/Users/diegosaa/Downloads/IMG_2368.mp4`, H.264/AAC, 1080×1350, 41,044 segundos. Se inspeccionaron fotogramas distribuidos por el clip y uno a resolución completa. No se transcribió el audio. Original conservado sin cambios; copia íntegra en `/private/tmp/tdf-event-defrii-evidence/IMG_2368.original.mp4`.
- [Andes Brewing, enlaces oficiales](https://linktr.ee/andesbrewing): sucursal La Pradera en Quito y mapa enlazado, coordenadas -0.1945826, -78.4855891. [Mapa enlazado por el venue](https://maps.app.goo.gl/d7t9CBaLMaxYHVMQ8).
- [Meet2Go](https://www.meet2go.com/): portada pública accesible, renderiza aplicación Flutter; no se obtuvo una ficha verificable de este evento. Las búsquedas del nombre exacto no aportaron otra fuente oficial útil. Un logo no prueba que exista inventario ni venta activa.
- TDF: consultas GET públicas a sitio canónico, API, próximos eventos, directorio, capacidades de pago y ficha de control existente. No se crearon órdenes ni se consultaron órdenes privadas.

La imagen `evidence/instagram-preview.jpg` es la **preview publicada**, no una afirmación de descarga del master original. Contiene el mismo evento y un logo Meet2Go adicional. `flyer-video-frame.png` es un fotograma derivado del archivo entregado. No se verificó el carrusel completo ni la lista de etiquetas/collaborators; el HTML disponible no los expone. Las evidencias y sus hashes están en `evidence/source-provenance.json`.

## Información confirmada

| Campo | Evidencia |
|---|---|
| Nombre | PATCH CULTURE Vol. 1, flyer/video y preview de Instagram |
| Formato | Taller introductorio de síntesis modular, jam de máquinas, grabación en vivo y demostración/live act |
| Fecha publicada | Sábado 24 de octubre; sin año escrito en flyer/caption |
| Inicio taller | 14:00 |
| Fin taller / inicio jam | Flyer 16:30; caption ofrece 16:00 / 16:30 |
| Venue | Andes Brewing, La Pradera |
| Ciudad/país | Quito, Ecuador, por identificación del venue oficial |
| Precio | USD 20 por persona para el taller |
| Inclusión expresa | Una pinta |
| Cupo | Máximo 20 personas para el taller según caption |
| Después del taller | Apertura al público para jam, grabación y live act; sin precio expreso |
| Autor | @elemental.zoser |
| Marcas visibles | Elemental Zoser Sound Studio, Andes Brewing Co., TDF Records; Meet2Go únicamente en preview publicada |
| Inscripción | Caption remite a link en bio, destino no recuperado |

## Inferencias y propuesta inicial (histórico; respuestas posteriores arriba)

- **2026-10-24**, confianza alta: publicación fechada 2026-10-04, referencia a la fecha próxima y coincidencia de sábado. No se deduce el año del nombre del archivo.
- Timezone `America/Guayaquil`, por ubicación en Quito; inicio propuesto `2026-10-24T14:00:00-05:00`.
- Audiencia inicial: personas interesadas en síntesis modular y música electrónica experimental. Son tags editoriales propuestos, no géneros declarados del lineup.
- Admisión general y un único producto «Taller + pinta» a USD 20. El acceso posterior a la jam y su capacidad requieren aclaración. No inventar VIP, mesas, early bird ni descuentos para un taller de 20 plazas con precio ya anunciado.
- Una página canónica de TDF puede presentar ambos bloques; el cupo del taller debe distinguirse del aforo de la jam y del venue. No usar automáticamente `SocialEvent.capacity=20` para toda la jornada.
- Slug editorial propuesto `patch-culture-vol-1`; las rutas públicas existentes usan ID `/eventos/:eventId`. Cualquier alias futuro debe resolver a una única URL canónica, sin duplicar evento.

## Faltantes e inconsistencias de la investigación inicial (histórico)

1. Horario final del taller/jam, apertura de puertas y fin de jornada. Se preserva `endTime=null` hasta tener evidencia, permitido por la arquitectura.
2. Jam abierta no significa necesariamente gratuita. Faltan precio/registro/aforo y condiciones para participar con máquinas.
3. Nombres de instructor/es y live act, roles legales/operativos de marcas y contacto de soporte. Logos no equivalen a roles de organizador ni permisos de cobro.
4. Inventario ya vendido, reservado o comprometido en otros canales; cortesías y reservas dentro de las 20 plazas; política de coexistencia o retiro de Meet2Go. No asumir cero ventas por no encontrar una ficha pública.
5. Precio total final, distribución de fees, facturación/impuestos aplicables y beneficiario de la recaudación. No trasladar tasas de ejemplo del repositorio a este evento.
6. Edad mínima, identificación, restricciones, equipos necesarios, transferencias, reembolsos/cancelaciones y condiciones de grabación. La pinta no prueba que todo el evento sea exclusivamente para mayores de edad.
7. El MP4 entregado omite el logo Meet2Go presente en la preview. Conservar ambas versiones, sin borrar marcas ni presentar una como master de la otra.

## Arquitectura y estado técnico revisados

Referencia de código: `origin/main` = `fdac8e76523befee1603f49f6c7cf7d00762931b`. El checkout de trabajo está modificado y más antiguo; no se alteró. Referencia móvil fijada en ese root: `cd3f664663f328a47341914524a2af130ae0d3ec`; el móvil local está en `53569fc4baa842a6882235d9a12c4ee68c44ff24`. No confundir estas referencias con el binario nativo distribuido.

Backend público comprobado en `/version`: `645f56fcc44f81609fbfd0e03d683b40376ce77a`, build `2026-09-30T10:36:07Z`. Hay diferencia de versiones entre API desplegada y código revisado. Infraestructura canónica documentada: Cloudflare Pages para web/preview, API Hetzner con Compose/Caddy/PostgreSQL; permanecen herramientas Fly históricas. No ejecutar un deploy histórico por inercia.

| Área | Implementación reutilizable y evidencia | Brecha / comprobación pendiente |
|---|---|---|
| Arquitectura | Haskell/Servant/Persistent/PostgreSQL; React/Vite/MUI/React Query; Expo/React Native submódulo; OpenAPI y clientes generados | Elegir rama limpia y reconciliar versiones desplegadas; no crear otra plataforma |
| Usuarios, RBAC, CRM, perfiles | `ServerAuth.hs`, `API/SocialEventsAPI.hs`, directorio y parties; `organizerPartyId`, event-manager checks; `EventOperations` con transiciones y RACI | Verificar staff mínimo por evento, ownership y acceso horizontal con API real; no atribuir permisos por logos |
| Venues y relaciones | `Models/SocialEventsModels.hs`: Venue, SocialEvent, artistas, referencias externas; directorio público | Búsqueda pública de Patch Culture, Zoser y Andes: 0 resultados. Próximos eventos: 22, ninguno coincide. No descarta borradores/perfiles privados; deduplicación administrativa pendiente |
| Ingestión reutilizable | `EventResearchAPI.hs`: runs, candidates, evidencia/cambios, materialize; fuentes de EventDiscovery | Usar candidato con procedencia y revisión antes de materializar; no construir otro importador independiente |
| Descubrimiento | Upcoming, directorio/búsqueda, Discover/Fan Hub, RSVP, perfiles y feeds | Probar visibilidad por fecha, ciudad, intereses, workflow y permisos de este evento |
| Órdenes/checkout | `Server/EventTicketCheckout.hs`, `Routes/EventTickets.hs`, `Commerce/EventTickets.hs`; orden de dominio enlazada a checkout canónico; huésped web con lookup capability | Página pública emite códigos de tickets pero no QR visual en ese flujo; revisar recuperación segura de compra en otro dispositivo |
| Precios/fees | Cálculo backend en unidades menores, snapshot de política aprobada, cantidad, descuento, fees comprador/organizador, impuestos y ledger | No hay política aprobada de este evento; fee de procesador y neto real deben conciliarse, no inferirse del payable |
| Inventario | Locks de evento/tier/promo, advisory lock para idempotencia, reserva condicional, expiración, constraints y estado de fulfillment separado | Ejecutar carrera real con 1–2 entradas, límites por comprador y cruce entre canales; `quantity` tiene límite general 100, no demuestra cuota configurable acumulada por comprador |
| Pagos/webhooks | PayPal/Datafast en checkout público, plataforma canónica con capacidades, bindings y eventos verificados; proveedores adicionales en main | Consulta viva EC/USD2000/event_ticket: tarjeta y PayPal `routes=[]`. No demuestra ausencia de toda integración legacy; sí impide afirmar ruta canónica utilizable. Falta sandbox oficial de este circuito |
| Tickets y transferencias | Ticket separado de orden/comprador, código único, holder, historial, estados y transferencia | Verificar nominación por ticket en compra múltiple, revocación del código previo y compatibilidad guest/móvil |
| QR | QR web legacy y QR móvil a partir del código | Legacy `getTicketQR` incorpora email y clave HMAC literal; payload no coincide con API check-in que busca código o ID. Unificar contrato opaco sin PII y rotación/revocación |
| Check-in | Endpoint administrado por evento y entrada manual de código en `SocialEventsPage` | `checkInTicket` lee ticket/orden y actualiza en llamadas `runSqlPool` distintas; no comparación atómica al escribir. Ya usado devuelve DTO exitoso. Corregir carrera, distinguir resultados, scanner móvil y contingencia de red segura |
| Cortesías | Emisión directa restringida a tarifa cero; fulfillment contempla entitlement sin pago | No se encontró guest list completa con categorías, creador, autorizador y separación de capacidad comercial. No simular pago para cortesía |
| Promos | Fijo/porcentaje, vigencia, total de usos, evento/tier elegibles, redenciones y reserva | Modelo revisado no muestra límite por usuario; atribución por artista/partner no está enlazada al funnel de ticket |
| Waitlist | Modelo y rutas activas/notified, expiry y convertedOrderId | `notifyWaitlist` revisado solo actualiza estado y vencimiento; no envía notificación ni reserva plazas. No afirmar lista de espera funcional de extremo a extremo |
| Refunds/cancelación | Modelos, rutas, checkout refund runtime, proveedor/ledger canónicos | Verificar reembolso real sandbox, total/parcial, disputa y carrera refund/check-in; no asumir integración al ver formulario |
| Comunicaciones | `sendTicketConfirmationForOrder`, SMTP y notificaciones existentes | Email best-effort tras emisión: error se registra, sin retry durable visible en ese camino. Fecha UTC y enlace `tdf://tickets`; revisar recuperación web guest y outbox/reintentos. Recordatorios/cambios/postevento requieren consentimiento y pruebas |
| Analítica/dashboard | PostHog, UTMs/referrals y eventos de RSVP/share, finanzas/logística de evento | No aparece instrumentación del funnel completo en `PublicEventTicketsPage`; faltan reconciliación de GMV/neto/abandonos/canal/check-ins y dashboard restringido verificable |
| SEO/share | Function `/eventos/[eventId]`, OG, Twitter, canonical, JSON-LD MusicEvent, protección de visibilidad y escape | GET real evento111 confirma canonical y OG `www.tdfrecords.net`. Completar offers/organizer/performer/address y tipado adecuado del taller; comprobar sitemap y previews del evento nuevo |
| Móvil/beta | App Expo, tickets y QR, checkout legacy; documentación con Play testing y TestFlight | No se ejecutó app ni verificó versión instalada. Canales beta documentados no equivalen a enlaces activos: tool web no pudo validarlos. Web guest debe funcionar sin instalación |
| Accesibilidad/UX | MUI, formulario responsive, estados separados pago/fulfillment, errores y loading | Auditoría teclado/focus/labels/contraste/zoom/touch/screen reader/scanner pendiente en navegador real |
| CI/CD y seguridad | Hspec/Jest/Playwright, suites SQL, pruebas de pagos/migraciones, checks formales/event operations y release gates | No se ejecutaron suites ni CI nuevos. No se debilitan gates; migraciones/rollback/deploy se prepararán después de aclaraciones |

## Hallazgos prioritarios comprobados por lectura

1. **QR legacy:** `tdf-hq/src/TDF/Server/SocialEventsHandlers.hs`, `getTicketQR` (aprox. 5848): email en payload y clave HMAC fija en fuente. No afirmar explotación: no se intentó falsificar ni canjear entradas. La API de check-in usa ID/código, no verifica ese formato HMAC.
2. **Check-in no atómico:** mismo archivo, `checkInTicket` (aprox. 4688): lectura de estados separada del update incondicional. Dos scanners pueden leer el mismo estado admisible; también existe ventana frente a refund/cancelación. Es un hallazgo estático, no una carrera reproducida en este turno.
3. **Comunicaciones no durables en ese camino:** `sendTicketConfirmationEmailBestEffort` (aprox. 1035); failure log no asegura entrega, reintento ni recuperación. `notifyWaitlist` (aprox. 5790) no equivale a envío.
4. **Falta verificación de pago real:** capacidades canónicas vivas vacías para tarjeta/PayPal en las dos consultas documentadas; no se inició ningún intento de pago para descubrir métodos.

## Propuesta comercial inicial

Conservar el precio anunciado de USD 20 y un solo tier del taller. No hay justificación suficiente para introducir precios escalonados nuevos en este cupo pequeño. La investigación de comparables no es necesaria para reemplazar un precio ya publicado. La jam se deja sin oferta comercial hasta aclarar condiciones.

| Escenario ilustrativo, sin previsión de demanda | Talleres pagados | Valor nominal bruto |
|---|---:|---:|
| Conservador, 50% | 10 | USD 200 |
| Base, 75% | 15 | USD 300 |
| Completo, 100% | 20 | USD 400 |

Estos montos suponen 20 plazas comercializables, sin cortesías, descuento ni ventas previas. No equivalen a ingreso neto, recaudación nueva de TDF ni beneficio. Con `c` cortesías que ocupen plaza y `r` reservas internas, máximo nominal comercial = `20 × (20 − c − r)` USD; ventas ya hechas en otros canales consumen ese mismo inventario. Neto = cobrado − descuentos aplicables − fees − impuestos − devoluciones − costos; evitar descontar dos veces descuentos ya incluidos en cobrado. Tasas, costos y obligaciones fiscales no verificados: no se inventa neto ni break-even. Los escenarios no son predicciones.

## Decisiones técnicas y fuentes

- Reutilizar checkout/órdenes/ingestión existentes: ADR0112 y ADR0113 del repositorio. Estados de pago y emisión separados; no emitir por un redirect.
- Reserva/check-in/refund deben compartir límites transaccionales y orden consistente de locks: [PostgreSQL17, explicit locking](https://www.postgresql.org/docs/17/explicit-locking.html). Probar reintentos y contención real, no solo modelos.
- Verificar amount/currency/merchant/environment/resource y firma/evidencia server-side: [PayPal idempotency](https://developer.paypal.com/api/rest/reference/idempotency/), [webhooks](https://developer.paypal.com/api/rest/webhooks/rest/), [OWASP Transaction Authorization](https://cheatsheetseries.owasp.org/cheatsheets/Transaction_Authorization_Cheat_Sheet.html).
- QR opaco y validación online atómica. Offline no puede prometer anti-replay global entre scanners desconectados; contingencia inicial: dispositivo autorizado con conexión alternativa/código manual, sin marcar válido un resultado no confirmado.
- Minimizar datos de QR, analytics y enlaces compartidos. Probar autorización/IDOR, revocación tras transferencia/refund, replay, webhooks duplicados y no registrar capacidades privadas.
- Accesibilidad objetivo WCAG2.2 AA: [W3C](https://www.w3.org/TR/WCAG22/). Verificación manual y automatizada, no declaración de conformidad anticipada.
- Metadata ligada al evento público y ofertas reales: [Schema.org Event](https://schema.org/Event), [Google Event structured data](https://developers.google.com/search/docs/appearance/structured-data/event). No prometer rich results.

## Puerta de validación posterior

Después de resolver datos comerciales: candidato/draft canónico y relaciones deduplicadas; pruebas locales/integración; sandbox oficial evento→checkout→pago→orden→emisión→QR→check-in; email recibido y analítica reconciliada; pruebas simultáneas último cupo/último canje; doble click, webhook duplicado, timeout/pending/failed, refresh/back/multitab, abandono, invalidez/cancelación/refund, usuario sin permisos; móvil real/viewport/teclado; CI y revisión requeridos; merge/deploy con rollback y smoke. Una única aprobación final concreta antes de habilitar los primeros cobros o nuevos términos.

Se ejecutó `npm run ai:doctor`: 15 OK, 3 advertencias (workspace modificado, estado de autenticación gh reportado fallido en sandbox, configuración loop apunta a main), 0 errores. La autenticación GitHub deberá revalidarse en contexto autorizado antes de concluir que requiere intervención. Se hicieron GETs públicos, inspección de código y medios; **ninguna compra, E2E, carrera, envío, migración, merge o deploy en este turno**.
