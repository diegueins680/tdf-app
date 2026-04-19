# Proyecto financiero de Domo del Pululahua

Preparado para la organización OpenClaw `venue-manager`.

Última actualización: 2026-04-19

## Hechos públicos verificados

- Domo del Pululahua se presenta públicamente como un espacio para eventos al borde del cráter del Pululahua, cerca de Quito.
- Su posicionamiento público incluye bodas, eventos corporativos, conciertos, talleres, retiros espirituales y sesiones fotográficas.
- El sitio público actual de Domo muestra un blog visible y formulario de contacto, pero no publica aforo, tarifario, paquetes, política de reservas ni carpeta de financiamiento.
- Pululahua está al norte de Quito, en la provincia de Pichincha. Referencias públicas de volcanología describen el complejo Pululahua como una depresión con domos de lava y actividad potencial.
- El competidor cercano El Cráter ofrece talleres, seminarios, bodas, bautizos, cumpleaños, celebraciones grupales, apoyo de hospedaje, catering y capacidad exterior de hasta 200 personas.
- Las tasas máximas efectivas anuales publicadas por el Banco Central del Ecuador para abril de 2026 ubican Productivo PYMES en 10.28%, Productivo Empresarial en 11.00% y Consumo en 16.77%. Esto refuerza que la solicitud debe presentarse como financiamiento productivo empresarial, no como consumo personal.

Fuentes:

- https://www.domopululahua.com/
- https://www.elcrater.com/en/eventos
- https://www.volcanesdelecuador.com/pululahua
- https://contenido.bce.fin.ec/documentos/Estadisticas/SectorMonFin/TasasInteres/Indice.htm

Documentos relacionados de la carpeta para bancos y cooperativas:

- `docs/venue-manager/domo-lender-one-pager.md`
- `docs/venue-manager/domo-lender-one-pager-print.html`
- `docs/venue-manager/domo-36-month-pro-forma.md`
- `docs/venue-manager/domo-36-month-pro-forma.csv`
- `docs/venue-manager/domo-lender-outreach-table.md`
- `docs/venue-manager/domo-loan-document-checklist.md`
- `docs/venue-manager/domo-loan-packet/`

## Supuestos entregados por el propietario

- Monto objetivo de crédito: USD 100,000.
- Plazo deseado: el más largo disponible.
- Cuota mensual máxima cómoda: USD 1,000.
- El inmueble es propio.
- Opciones de deudor: TDF Records o el propietario como persona natural, según cuál mejore la probabilidad de aprobación.
- La estructura principal del espacio ya está sustancialmente construida.
- Trabajos pendientes por financiar: paisajismo, parte del camino, mobiliario, señalética y capital de trabajo.
- Los permisos están en trámite.
- Domo operará directamente catering, barra, sonido y luces.
- Domo coordinará el resto de servicios a través de proveedores.
- Base de tamaño confirmada: diámetro de 16 m, equivalente a aproximadamente 201 m2 de área circular bruta.
- Primeros objetivos de financiamiento: bancos y cooperativas.

## Objetivo del crédito

Poner en marcha Domo del Pululahua como espacio de destino con canal profesional de cotización y reserva, preparación operativa y capital de trabajo suficiente para convertir eventos privados en flujo predecible.

Financiamiento solicitado: USD 100,000.

Restricción de pago: la estructura debe mantener el servicio de deuda en USD 1,000 mensuales o menos. Con la tasa máxima efectiva anual Productivo PYMES de 10.28% publicada por el BCE para abril de 2026, eso requiere aproximadamente una amortización de 18 años. Si los bancos o cooperativas solo ofrecen 10 a 15 años, la solicitud debe reducirse, acompañarse con periodo de gracia, estructurarse con saldo residual, o complementarse con aporte del propietario.

Meta de negociación: pedir una facilidad productiva o empresarial de 18 a 20 años, con el inmueble del propietario como garantía si ayuda, y con derecho de prepago cuando el flujo de eventos se estabilice.

Canales principales: bancos y cooperativas. La primera ronda debe comparar al menos tres ofertas en tasa, plazo, garantía, periodo de gracia, reglas de prepago, tratamiento del avalúo y posibilidad de que la solicitud se evalúe como crédito productivo PYME o empresarial, no como crédito de consumo.

## Estrategia de deudor

Primera opción recomendada: aplicar con TDF Records como empresa operadora, con el propietario como garante y el inmueble como garantía real si la institución lo exige.

Razones:

- El uso de fondos es productivo: lanzamiento del espacio, terminación de infraestructura, mobiliario, señalética, equipo operativo y capital de trabajo.
- El crédito productivo o empresarial suele ser más barato que el crédito de consumo bajo los techos vigentes del BCE.
- Dejar el crédito en la empresa mantiene ingresos, gastos, impuestos, depósitos y reportes del proyecto en una sola contabilidad operativa.
- El inmueble propio puede fortalecer la solicitud mediante garantía real o garantía personal.

Plan alternativo: aplicar como persona natural solo si la empresa no tiene historial financiero suficiente o si una institución específica ofrece mejores condiciones de aprobación, tasa, plazo o garantía. Si el crédito sale a nombre personal, debe documentarse que los fondos se invierten en TDF Records o en la operación de Domo para mantener trazabilidad contable.

## Modelo de negocio

Ingresos principales:

- Alquiler del espacio para bodas y celebraciones privadas.
- Eventos corporativos, talleres y activaciones de marca.
- Retiros y programación de bienestar.
- Conciertos, presentaciones íntimas y eventos culturales.
- Sesiones de foto, video y producción de contenido.

Ingresos secundarios:

- Fee de coordinación de producción.
- Paquetes de catering y barra operados por Domo.
- Paquetes de sonido e iluminación operados por Domo.
- Margen de coordinación de transporte.
- Comisiones de proveedores preferidos cuando sea legal y comercialmente apropiado.
- Margen por hospedaje aliado o paquetes overnight si se agregan más adelante.

## Primer embudo público

Página objetivo implementada:

- Ruta canónica: `https://tdf-app.pages.dev/domo-del-pululahua`
- Ruta alterna con redirección: `https://tdf-app.pages.dev/venues/domo-del-pululahua`
- API usada para ingreso de reservas: `POST https://tdf-hq.fly.dev/bookings/public`
- La lógica de cotización está actualmente en el cliente, por lo que la página pública funciona contra el API desplegado sin requerir un release de backend.

La herramienta de cotización estima:

- Precio base por tipo de evento.
- Uso del espacio por horas.
- Horas de montaje y desmontaje.
- Cargos por invitados adicionales sobre el número incluido.
- Operación opcional de catering y barra.
- Operación opcional de sonido e iluminación.
- Coordinación opcional de transporte Quito - Pululahua.
- IVA 12%.
- Depósito sugerido de 40% para separar fecha.

Estos valores son supuestos de planificación, no un tarifario final publicado.

## Uso inicial de fondos

Asignación de trabajo para discusión con bancos y cooperativas:

| Categoría | Presupuesto base | Notas |
| --- | ---: | --- |
| Paisajismo y acabados exteriores | USD 20,000 | Senderos, áreas exteriores, flujo de invitados, puntos fotográficos, drenaje y resiliencia climática. |
| Camino y acceso | USD 16,000 | Obra final de camino, flujo de parqueo, seguridad de llegada y señalización básica. |
| Mobiliario y equipo operativo | USD 18,000 | Mesas, sillas, mantelería, almacenamiento, equipo de servicio, calefactores o cubiertas si aplica. |
| Señalética, contenido y material comercial | USD 6,000 | Señalización vial y del espacio, foto, video, carpeta comercial y material de cotización. |
| Kit inicial de catering y barra | USD 9,000 | Estaciones de servicio, equipo de barra, inventario inicial y preparación sanitaria. |
| Base operativa de sonido e iluminación | USD 7,000 | Paquete base para discursos, música ambiental, ceremonias y presentaciones pequeñas. |
| Permisos, legal, seguros e ingeniería | USD 6,000 | Trámites, contratos, soporte de inspecciones y cobertura de responsabilidad civil. |
| Reserva de capital de trabajo | USD 13,000 | Staff, marketing, servicios, mantenimiento, depósitos y flotación de proveedores. |
| Contingencia | USD 5,000 | Clima, aumentos de precio, sorpresas de acceso y mantenimiento. |
| Total solicitado | USD 100,000 | Coincide con el principal solicitado. |

## Encaje del servicio de deuda

Cuotas estimadas para USD 100,000 a 10.28% efectivo anual:

| Plazo | Cuota mensual estimada | Encaje frente a meta de USD 1,000 |
| --- | ---: | --- |
| 10 años | USD 1,312 | Muy alta |
| 12 años | USD 1,185 | Alta |
| 15 años | USD 1,064 | Ligeramente alta |
| 18 años | USD 989 | Encaja |
| 20 años | USD 953 | Encaja |

Si una institución limita el plazo por debajo de 18 años con una tasa similar, para mantener la cuota de USD 1,000 se debe reducir el principal aproximadamente a:

| Plazo | Principal soportado por USD 1,000/mes |
| --- | ---: |
| 10 años | USD 76,000 |
| 12 años | USD 84,000 |
| 15 años | USD 94,000 |

## Supuestos de economía unitaria

Supuestos de planificación por validar:

- Área bruta interior de planificación: aproximadamente 201 m2, con base en el diámetro confirmado de 16 m.
- Aforo práctico antes de permisos finales: 70 a 100 invitados para eventos premium sentados; 60 a 90 para talleres o retiros; 120 a 160 para cóctel, formatos de pie o conciertos si salidas, baños, parqueo y plan de seguridad lo permiten.
- Duración promedio de evento reservado: 6 a 8 horas más 1 a 3 horas de montaje.
- Ingreso promedio por alquiler del espacio por evento: USD 1,600 a USD 3,400 antes de servicios opcionales.
- Margen bruto promedio del alquiler del espacio: 55% a 70% después de limpieza, personal, servicios básicos, coordinación y mantenimiento.
- Margen bruto promedio de catering, barra, sonido y luces operados directamente: 30% a 55%, según personal, merma de inventario, necesidad de alquiler y precios de proveedores.
- Margen bruto promedio de servicios tercerizados coordinados: 10% a 25%, según acuerdos de referido o coordinación.

## Escenarios de ingresos a 12 meses

| Escenario | Eventos/mes | Factura promedio | Margen de contribución | Contribución mensual antes de costos fijos |
| --- | ---: | ---: | ---: | ---: |
| Conservador | 2 | USD 3,500 | 45% | USD 3,150 |
| Base | 4 | USD 5,000 | 50% | USD 10,000 |
| Crecimiento | 6 | USD 6,500 | 52% | USD 20,280 |

Punto de equilibrio del caso base: con USD 4,000 de costo fijo austero y USD 1,000 de deuda, el espacio necesita aproximadamente USD 5,000 de contribución mensual. Eso equivale a unos dos eventos fuertes por mes con factura promedio de USD 5,000 y margen de contribución de 50%.

## Supuestos de costos fijos operativos

Meta mensual austera:

- Responsable comercial y coordinación de reservas: USD 900 a USD 1,400.
- Limpieza, mantenimiento y exteriores: USD 500 a USD 900.
- Servicios básicos, internet, seguridad y residuos: USD 350 a USD 700.
- Seguros, contabilidad y soporte legal: USD 300 a USD 650.
- Marketing y contenido: USD 600 a USD 1,200.
- Software, pagos, CRM y hosting: USD 120 a USD 300.

Rango objetivo de costo fijo: USD 2,770 a USD 5,150 mensuales antes de deuda.

Meta de servicio de deuda: máximo USD 1,000 mensuales.

## Contenido esperado de la carpeta bancaria

La solicitud final debe incluir:

- Resumen ejecutivo con posicionamiento de Domo y segmentos objetivo.
- Aforo verificado y uso permitido del sitio.
- Documentos de propiedad y avalúo disponible para garantía.
- Documentos societarios de TDF Records, historial tributario, estados de cuenta y paquete de garantía del propietario si aplica como empresa.
- Proformas de inversión y cotizaciones de contratistas o proveedores.
- Pro forma a 12, 24 y 36 meses.
- Flujo de caja con cobertura del pago de crédito.
- Evidencia de embudo comercial: solicitudes, cartas de intención y alianzas de lanzamiento.
- Tarifario preliminar y política de contrato, depósito y cancelación.
- Plan de riesgo para clima, acceso, ruido, seguridad, cancelación y divulgaciones geológicas/volcánicas.

## Trabajo inmediato pendiente

- Confirmar la ruta pública final y mantener desplegada la página de cotización y reserva.
- Reemplazar tarifas de planificación con tarifario aprobado de Domo.
- Cargar metadatos de Domo en el catálogo protegido de Social Events cuando exista token admin disponible.
- Agregar flujo interno para convertir solicitudes públicas en bloqueos de fecha, facturas, depósitos y eventos confirmados.
- Agregar panel de financiamiento para medir consultas, cotizaciones, depósitos, ingresos reservados y cobertura de deuda.

## Preguntas antes de entregar una solicitud formal

1. ¿TDF Records ya tiene estados financieros, declaraciones tributarias y estados de cuenta suficientes para análisis crediticio?
2. ¿El propietario está dispuesto a hipotecar o pignorar el inmueble de Domo, o se prefiere evitar garantía real si es posible?
3. ¿Cuál es la fecha esperada más reciente para permisos de eventos, alcohol, música amplificada, alimentos y operación nocturna?
4. ¿Cuál es la fecha objetivo del primer evento pagado y cuántos leads o compromisos blandos existen?
5. ¿Las tarifas de cotización deben mantenerse como estimados públicos o pasar a cotización privada cuando existan suficientes leads?
