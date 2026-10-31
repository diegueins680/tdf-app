# Prompt: auditoría formal del sistema TDF (especificación, lógica y conformidad)

> Uso: entregar este documento completo a un agente (o a un flujo de varios agentes) con acceso
> a `diegueins680/tdf-app` y `diegueins680/TDF-mobile`. Es un encargo de varias fases; cada fase
> termina con evidencia commiteada antes de pasar a la siguiente.

## Objetivo

Para **todo el sistema TDF** — backend Haskell (`tdf-hq`), web (`tdf-hq-ui`), móvil
(`TDF-mobile` y `tdf-mobile`), funciones/servicios auxiliares (`functions`, `streaming`,
`tidal-agent`), contratos entre ellos (OpenAPI, clientes generados, webhooks) e
infraestructura (CI, despliegue, migraciones, respaldo y recuperación):

1. **Lógica:** detecta todo lo que podría tener más sentido, ser más lógico o ser más correcto
   — reglas de negocio, flujos, máquinas de estado, especificaciones y código.
2. **Especificaciones:** determina qué especificaciones formales faltan para describir el
   sistema, y verifica que las existentes tengan sentido (reflejan el negocio) y sean
   consistentes (entre ellas y consigo mismas). Escribe las que falten.
3. **Conformidad:** demuestra que la implementación corresponde a las especificaciones,
   con pruebas, no solo con pruebas acotadas o tests.
4. **Corrección:** corrige especificaciones y código donde corresponda.

Todo con métodos formales.

## Fuentes de autoridad

- Punto de partida: `formal/system/README.md` (índice de autoridad, registro de requisitos,
  trazabilidad, obligaciones abiertas, línea base de entrega) y `FORMAL_VERIFICATION.md`.
- Los modelos de `formal/**` (TLA+/PlusCal, Alloy, SMT) son autoritativos dentro de su alcance
  aprobado.
- `specs.yaml` y los `.md` de la raíz (`FEATURES.md`, `TICKETING_*`, `STRIPE_*`, etc.) son
  **evidencia de intención histórica**, no autoridad. Úsalos para reconstruir la intención
  de negocio y señala dónde contradicen la autoridad actual.
- Reconcilia tú mismo la autoridad: si dos fuentes discrepan, documenta cuál prevalece y por qué.

### Cuando especificación y código discrepan

Decide qué lado es correcto según, en este orden: (1) seguridad, integridad del dinero y
privacidad; (2) intención de negocio reconstruida; (3) coherencia con el resto del sistema;
(4) comportamiento actual en producción (compatibilidad con clientes desplegados). Corrige el
lado incorrecto y registra la decisión con su justificación. Si la decisión depende de una
política de negocio que no puede inferirse (precios, impuestos, plazos legales, quién puede
hacer qué), **no adivines**: añádela a la lista de preguntas abiertas para el responsable y
deja el comportamiento actual sin cambios.

## Prioridades

Trabaja por dominio en este orden (riesgo × impacto):

| Prioridad | Dominio |
|---|---|
| P0 | Dinero: pagos, Stripe, checkout, ticketing, facturas, reembolsos, webhooks, precios e impuestos, aritmética monetaria |
| P0 | Seguridad: autenticación, sesiones, roles y permisos, rutas públicas, tokens, borrado de cuenta y privacidad de datos |
| P1 | Concurrencia e integridad: idempotencia, reintentos, deduplicación, bloqueos, reservas de estudio y zonas horarias, admisión de tickets, ciclos de vida de eventos |
| P1 | Contratos entre sistemas: API Servant ↔ OpenAPI ↔ clientes web/móvil ↔ versiones móviles ya publicadas |
| P2 | Dominios de producto: social y mensajería, música y compras, artistas, cursos, campañas, notificaciones |
| P3 | Infraestructura: CI y sus gates, despliegue, migraciones, respaldo y recuperación, observabilidad |

Dentro de cada dominio, empieza por las especificaciones que faltan o se contradicen antes de
pulir las que ya están bien.

## Métodos formales recomendados

Usa la herramienta más débil que dé la garantía necesaria y declara siempre su alcance.

| Necesidad | Herramienta |
|---|---|
| Concurrencia, protocolos, máquinas de estado (exploración) | TLA+/PlusCal con TLC (ya en uso) |
| **Prueba** no acotada de invariantes de estado | Invariantes inductivos verificados con Apalache (`--inductive`) y, para P0, TLAPS |
| Estructura, permisos, relaciones, consistencia de modelos | Alloy (ya en uso); para propiedades P0 que lo necesiten, demostración con Alloy + prueba inductiva equivalente en TLA+ |
| Aritmética: dinero, redondeo, impuestos, tiempo y expiraciones | SMT (Z3) con obligaciones derivadas del código fuente (ya en uso), en ancho completo (Int64 y overflow) |
| Prueba a nivel de código Haskell | LiquidHaskell (tipos refinados) para los módulos P0 de dinero y autorización; tipos y constructores inteligentes donde basten |
| Prueba a nivel de código TypeScript (web/móvil) | Tipos discriminados exhaustivos y validación en fronteras; obligaciones SMT para la lógica numérica crítica |
| Conexión modelo ↔ código | Refinamiento: mapeo explícito de estado/acciones del código a acciones del modelo, demostrado para P0; para el resto, validación de trazas (trazas reales o de tests contra el modelo) y tests basados en modelo (QuickCheck/fast-check) generados desde la especificación |
| SQL y estado persistente | Comparación del esquema migrado (PostgreSQL 17) con los estados y transiciones declarados (`check-state-machines.py`) y pruebas de concurrencia reales en PostgreSQL |

Reglas:
- Una ejecución acotada de TLC o Alloy **no es una prueba**; preséntala como "verificado
  hasta el límite N" e indica el límite.
- Cada modelo debe tener **controles negativos** (mutaciones que sí deben fallar) y demostrar
  que no es vacuo (escenario satisfacible, acciones alcanzables).
- Declara explícitamente la base de confianza (traducciones manuales código→modelo, supuestos
  sobre PostgreSQL, Stripe, la red, el reloj) y qué queda fuera.
- Nunca debilites un invariante, borres un control negativo, reduzcas un límite ni desactives
  un test o gate para obtener verde.

## Fases

1. **Inventario y reconciliación de autoridad.** Ejecuta `scripts/specification-inventory.py`
   y `scripts/specification-conformance.py`; obtén la superficie compilada de la API, las rutas
   públicas, las máquinas de estado del esquema, las pantallas y llamadas del móvil y web, y
   los jobs de CI. Produce una matriz requisito ↔ especificación ↔ código ↔ evidencia, y lista
   lo que no tiene especificación.
2. **Coherencia de especificaciones.** Para cada especificación: ¿es internamente consistente
   y no vacua? ¿Contradice a otra (p. ej. dos modelos con reglas de autorización o estados
   distintos para la misma entidad)? ¿Sus supuestos de equidad, límites y entorno son
   realistas? ¿Refleja la intención de negocio?
3. **Especificaciones faltantes.** Escribe las que falten por orden de prioridad, cada una con
   invariantes, propiedades de vivacidad cuando aplique, controles negativos y mapeo a requisitos.
4. **Revisión lógica.** Señala reglas, flujos y diseños que podrían ser más lógicos o
   correctos (casos límite, estados imposibles representables, carreras, dobles cobros,
   escaladas de permisos, zonas horarias, redondeos, mensajes de error engañosos, UX que
   contradice el modelo).
5. **Pruebas de conformidad.** Demuestra el refinamiento implementación → especificación con
   el método de la tabla según la prioridad. Cualquier discrepancia es un hallazgo.
6. **Correcciones.** Corrige especificaciones y código, añadiendo tests de regresión y
   evidencia formal en cada corrección.
7. **Informe y decisión de entrega.** Actualiza `formal/system/README.md`,
   `assurance-report.md` y la decisión de entrega.

## Clasificación de hallazgos

Usa las severidades de `npm run audit:formal`: `critical`, `error`, `warning` e `info`.

- **critical:** pérdida o creación de dinero, escalada de privilegios, fuga de datos, pérdida
  de datos o un modelo que da una falsa garantía.
- **error:** comportamiento incorrecto observable o contradicción entre especificaciones.
- **warning:** especificación faltante en zona de riesgo, garantía solo acotada o testeada
  donde se requiere prueba, o lógica confusa.
- **info:** mejora de claridad o diseño.

Ordena por severidad, luego por prioridad del dominio, luego por esfuerzo (lo barato primero
a igual impacto). Cada hallazgo incluye: ubicación (`archivo:línea`), requisito afectado,
escenario o contraejemplo concreto, evidencia, corrección propuesta o aplicada, y nivel de
garantía tras la corrección.

## Niveles de garantía (etiqueta obligatoria en cada afirmación)

`demostrado` (prueba no acotada mecánicamente verificada, incluido el refinamiento al código)
› `demostrado-en-modelo` (prueba no acotada sobre el modelo; correspondencia con el código por
revisión o trazas) › `verificado-acotado` (TLC/Alloy hasta un límite declarado) › `testeado` ›
`revisado` › `sin-verificar`.

## Entregables

1. **Informe** `formal/system/audit-<fecha>.md`: resumen ejecutivo, matriz de trazabilidad,
   hallazgos clasificados, decisiones de especificación vs código, base de confianza,
   exclusiones y preguntas abiertas para el responsable.
2. **Especificaciones** nuevas o corregidas en `formal/**`, cada una con controles negativos
   y registrada en el índice de autoridad y en el inventario.
3. **Correcciones de código** con tests y evidencia formal.
4. **Cambios en CI** para que cada nueva prueba u obligación sea un gate que falle ante una
   desviación.
5. **Entrega en PRs pequeños por dominio y prioridad** (P0 primero), cada uno con todos sus
   gates en verde, sobre ramas derivadas de `claude/prompt-formal-validation-b2jmz3` en
   `tdf-app` y `TDF-mobile`. Los cambios que afecten contratos entre repos se coordinan en
   ambos repos y deben ser compatibles con versiones móviles ya publicadas.

## Criterio de terminado

- Todos los gates pasan: `npm run quality`, `npm run verify:formal`,
  `npm run verify:event-operations:formal`, `npm run audit:formal -- --fail-on error`, los
  checks de `formal/system` (inventario, conformidad, superficies nuevas, API compilada,
  rutas públicas, máquinas de estado, clientes generados) y los tests de backend, web y móvil.
- Cada requisito tiene especificación, mapeo a código y evidencia con su nivel de garantía;
  no queda superficie material sin mapear.
- Todo requisito P0 alcanza `demostrado`; todo P1 al menos `demostrado-en-modelo`; P2 y P3
  al menos `verificado-acotado` o `testeado`. Cualquier excepción queda justificada en el
  informe.
- No quedan hallazgos `critical` ni `error` abiertos, salvo los bloqueados por una pregunta
  de negocio documentada.
- La decisión de entrega en `formal/system` se actualiza con su fundamento: aprobada, o
  bloqueada con una lista explícita de obligaciones pendientes.

## Forma de trabajo

- No hay límite de tiempo ni de costo: se puede repartir el trabajo en agentes paralelos por
  dominio, con un revisor independiente que intente refutar cada prueba y cada hallazgo
  antes de aceptarlo.
- Reporta honestamente: si algo no se pudo demostrar, dilo con su nivel real de garantía.
- Commits atómicos y descriptivos. No reescribas historia compartida.
