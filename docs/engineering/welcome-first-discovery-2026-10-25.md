# Bienvenida y descubrimiento progresivo — 25 de octubre de 2026

## Estado, fuentes y límites

Este documento especifica el **candidato de bienvenida**. Los resultados locales y su alcance se registran en `reports/welcome-first-discovery-2026-10-25.json`; integración y producción siguen siendo hitos separados. No acredita
por sí mismo implementación completa, pruebas aprobadas, integración, despliegue
ni mejora de conversión. Cada hito requiere evidencia del responsable y del SHA
correspondiente.

- Base de trabajo: `b732efd65de23e5b97f3f619a2242397c64965c3`, integración
  protegida de #610. El checkout principal del operador es divergente y contiene
  trabajo ajeno; este incremento utiliza una copia aislada.
- Feedback: ocho notas de voz del 25/oct, de las 16:39:53 a las 16:46:43,
  transcritas localmente. Son comentarios de una misma conversación, **no ocho
  participantes independientes**. La frecuencia del problema en la población
  y su efecto sobre conversión no están medidos.
- Código contrastado: `tdf-hq-ui/src/routes/publicRoutes.tsx`,
  `tdf-hq-ui/src/pages/DirectorySearchPage.tsx`,
  `tdf-hq-ui/src/pages/TdfPlatformPage.tsx`,
  `tdf-hq-ui/src/pages/RecordsPublicPage.tsx` y catálogos de traducción.
- Recibo productivo público de solo lectura: seis observaciones de `/`, `/tdf` y `/records`, en escritorio y Pixel 7 emulado. Fecha: `2026-10-25T22:34:11.046Z`. `/` termina en `/inicio`; `/tdf` antepone tres CTA de registro. Capturas y resumen acompañan el informe de entrega. La primera ejecución parcial se interrumpió porque el observador suponía incorrectamente un h1 en `/records`; se corrigió el observador sin cambiar el producto. La ejecución completa posterior no representa entrevistas ni activación humana.
- Las referencias a PR #616 y #615 proceden del contexto operativo y las
  revisiones de sus propietarios. Su estado y su head deben comprobarse de
  nuevo antes de decidir una integración o un piloto.

La inspección no constituye una auditoría exhaustiva de todas las rutas. No se
han entrevistado participantes nuevos ni realizado transacciones productivas
como parte de este documento.

## Línea base cualitativa

El visitante anónimo de `/` llega a `/inicio`. Tanto `/inicio` como `/buscar`
renderizan el buscador del directorio: campo «¿Qué necesitas?», ciudad,
geolocalización opcional y filtros de profesión, servicio, instrumento y género.
El feedback describe esa entrada como una encuesta y no entiende si las opciones
describen a la persona o lo que busca. El código inspeccionado no acredita un
cuestionario obligatorio de bienvenida: el problema reproducible de contenido
es la presentación inicial del buscador y su lenguaje.

En `/tdf`, los tres CTA principales de la base llevan a crear una cuenta o un
perfil. Los servicios y el estudio aparecen después de bloques para fans y
artistas. En `/records` ya existe contenido público de grabaciones, lanzamientos
y sesiones que puede servir como destino para conocer trabajo real.

Las capturas de entrada pública muestran el título del directorio al entrar desde `/` y «TDF Records» en `/tdf`. La navegación de línea base no demuestra por sí sola que finalizara toda carga asíncrona de `/records`; la prueba candidata exige contenido explícito. Esto no prueba comprensión, confianza ni activación humana.

Las tasas de registro, activación, abandono y retención para este incremento
son **desconocidas**. No se convierten clics, visitas ni cuentas sintéticas en
usuarios activados. No se atribuyen a esta propuesta métricas de cohortes
históricas de otros trabajos.

## Priorización y backlog

Primero se aplica una categoría: **P0** para privacidad, autoridad o integridad;
después **P1** para tareas y comprensión; finalmente **P2** para ampliaciones.
Una puntuación P1 alta nunca permite saltarse un P0 ni los controles de entrega.
Un P0 con propietario activo permanece visible y permite avanzar trabajo
independiente sin duplicar su implementación.

Para ordenar dentro de una categoría:

`P = 100 × (I / 5) × (C / 5) × ((6 − E) / 5) × ((6 − R) / 5)`.

Cada dimensión utiliza valores ordinales de 1 a 5: impacto esperado `I`, confianza
en la evidencia `C`, esfuerzo `E` y riesgo de regresión/entrega `R`. Esfuerzo y
riesgo altos reducen la puntuación. Estos valores son estimaciones revisables,
no incidencia, ROI ni una mejora porcentual de producto.

| Orden | Problema y evidencia | I/C/E/R; P | Acción y aceptación |
| --- | --- | --- | --- |
| P0, propietario externo activo | #616: cierre de sesión fallido y coordinación de cambios de sesión; reproducción y revisión independientes registradas. | 5/5/3/4; 24,0 | Reutilizar el trabajo del propietario y exigir evidencia de su head final. No modificar su rama ni certificar un piloto humano mientras el bloqueo aplicable siga abierto. |
| P0, propietario externo activo | #615: guardias de propietario de borradores, preferencias y moderación de recuerdos; defecto encontrado en un candidato no desplegado. | 5/5/3/4; 24,0 | Mantener la separación de cuentas bajo su propietario. Este incremento no cambia sesión, API ni moderación y no declara resuelta esa dependencia. |
| P1, seleccionado | Entrada a `/inicio` dominada por búsqueda y categorías ambiguas; código, observación productiva parcial y feedback cualitativo convergen. | 4/3/2/2; 30,7 | Bienvenida antes del buscador existente, acceso público y etiquetas inequívocas de búsqueda; `/buscar` conserva entrada directa. |
| P1, composición del incremento | `/tdf` prioriza tres CTA de registro antes de mostrar servicios y estudio. | 4/3/2/2; 30,7 | CTA de exploración pública y servicios antes de invitaciones de cuenta; destinos reales y contexto conservado. |
| P1, medición pendiente | No hay línea base fiable que vincule comprensión inicial con un resultado persistido. | 4/2/3/3; 11,5 | Revisar la instrumentación existente; definir eventos y denominadores antes de comparar. No activar proveedores ni inventar datos. |
| P2, contenido pendiente | Foto del anfitrión mencionada en los audios sin identificación verificable; portadas del feed no acreditan una sala concreta. | 3/2/3/2; 11,5 | Inventariar fotografías autorizadas de personas y espacios. No presentar a otro instructor como Diego ni portadas como fotografías del estudio. |
| P2, contenido pendiente | Solicitud de demostraciones didácticas y sugerencia de mapa/recorrido. | 3/2/4/3; 5,8 | Obtener videos, plano y material panorámico reales. Separar estas ampliaciones de la corrección de entrada. |

La prioridad de producto no modifica la cola de admisión, revisiones requeridas,
protecciones de rama ni autorización de despliegue. Las cifras de P0 anteriores
son estimaciones de planificación apoyadas en la evidencia de sus propietarios,
no una nueva reproducción ejecutada por este documento.

## Recorridos y resultados completos por rol

| Actor | Recorrido principal | Resultado significativo y comprobación |
| --- | --- | --- |
| Visitante nuevo | Bienvenida → espacios o servicios → contenido público o búsqueda | Comprende la oferta y llega a un contenido pertinente sin cuenta. Es éxito de descubrimiento; no se cuenta como activación de fan o artista. |
| Fan | Búsqueda o contenido → artista público → autenticación contextual si hace falta → seguir | Seguimiento confirmado por el servidor, presente tras recarga y asociado a la cuenta correcta; no basta el clic ni un cambio optimista. |
| Artista individual | Conocer posibilidades → registro mínimo → perfil propio → guardar → abrir perfil público | Perfil mínimamente útil persistido, recuperable y descubrible conforme a sus reglas de visibilidad. No inventar un botón «Publicar» si el producto publica mediante guardado. |
| Banda o proyecto | Explorar → crear o reclamar identidad autorizada → completar perfil → comprobar público | Perfil del proyecto correcto sin duplicación ni atribución indebida; reclamaciones respetan sus permisos y revisiones. |
| Organizador | Explorar eventos → acceder con permisos → crear → publicar → descubrir evento | Evento persistido y público bajo las reglas vigentes. Borrador, RSVP y asistencia son estados diferentes. |
| Cliente | Servicio pertinente → disponibilidad/formulario público → solicitud o reserva → recuperación | Solicitud o reserva identificable y persistida; distinguir solicitud recibida, confirmación y pago. No convertir navegación en una reserva confirmada. |
| Estudiante | Oferta formativa → información real → inscripción → confirmación | Inscripción persistida con estado verificable. Solicitud, plaza confirmada y pago completado se miden por separado. |
| Administrador | Acceso autorizado → cola adecuada → revisión explícita → resultado auditable | Tarea administrativa confirmada y persistida. La bienvenida no aprueba solicitudes ni cambia roles automáticamente. |

Los recorridos de autenticación conservan destino seguro e intención. El acceso
a contenido público no implica consentimiento de marketing, participación en
campañas ni autorización para contactar a terceros.

## Contrato del candidato de bienvenida

Comportamiento previsto:

1. En `/inicio`, presentar una bienvenida humana antes del buscador existente.
   Explicar TDF mediante posibilidades concretas y enlaces públicos funcionales.
2. Conservar `/buscar` como búsqueda directa. Mantener filtros, consulta,
   historial y enlaces profundos de ambas rutas sin pérdida de contexto.
3. Expresar filtros como propiedades del resultado buscado, diferenciadas de
   «tus especialidades» en edición de perfil.
4. En `/tdf`, priorizar exploración y servicios antes de invitaciones de cuenta.
5. Evaluar el video existente `music-hero.mp4` y su procedencia. Solo si la
   inspección confirma su pertinencia, ofrecer reproducción voluntaria y un
   póster extraído del propio video. No reproducir automáticamente ni presentar
   una imagen generada o ajena como un espacio real de TDF.
6. Mantener un recorrido útil si los medios o feeds fallan. Los enlaces no
   dependen de responder preguntas personales.

Invariantes:

- **Exploración pública:** leer la bienvenida, conocer la propuesta y buscar
  no exige crear una cuenta, elegir identidad ni enviar datos personales.
- **Destino:** `/buscar` sigue siendo una herramienta de búsqueda; una visita
  con filtros no descarta sus valores para empezar un tour.
- **Sesión:** el destino actual de usuarios autenticados y el bootstrap seguro
  no se alteran; este incremento no introduce un mecanismo de sesión paralelo.
- **Verdad del contenido:** servicios, personas, resultados y testimonios solo
  se muestran con respaldo. Un enlace a ubicación no equivale a un plano interior.
- **Medios voluntarios:** no hay autoplay ni audio inesperado. Controles de
  reproducción, foco, texto alternativo y carga móvil deben verificarse.
- **Recuperación:** un fallo de imágenes o contenido remoto no oculta los CTA
  ni causa una pantalla vacía o un mensaje falso de éxito.
- **Sin efectos de negocio:** explorar o reproducir un video no crea perfiles,
  contactos, reservas, seguimientos ni consentimientos.
- **Observabilidad mínima:** cualquier evento nuevo utiliza propiedades
  acotadas y el cliente analítico existente; no envía texto libre de búsqueda,
  información personal ni credenciales.

El candidato pasa de `bienvenida visible` a `exploración elegida`, `búsqueda` o
`contenido público`. Solo una acción posterior que requiera autoridad conduce a
`autenticación contextual`; sus estados de carga, error, recuperación y retorno
pertenecen a los contratos existentes. Ninguna transición se acredita como
completada exclusivamente por una animación, un clic o una prueba de componente.

## Trazabilidad de los ocho audios

| Nota de voz | Comentario | Respuesta prevista o pendiente |
| --- | --- | --- |
| 16:39:53 | Entrada fría, sin bienvenida ni orientación; pregunta antes de explicar. | Bienvenida en `/inicio` y acciones concretas de exploración. La comprensión y la sensación de acogida requieren validación humana. |
| 16:41:32 | Sensación de intromisión en la vida personal. | Exploración pública sin captura de identidad; ubicación solo con permiso y filtros expresados como búsqueda. Revisar solicitudes de datos posteriores en su contexto. |
| 16:41:58 | Parece una encuesta; primero deberían mostrarse las posibilidades de TDF. | Propuesta y servicios antes de cuenta; conservar búsqueda como herramienta opcional y accesible. |
| 16:42:25 | Mostrar espacios y equipos; posible mapa o recorrido divertido. | Medio real existente si se valida su pertinencia. Fotografías específicas, plano interior y VR pendientes de recursos y evaluación. |
| 16:42:48 | Construir confianza gradualmente; difícil elegir una identidad musical. | No exigir rol para explorar; personalización posterior opcional. No modificar automáticamente permisos ni identidades. |
| 16:43:03 | Ambigüedad entre buscar a un músico y decir qué eres. | Etiquetas de búsqueda explícitas en ambos idiomas disponibles; conservar el lenguaje de identidad únicamente en el perfil. |
| 16:44:57 | Adelantar foto del anfitrión, bienvenida y capacidad de asesorar. | Copy cercano y servicios visibles. Foto de Diego sin identificar: pendiente, sin sustitución engañosa. |
| 16:46:43 | Demostrar experiencia usando equipos y explicando procedimientos. | Enlaces al trabajo real disponible. Videos didácticos nuevos y material profesional verificable pendientes; no inventar experiencia ni resultados. |

## Verificación requerida y prueba humana propuesta

El ejecutor debe registrar SHA, comandos, recibos y límites después de correr
las verificaciones: regresiones de `/inicio` frente a `/buscar`, conservación de
consulta/filtros e historial, enlaces de `/tdf`, medios sin autoplay, estados de
fallo, navegación por teclado y ausencia de desbordamiento en escritorio y móvil.
Comprobar el recorrido real de destino además de la presencia de un enlace.
Las pruebas con fixtures y API sintética no demuestran persistencia productiva.

Protocolo humano **propuesto, todavía no ejecutado**:

1. Reclutar voluntarios representativos mediante canales autorizados y con
   consentimiento; este documento no autoriza contactar ni crear participantes.
2. Mostrar la entrada durante un intervalo breve y preguntar, sin sugerir la
   respuesta: qué ofrece TDF, para quién sirve y qué harían a continuación.
3. Solicitar encontrar un servicio, conocer el estudio y localizar un artista.
   Observar si creen que se les pide declarar su profesión o entregar datos.
4. Registrar finalización, ayuda necesaria, errores, tiempo de tarea y palabras
   del participante sin datos innecesarios. Separar comprensión de preferencia
   estética y de activación persistida.
5. Comparar con la línea base únicamente con tareas y condiciones equivalentes.
   Informar tamaño de muestra, dispositivos, fallos y limitaciones. No presentar
   una comparación exploratoria como causalidad demostrada.

Participantes observados por este documento: **0**. No existe un resultado de
usabilidad ni un aumento de conversión atribuible al candidato. El responsable
de la entrega completará de forma separada los hitos implementado, probado,
integrado, desplegado y verificado en producción; las dependencias P0 y los
recursos de contenido pendientes permanecen explícitos.


## Implementación del primer incremento

`/inicio` conserva su búsqueda y parámetros y añade una bienvenida previa; `/buscar` permanece directo. Las variantes `/inicio/` y `/Inicio` muestran la misma bienvenida. Los CTA de exploración enlazan destinos públicos existentes. `/tdf` adelanta estudio y servicios, conserva el registro contextual y sustituye el video del Domo por una fotografía ya publicada en la sección de formación, sin atribuir la identidad del anfitrión. No se utiliza una portada editorial arbitraria como foto del estudio. La imagen pesa aproximadamente 57 KiB y no añade reproducción automática.

Los eventos nuevos `public_discovery_cta_clicked` contienen parámetros de ruta y destino fijos. No incluyen búsqueda, identidad o coordenadas, respetan el cliente de analítica existente y su fallo no interrumpe la navegación. Estos clics expresan interés, no activación. El backend, sus permisos, el contrato API, las migraciones, las dependencias y el pin Mobile permanecen sin cambios.
