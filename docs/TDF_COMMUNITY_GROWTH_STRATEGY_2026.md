# Estrategia tecnológica y de crecimiento de la comunidad TDF

**Fecha:** 7 de agosto de 2026

**Horizonte operativo:** 90 días

**Mercado inicial:** Quito como núcleo de densidad; Ecuador como alcance digital

**Marca principal:** TDF Records

**Concepto de campaña:** **Tu escena, conectada.**

**Llamado principal:** **Crea tu perfil. Sigue a tu artista. Activa tu escena.**

## 1. Decisión estratégica

TDF no debe salir a comprar “la mayor cantidad posible” de registros todavía. Debe salir cuando pueda medir y producir una relación real entre dos lados de la red: artista y fan.

La unidad de valor no será el usuario registrado, sino la **relación activada**. Un usuario queda activado cuando completa su perfil y realiza al menos una acción significativa:

- Fan: sigue a un artista, entra a un club, reacciona, dona, reserva o participa.
- Artista/banda/DJ: completa su perfil público y publica un release, convocatoria, evento, contenido o enlace compartible.
- Profesional musical: completa portafolio y publica una oferta, disponibilidad o necesidad.
- Promotor/venue/estudio: publica una oportunidad o servicio y recibe una interacción válida.

La métrica norte será **Relaciones Activadas Semanales (RAS)**: número de relaciones nuevas y verificables creadas cada semana entre dos miembros de la comunidad. La sigla coincide de forma útil con *Registrar, Accionar y Sostener*:

1. Registrar: cuenta creada con atribución conocida.
2. Accionar: perfil completo + primera acción.
3. Sostener: regreso o segunda acción dentro de siete días.

“Millones de usuarios” es una visión de largo plazo. La prueba de éxito de los primeros 90 días será demostrar que TDF puede repetir este ciclo con retención y sin depender de pauta creciente.

## 2. Diagnóstico del producto actual

La auditoría del repositorio confirma que el producto está más avanzado que una landing de captación.

| Capacidad | Estado observado | Implicación de crecimiento |
| --- | --- | --- |
| Registro por correo y contraseña | Implementado | Puede medirse y optimizarse ya |
| Ingreso con Google | Implementado | Reduce fricción; Apple/TikTok/Spotify no están implementados |
| Roles de Fan, Artista y otros perfiles | Implementado | Permite onboarding por recorrido |
| Selección de artistas durante registro fan | Implementado | Puede generar la primera relación inmediatamente |
| Perfil fan editable | Implementado | Falta un porcentaje/completitud unificado |
| Perfil público de artista y reclamación | Implementado | Excelente activo compartible para adquisición orgánica |
| Bio, géneros, ciudad, portada, Spotify, YouTube, web y video | Implementado | Portafolio básico disponible |
| Releases y reproductores/enlaces | Implementado | Sirve como contenido de retorno |
| Seguir/dejar de seguir artistas | Implementado | Acción principal de activación fan |
| Clubes de fans, posts, eventos, elecciones, recuerdos y reacciones | Implementado | Profundidad comunitaria real, aún compleja para el primer uso |
| Mensajería y notificaciones | Modelos y flujos implementados | Deben usarse después de activar, no en el registro |
| Donaciones/propinas a artistas | Endpoint Stripe y UI existentes | Beneficio inmediato sujeto a configuración Stripe del artista |
| Reservas, cursos, live sessions y marketplace | Implementado | Puente directo entre comunidad e ingresos TDF |
| PostHog web y móvil | Integrado con modo seguro/no-op | Requiere clave de producción y tablero de crecimiento |
| Referidos | Existe un sistema separado para Academy | No sirve todavía como referidos generales de comunidad |
| STC, wallet y ledger de recompensas | No observados como sistema general de producción | Debe implementarse antes de prometer saldos |
| Match entre músicos y bolsa de oportunidades | No existe como recorrido completo | Prioridad posterior al onboarding y referidos |
| Meta Pixel, TikTok Pixel y Conversions API | No observados | PostHog será la fuente de verdad inicial; publicidad se conecta después con consentimiento |

### Fricción principal

La plataforma ofrece demasiadas posibilidades para un usuario nuevo. La primera experiencia debe reducirse a una promesa y una acción por tipo de usuario:

- Fan: **“Sigue a tu artista y recibe acceso directo.”**
- Artista: **“Crea tu perfil público y conoce a tu comunidad.”**
- Músico/profesional: **“Publica qué haces o qué necesitas.”**

## 3. Mercado de lanzamiento

### Recomendación

Usar una estrategia de dos capas:

- **Densidad:** Quito y sus escenas de electrónica, ska, punk, hip-hop, urbano y música tradicional/popular.
- **Alcance:** todo Ecuador para contenido orgánico y registro, sin prometer todavía una oferta local completa en cada ciudad.

No lanzar simultáneamente a toda Latinoamérica. Con USD 100 de pauta y una sola persona operando dos horas diarias, dispersar la red produciría perfiles sin relaciones. La expansión debe habilitarse cuando una ciudad tenga al menos 25 artistas/profesionales activados, 150 fans activados y actividad semanal sostenida.

TikTok, Instagram, YouTube y Facebook tienen alcance suficiente en Ecuador; DataReportal reporta, con datos publicitarios de finales de 2025, 15,4 millones de identidades sociales, 7 millones de alcance potencial en Instagram, 11,5 millones en YouTube y 12,5 millones en Facebook. Las cifras de TikTok deben interpretarse con cautela porque su alcance publicitario reportado puede superar la población adulta estimada.

### Secuencia de públicos

El orden declarado por TDF pone a fans primero, pero una red de fans necesita artistas con contenido. Por eso la secuencia operativa será:

1. **Cohorte semilla privada:** 10–15 artistas/bandas/DJs, comenzando por Arkabuz, Skanka-Fe, Llama Este Pez y aliados.
2. **Fans de esos artistas:** cada artista comparte su perfil y una recompensa/experiencia propia.
3. **Músicos y profesionales:** se incorporan mediante “Busco músico”, colaboraciones y servicios.
4. **Estudios, venues, promotores, sellos y managers:** entran cuando ya existe demanda visible.

## 4. Propuesta de valor

### Promesa central

**TDF conecta artistas, fans y profesionales de la música en un solo lugar para convertir audiencia en comunidad, comunidad en oportunidades y oportunidades en ingresos.**

### Beneficio inmediato por segmento

| Segmento | Beneficio del día 1 | Primera acción |
| --- | --- | --- |
| Fan | Seguir artistas, entrar a clubes y descubrir contenido/eventos | Seguir al menos un artista |
| Artista/banda/DJ | Perfil público, enlaces, seguidores y propinas | Completar y compartir perfil |
| Músico de sesión/productor/ingeniero | Visibilidad y futuras oportunidades de colaboración | Publicar disponibilidad/oferta |
| Creador visual | Portafolio frente a artistas y eventos | Publicar servicio o muestra |
| Estudio/venue/promotor | Acceso a comunidad y demanda musical | Publicar servicio/oportunidad |

### Oferta fundacional

**Primeros 500: Miembros Fundadores TDF**

Beneficios recomendados:

- insignia permanente “Fundador/a 500”;
- acceso anticipado a funciones y convocatorias;
- prioridad, no garantía, en sorteos, pruebas y oportunidades;
- asignación STC según una curva con presupuesto fijo;
- un sorteo mensual entre fundadores activados, no simplemente registrados.

No prometer descuentos permanentes ilimitados ni prioridad automática en bookings; esos beneficios pueden crear pasivos difíciles de sostener.

## 5. Economía STC

### Corrección matemática

La fórmula `100000 / n` no reparte un total de 100.000 STC. El primer usuario recibiría 100.000, el segundo 50.000 y los primeros 500 sumarían aproximadamente 679.282 STC.

Para repartir un fondo cerrado de 100.000 STC entre 500 fundadores y conservar la curva inversa, usar:

`recompensa(n) = floor(100000 / (H500 × n))`

donde `H500 ≈ 6,792823` es el número armónico 500. Aproximadamente:

| Orden | STC aproximados |
| ---: | ---: |
| 1 | 14.721 |
| 2 | 7.360 |
| 3 | 4.907 |
| 10 | 1.472 |
| 100 | 147 |
| 500 | 29 |

El pequeño residuo por redondeo se asigna al tesoro de recompensas. Esta fórmula debe vivir en backend con pruebas deterministas; el cliente nunca calcula ni acredita saldos.

### Reglas de seguridad

- El premio fundador se reserva al registrarse, pero se acredita al activarse.
- Un referido se valida tras perfil completo + primera acción + regreso D7.
- No se premian autorreferidos, cuentas con teléfono/correo duplicado, ciclos de referidos ni actividad anulada.
- Límite de recompensa por persona y por dispositivo en ventanas temporales.
- Ledger inmutable de débitos/créditos; los saldos no se editan directamente.
- Revisión manual para patrones anómalos y premios grandes.
- Hasta revisión legal y contable, presentar STC como puntos internos no transferibles, sin valor en efectivo y sin lenguaje de inversión, rentabilidad o token negociable.

### Referidos recomendados

- Invitado: 50 STC al activarse.
- Invitador: 50 STC cuando el invitado se activa + 200 STC si regresa en D7.
- Máximo inicial: 20 referidos premiados por miembro al mes.
- Embajadores verificados: reglas y presupuesto separados.

El fondo de fundadores y el fondo de referidos deben ser dos presupuestos distintos.

## 6. Embudo y eventos

### Embudo canónico

`visita → CTA → inicio de registro → registro → perfil → primera acción → retorno D7 → compra/donación`

### Eventos mínimos de PostHog

| Etapa | Evento |
| --- | --- |
| Visita | `acquisition_landing_viewed` |
| Interés | `acquisition_cta_clicked` |
| Registro iniciado | `signup_started` |
| Roles | `signup_roles_selected` |
| Envío | `signup_submitted` |
| Registro | `signup_completed` |
| Perfil fan | `fan_profile_saved` |
| Perfil artista | `artist_profile_saved` |
| Primera relación | `artist_followed` con `is_first_follow=true` |
| Sostenimiento | retorno/segunda acción dentro de siete días |
| Monetización | propina, reserva, ticket, curso u orden pagada |

Todos los eventos de crecimiento deben incluir, cuando existan: `attribution_source`, `attribution_medium`, `attribution_campaign`, `attribution_content`, `attribution_term`, `referral_code` y `attribution_landing_path`. Nunca deben incluir correo o teléfono.

### Tablero ejecutivo

1. Visitantes por fuente/campaña.
2. Conversión visita → inicio de registro.
3. Conversión inicio → registro.
4. Registro → activación en 24 horas.
5. D1, D7 y D30 por cohorte.
6. RAS por semana y por segmento.
7. Coeficiente viral: invitaciones × conversión × activación.
8. CAC por usuario activado, no por registro.
9. Ingreso, donaciones y reservas por cohorte/fuente.

## 7. Objetivos

La progresión mínima propuesta convierte “5 diarios y subiendo” en metas verificables:

| Horizonte | Ritmo | Registros acumulados | Activados acumulados | Condición cualitativa |
| --- | ---: | ---: | ---: | --- |
| 30 días | 5/día | 150 | 60–75 | 10–15 artistas semilla y embudo medido |
| 90 días | 10/día desde día 31 | 750 | 375–450 | D7 ≥ 25% y RAS creciendo 10% semanal |
| 180 días | 20/día desde día 91 | 2.550 | 1.500+ | Una segunda ciudad lista para expansión |

Las metas se recalibran tras dos semanas de datos. No se debe escalar pauta si la activación es menor al 35% o la retención D7 menor al 20%.

## 8. Plan tecnológico

### Fase 0 — Instrumentación y cohorte semilla, días 0–14

- desplegar la atribución y eventos de crecimiento ya preparados;
- configurar la clave PostHog de producción y crear el tablero;
- registrar métricas base reales de usuarios, actividad y conversiones;
- simplificar el onboarding a una acción por rol;
- incorporar 10–15 artistas semilla con perfiles, contenido y enlaces completos;
- fijar SLA de respuesta: menos de 24 horas, objetivo de 4 horas en horario operativo;
- diseñar términos de Miembros Fundadores y STC antes de comunicar saldos.

**Criterio de salida:** todos los pasos del embudo se ven por fuente y al menos 20 usuarios de prueba completan recorridos sin asistencia.

### Fase 1 — Fundadores, referidos y activación, días 15–35

- tablas generales `GrowthReferralCode`, `GrowthReferralClaim`, `StcAccount` y `StcLedgerEntry`;
- código único por usuario y deep link con `ref`;
- acreditación transaccional e idempotente tras activación;
- indicador de completitud y checklist por rol;
- tarjeta/URL pública compartible;
- insignia Fundador 500;
- panel de soporte/fraude y exportación del ledger.

**Criterio de salida:** una cuenta invitada puede registrarse, activarse, regresar y generar recompensas auditables sin doble crédito.

### Fase 2 — Oportunidades y loops, días 36–60

- publicación “Busco / Ofrezco” con ciudad, género, instrumento/servicio, fecha y compensación;
- tarjetas compartibles para Instagram Stories, WhatsApp y TikTok;
- match inicial por reglas explicables, no por IA opaca;
- reto semanal y convocatoria con resultado publicado;
- notificaciones in-app/correo; WhatsApp/SMS solo con consentimiento explícito.

### Fase 3 — Monetización y expansión, días 61–90

- experimentos premium: perfil destacado, herramientas avanzadas, membresías y comisiones;
- retargeting Meta y, cuando el presupuesto lo permita, Conversions API con deduplicación;
- programa de embajadores por universidad/escena;
- evaluación de Guayaquil, Cuenca y primera ciudad colombiana por señales de demanda.

## 9. Campaña “Tu escena, conectada”

### Sistema de contenido

Con capacidad de dos videos semanales, cada video se publica como Reel, TikTok, Short y Facebook Reel. TikTok recomienda creatividad nativa, vertical, auténtica y con sonido; Instagram también prioriza video vertical 9:16 y audio para Reels. No crear cuatro piezas distintas: adaptar portada, caption y CTA.

Pilares:

1. **Utilidad compartible:** oportunidades, “busco músico”, retos, convocatorias.
2. **Prueba humana:** perfiles, historias, backstage y resultados.

Formato constante: gancho en 0–2 s, problema en 3–7 s, demostración en 8–22 s, CTA en 23–30 s. Subtítulos siempre. Diego aparece en cámara para dar confianza; la edición puede usar sesiones, Domo, estudio y archivo existente.

### Calendario editorial de 13 semanas

| Semana | Video A | Video B | Acción dentro de TDF |
| ---: | --- | --- | --- |
| 1 | “¿Dónde está la escena de Quito?” | Demo: crear perfil fan en 30 s | Registrarse y seguir artista |
| 2 | Perfil de Llama Este Pez | “3 cosas que todo artista debería saber de sus fans” | Completar perfil artista |
| 3 | Lanzamiento Fundadores 500 | Backstage de TDF Estudio | Activarse para reservar STC |
| 4 | “Busco bajista/baterista/productor” | Perfil de Skanka-Fe | Publicar necesidad piloto |
| 5 | Cómo recibir propinas directas | Historia/caso Arkabuz | Configurar perfil y links |
| 6 | Reto: colaboración en 7 días | Dúo/reacción a participantes | Participar e invitar |
| 7 | Oportunidad TDF Sessions | Cómo mejorar un EPK/perfil | Completar checklist |
| 8 | “Fan no es follower” | Beneficio exclusivo de artista semilla | Entrar a club |
| 9 | Domo: oportunidad de presentación | Convocatoria electrónica | Registrarse a evento |
| 10 | Top perfiles de la semana | “¿Qué músico necesita tu proyecto?” | Compartir tarjeta |
| 11 | Caso de colaboración lograda | Tutorial de referido | Invitar y activar |
| 12 | Preguntas incómodas de la industria | Respuesta/comentarios de comunidad | Comentar o publicar |
| 13 | Resultados transparentes de 90 días | Próxima ciudad/convocatoria | Votar y regresar |

### Guion base 1 — lanzamiento

**Gancho:** “Quito tiene músicos increíbles. El problema es que estamos todos desconectados.”

**Cuerpo:** “Por eso estamos abriendo TDF: artistas, fans, productores, músicos y espacios en una sola comunidad. Creas tu perfil, sigues a quienes de verdad escuchas y encuentras oportunidades sin depender del algoritmo de una red ajena.”

**Prueba visual:** pantalla del perfil + clips de estudio, sesión y Domo.

**CTA:** “Entra a tdf-app.pages.dev, crea tu perfil y activa tu escena. Los primeros 500 serán miembros fundadores.”

### Guion base 2 — fans

**Gancho:** “Seguir a un artista no debería significar perderse el 90% de lo que publica.”

**Cuerpo:** “En TDF puedes seguirlo desde un perfil directo, entrar a su comunidad, ver releases y acceder a experiencias sin que un algoritmo decida por ti.”

**CTA:** “Crea tu perfil fan y sigue hoy a una banda ecuatoriana.”

### Guion base 3 — artistas

**Gancho:** “Tienes miles de views, ¿pero sabes quién volvería a verte, comprar o colaborar?”

**Cuerpo:** “TDF convierte seguidores dispersos en una comunidad identificable: perfil público, fans, releases, eventos, propinas y herramientas del sello en un solo lugar.”

**CTA:** “Crea o reclama tu perfil y comparte tu enlace.”

### Guion base 4 — busco músico

**Gancho:** “Busco baterista para un proyecto en Quito. Y no quiero preguntarle al algoritmo.”

**Cuerpo:** “Publica qué necesitas, género, ciudad, fecha y si es pagado o colaboración. La comunidad correcta puede compartirlo y responder.”

**CTA:** “Regístrate en TDF y publica tu búsqueda.”

### Copys breves

**Lanzamiento:**

La escena ya existe. Lo que faltaba era conectarla. Crea tu perfil, sigue artistas ecuatorianos y encuentra personas, experiencias y oportunidades dentro de TDF. Primeros 500: Miembros Fundadores. #TDFRecords #MusicaEcuador #TuEscenaConectada

**Artistas:**

Tu audiencia no es una cifra. Convierte seguidores en comunidad con un perfil público, releases, fans, experiencias y apoyo directo. Crea o reclama tu perfil en TDF.

**Fans:**

No mires la escena desde afuera. Sigue artistas, entra a sus comunidades y participa en lo que viene. Tu escena también te pertenece.

### Distribución semanal

- Martes: video de utilidad o convocatoria.
- Viernes: historia, caso o demostración.
- Historias de bajo esfuerzo: 3–5 por semana con encuesta, repost, progreso y CTA.
- Domingo: resumen de oportunidades por carrusel o historia; no requiere un tercer video.
- Cada pieza usa un enlace UTM distinto y, para aliados, un código `ref` distinto.

## 10. Pauta y presupuesto

No gastar pauta durante los primeros 14 días. Primero comprobar registro y activación con la cohorte semilla.

### USD 100 mensuales

| Uso | Monto | Regla |
| --- | ---: | --- |
| Meta prospecting, Quito/Ecuador | 55 | Solo la mejor pieza orgánica; objetivo registro/landing view inicialmente |
| Meta retargeting | 30 | Visitó o inició registro y no se activó |
| Fondo de prueba | 15 | Variación de gancho/CTA; se pausa si no mejora activación |

TikTok se usa orgánicamente al inicio. Con USD 100 totales, dividir demasiado el presupuesto impide aprender. Solo mover presupuesto a TikTok Ads cuando un video orgánico muestre tracción y los mínimos vigentes de la cuenta permitan una prueba útil.

### USD 50 adicionales

- USD 25: premio de experiencia (hora de estudio, entrada o mentoría), preferiblemente con costo marginal bajo para TDF.
- USD 15: apoyo de edición/subtítulos/plantillas.
- USD 10: contingencia o impresión de QR para estudio, eventos y aliados.

## 11. Programa de embajadores

Piloto con 10 embajadores: Escuela de Músicos, escenas universitarias, Verde70, SpaceTrip Fest, Letal, Bou y contactos de venues/medios.

Cada embajador recibe:

- URL/código único;
- kit de tres Stories y un video colaborativo;
- tablero de registros activados, no solo clics;
- STC y acceso a experiencias según activaciones D7;
- reconocimiento público mensual.

No pagar por registro bruto. Premiar 40% por activación y 60% por sostenimiento D7 reduce fraude y perfiles vacíos.

## 12. Experimentos A/B

| Prioridad | Hipótesis | Variante A | Variante B | Éxito |
| ---: | --- | --- | --- | --- |
| 1 | Una promesa específica convierte mejor | “Únete a TDF” | “Sigue a tu artista y recibe acceso directo” | Activación/visita |
| 2 | Elegir artista en registro aumenta D7 | Opcional posterior | Selección durante registro | D7 fan |
| 3 | Fundadores activa urgencia útil | Sin oferta | Fundador 500 + STC al activarse | Activación, fraude |
| 4 | Prueba social mejora artistas | Beneficios genéricos | Caso Arkabuz/Skanka/Llama Este Pez | Perfil completo |
| 5 | Un CTA domina mejor | Tres CTAs | CTA adaptado por campaña | Inicio de registro |
| 6 | Compartir después de completar crea loop | Sin prompt | Tarjeta pública al 80% | Invitaciones/activado |

Cada experimento dura hasta alcanzar muestra suficiente o dos semanas; no cambiar varias piezas del embudo a la vez.

## 13. Operación diaria

Dos horas diarias:

- 30 min: responder mensajes y onboarding; SLA máximo 24 h. Quince días es incompatible con conversión comunitaria.
- 30 min: revisar embudo, errores y nuevos activados.
- 30 min: comentar, conectar miembros y moderar oportunidades.
- 30 min: preparar/editar/distribuir contenido.

Dos bloques semanales más largos pueden reemplazar parte del trabajo diario: grabar ambos videos en una sesión y programar publicaciones.

## 14. Privacidad, edades y confianza

Para el piloto, recomendar 18+. La plataforma maneja comunidad, mensajería, recompensas y pagos; incorporar adolescentes requiere consentimiento verificable, controles de contacto, moderación, reportes, privacidad por defecto y términos específicos. No se debe anunciar una campaña para menores hasta implementar esos controles.

Correo, WhatsApp, push y SMS requieren consentimiento por canal, baja fácil y frecuencia limitada. PostHog seguirá sin grabación de sesión por defecto y sin PII en eventos. Meta/TikTok se conectan solo tras política de consentimiento y mapeo de datos.

## 15. Información que no puede inferirse del código

- número real de usuarios totales y activos en producción;
- tasa actual de finalización del registro;
- clave/estado real de PostHog en producción;
- seguidores actuales por red;
- cinco publicaciones con mayor alcance, retención, compartidos y clics;
- audiencia y conversiones históricas de Instagram/TikTok.

Para cerrar la línea base se necesita exportar Insights de Instagram/TikTok/YouTube/Facebook de los últimos 90 días y consultar producción/PostHog con acceso administrativo. Las páginas públicas no exponen estas métricas de forma fiable y no deben estimarse.

## 16. Referentes, no clones

- BandLab: creación y colaboración musical.
- Vampr: descubrimiento y networking profesional.
- Patreon: relación directa y membresía.
- Discord/WhatsApp: conversación y coordinación.
- Link-in-bio/EPK: identidad pública compartible.

La ventaja de TDF no será copiar cada función, sino unir comunidad digital con activos reales: estudio, sello, escuela, TDF Sessions, Domo y escenas locales.

## 17. Fuentes de mercado y plataforma

- DataReportal, *Digital 2026: Ecuador*: https://datareportal.com/reports/digital-2026-ecuador
- Instagram for Business, Reels: https://business.instagram.com/instagram-reels
- TikTok for Business, Creative Codes: https://ads.tiktok.com/business/en-US/creative-codes
- Meta, Conversions API: https://www.facebook.com/business/help/AboutConversionsAPI
- Meta, deduplicación Pixel/CAPI: https://www.facebook.com/business/help/823677331451951
