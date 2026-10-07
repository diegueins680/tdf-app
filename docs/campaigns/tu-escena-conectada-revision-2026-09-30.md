# Tu escena, conectada — lote para aprobación, 1 de octubre de 2026

**Lote TDF-TEC-20261001-01 · 20 destinatarios · A10/B10 aprobados · en ejecución.** Estado al 3/oct 17:01 Ecuador: 15 contactos con envío confirmado sin fallo observado (A8/B7); un intento no entregado a Hugo (fan B), excluido sin reintento; cuatro aprobados pendientes (#16,18,19,20). Último bloque cerrado con cinco invitaciones. Hoy: ocho intentos de invitación (incluido Hugo) y dos respuestas de ayuda; diez salidas en total, cierre conservador de envíos por hoy. Cuatro respuestas escritas de tono positivo acumuladas, una neutral, sin activación verificada. Todos los enlaces vigentes usan www.tdfrecords.net. El Reel ya está publicado, no volver a publicar: https://www.instagram.com/tdf.records.label/reel/Dd71zzlt8TD/ . Documento de revisión e informe; el CRM conserva la gestión operativa.

**Horario vigente desde el 3/oct/2026:** todos los días, 08:00–22:00 `America/Guayaquil`, por instrucción «Empieza automáticamente a las 8am, siempre». Permanecen máximo 10 invitaciones/día y 5 mensajes/bloque, exclusivamente dentro de lotes aprobados. **Automatización pendiente de activación:** esta sesión no tiene una herramienta de programación de tareas del chat; guardar el horario no programa una ejecución. Las referencias anteriores a 10:00 son históricas.

**Automatización vigente (4/oct 11:31 Ecuador): ACTIVADA en ChatGPT Scheduled, diariamente 08:00 America/Guayaquil, sin fecha de fin. Próxima ejecución confirmada 5/oct/2026 08:00.** [Abrir tarea](https://chatgpt.com/scheduled?automationId=6ac27f594a088190980ae08e3ae4fd69&automationSource=cloud). Cloud: acceso al navegador adjunto y CRM pendiente de verificar durante ejecución; si falta, detener e informar. Las notas anteriores de automatización pendiente son históricas.

**Estado vigente 4/oct/2026 11:53 Ecuador:** 19 contactos con envío visible confirmado sin fallo observado (A10/B9), un intento fallido excluido (Hugo B), **cero invitaciones pendientes**. Hoy cuatro invitaciones en un bloque (artista A2, artista B1, musico B1), CRM verificado. Programación diaria08:00 incluye domingos. Prueba Run now cloud detenida por falta de navegador; operación local de hoy completada. No volver a enviar las cuatro invitaciones.

**Revisión vigente4/oct19:48:**19hilosrevisados;5positivas(A2/B3),1neutral, sinactivaciónconfirmada. Francisco recibiórespuestaayuda19:42; Carlagracias08:20. CRM276,19matchesfalse. Hoy5salidas(4invitaciones+1ayuda). Dosseguimientospropuestos5–7octpendientesaprobación; ningúnnuevoloteautorizado.

**Aprobación vigente4/oct19:50:** dos seguimientos exactos a Joel y Quito Ska Society autorizados para5–7oct, una sola vez cadauno; todavía NO enviados. No pedir otra aprobación de estos textos.

## Resultado de la auditoría del producto

- Código remoto actual comprobado: [49f1f0ec](https://github.com/diegueins680/tdf-app/commit/49f1f0ec087067d17f62d91df1b616eb053eb894). Comparado con 645f56f, conserva sin cambios loginRouting.ts, LoginPage.tsx, ArtistPublicPage.tsx y FanHubPage.tsx ya auditados. El checkout local contiene trabajo ajeno y no se usó como prueba de lo desplegado; no se modificó código del producto.
- Producción pública comprobada en https://www.tdfrecords.net. `/fans` muestra55 artistas, perfiles, enlaces musicales y botones Seguir; puede explorarse sin sesión. `/records` mostró sesiones y lanzamientos reales (33 grabaciones y67 releases en la revisión del 30/sept). Estas cifras describen el catálogo, no resultados de campaña.
- Registro: formularios con nombre, correo, contraseña y aceptación de términos listos. En `/login` están visibles «Continuar con Google» y «Crear cuenta con Google». No se creó una cuenta de prueba ni se completó OAuth: se verificaron las entradas al registro, no el alta de extremo a extremo.
- Los 11 enlaces distintos utilizados en los 20 mensajes abren Crear cuenta y llegan al botón «Crear e ingresar», conservando sus parámetros. Diez recorridos A/B comprobados a~01:41UTC y la variante Producer de Joel comprobada después de recuperar la sesión. No contienen datos personales ni códigos de referido.
- Artistas: `intent=artist_profile&roles=Artista&redirect=/mi-artista`; fans: `intent=follow_artists&roles=Fan&redirect=/fans`; DJs: intención profesional con `roles=DJ`; Joel: intención profesional con `roles=Producer`; otros músicos/aliados: registro general y `/fans`.
- **Limitación del producto:** el código convierte estos parámetros en intenciones; no asigna automáticamente un rol o permiso. La ruta artista puede continuar en `/artista/crear` si falta acceso a `/mi-artista`. No se probó la redirección posterior a un alta nueva. Se presenta esta limitación en vez de prometer una preselección de permisos que producción no ofrece.
- El perfil público observado muestra biografía, enlaces y área de lanzamientos, pero el perfil Diego Saa no tenía biografía ni releases publicados; no se promete contenido suyo. El club 9 no está activo: no se usa como primera acción. `/reservar` mostró un formulario público y acceso para invitados; no se realizó reserva ni pago. Las invitaciones no prometen disponibilidad de cursos, marketplace, propinas, exclusivas ni experiencias concretas.
- PostHog: funcionamiento y clics atribuibles no comprobados. UTM disponible no equivale a medición verificada.

## Selección y comprobación del CRM

Lectura completa autenticada en **CRM → Contactos**: HTTP200, **254 contactos**, límite 500 sin truncamiento, 1/oct ~01:53 UTC. Se normalizó Instagram quitando @, espacios y mayúsculas. Dos coincidencias exactas: elbloque_oficial y lysergicman_music, ambas `hasUserAccount=false`, notas vacías. Los otros 18 no tienen coincidencia exacta. No se excluyó a nadie por parecido de nombre ni se creó un contacto anticipadamente.

La revisión pública de perfiles e historiales se realizó el 30/sept; los detalles relativos («hace seis días», etc.) describen ese momento. Se encontraron seguidores actuales, interacciones recientes y proyectos con vínculo verificable con Quito. TDF siguiendo a una cuenta no se considera prueba de seguimiento recíproco. No se encontró invitación previa en los historiales visibles ni en la búsqueda exacta de estos 20 usuarios en los documentos de campaña.

No hay indicios observados de minoría de edad en los seleccionados; esto no equivale a verificar documentalmente su edad. No se pedirán documentos. Cualquier duda nueva de edad, identidad, pertinencia o antecedentes obliga a excluir antes de enviar.

Puntos en orden: consulta/DM significativo(máx. 3), interacción reciente(2), profesión musical(2), Quito(2), actividad últimos 60 días(1), escena afín(1). El seguimiento reciente cuenta como interacción; un seguimiento antiguo no da puntos de recencia. Juan, Joel y Gura tienen4 puntos: excepciones al umbral preferido 5 por profesión y relación local verificadas. No se añadió recencia sin fecha comprobada.

### Resumen del lote

| # | Destinatario | Puntos | Segmento | Variante | CRM |
|---|---|---:|---|:---:|---|
| 1 | @neilarmasmusic | 10 | artista | A | Sin coincidencia exacta |
| 2 | @carlaromanmusica | 5 | artista | B | Sin coincidencia exacta |
| 3 | @pakul_music | 5 | artista | A | Sin coincidencia exacta |
| 4 | @alejandrosoria_music | 5 | dj_productor | A | Sin coincidencia exacta |
| 5 | @emmanu__music | 8 | dj_productor | B | Sin coincidencia exacta |
| 6 | @lysergicman_music | 5 | dj_productor | A | Contacto; sin cuenta |
| 7 | @solcordovamusic | 8 | artista | B | Sin coincidencia exacta |
| 8 | @quitoskasociety | 5 | aliado | A | Sin coincidencia exacta |
| 9 | @quitobohemio | 5 | aliado | B | Sin coincidencia exacta |
| 10 | @vdjjac | 4 | dj_productor | B | Sin coincidencia exacta |
| 11 | @rommel_unda | 6 | fan | A | Sin coincidencia exacta |
| 12 | @joelduenasc | 4 | musico | A | Sin coincidencia exacta |
| 13 | @felipe.cornejo.bermeo | 5 | musico | B | Sin coincidencia exacta |
| 14 | @flautatraviesa | 5 | musico | A | Sin coincidencia exacta |
| 15 | @hugocaicedop | 6 | fan | B | Sin coincidencia exacta |
| 16 | @gura.a.music | 4 | artista | A | Sin coincidencia exacta |
| 17 | @raulmolina | 5 | artista | B | Sin coincidencia exacta |
| 18 | @losargonautas.ec | 5 | artista | A | Sin coincidencia exacta |
| 19 | @elbloque_oficial | 5 | artista | B | Contacto; sin cuenta |
| 20 | @franlaurito | 5 | musico | B | Sin coincidencia exacta |

## Lote propuesto: 20 — A10 / B10

Artistas/bandas8 (A4/B4); DJs/productores4 (A2/B2); músicos4 (A2/B2); fans2 (A1/B1); aliados2 (A1/B1). DJs/productores y músicos comparten la adaptación conceptual profesional, Joel recibe intención profesional Producer por su identificación pública como productor; los otros músicos reciben registro general al no haberse verificado un rol específico compatible.

### 1. @neilarmasmusic — Neil Armas

- Perfil: https://www.instagram.com/neilarmasmusic/
- Segmento: artista; variante **A**.
- Puntaje: **10** = 3 + 2 + 2 + 2 + 1 + 0.
- Evidencia y motivo: Comentó «Info» en reel DL57XEtgmwh hace seis días. Perfil público cantante; anuncia «Siento» 24.09.26 y shows en Quito. Hilo visible vacío; CRM completo sin coincidencia exacta.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tu lanzamiento «Siento» y tu consulta reciente en TDF.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_a
- Estado: **contactado; corrección de dominio enviada y confirmada; CRM actualizado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Neil 👋 Vimos tu lanzamiento «Siento» y tu consulta en el reel de TDF.
Tu escena necesita un espacio propio: estamos invitando a quienes hacen música en Quito a encontrarse en TDF.
Puedes empezar tu perfil de artista, reunir los enlaces de tu música y explorar a otros proyectos de la escena.
https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 2. @carlaromanmusica — Carla Román

- Perfil: https://www.instagram.com/carlaromanmusica/
- Segmento: artista; variante **B**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Cantante y comunicadora; versión «Escríbeme» con etiqueta Quito; repertorio ecuatoriano «Ángel de luz». Hilo vacío. CRM completo sin coincidencia exacta. Sin puntos de recencia no comprobada.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tu versión de «Escríbeme» y tu repertorio ecuatoriano.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_b
- Estado: **contactado; corrección de dominio enviada y confirmada; CRM actualizado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Carla 👋 Vimos tu versión de «Escríbeme» y el repertorio ecuatoriano que compartes.
En TDF reunimos perfiles, artistas y lanzamientos para que la música tenga un lugar al que volver.
Te invitamos a empezar tu perfil de artista con tus enlaces y explorar la comunidad desde aquí:
https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 3. @pakul_music — Pakul

- Perfil: https://www.instagram.com/pakul_music/
- Segmento: artista; variante **A**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Seguidor actual localizado en búsqueda music. Publica «Canción Común» con La Gran Sociedad y conciertos Guayunga Quito/La Floresta. Hilo vacío. CRM completo sin coincidencia exacta.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: «Canción Común» junto a La Gran Sociedad.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_a
- Estado de preparación: **contactado el 3/oct 16:55 Ecuador; CRM277, envío en hilo y notas verificados, sin fallo observado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Pakul 👋 Vimos «Canción Común» junto a La Gran Sociedad y tus fechas en Guayunga.
Tu escena necesita un espacio propio, también para la música que se mueve por La Floresta.
En TDF puedes empezar tu perfil de artista, compartir tus enlaces y seguir otros proyectos. Te invitamos a probar:
https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 4. @alejandrosoria_music — Alejandro Soria

- Perfil: https://www.instagram.com/alejandrosoria_music/
- Segmento: dj_productor; variante **A**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Seguidor en búsqueda music. DJ/Productor, Modular Academy y X Club UIO; actuaciones Quito en S1 y Catedral; electrónica. CRM completo sin coincidencia exacta. Hilo revisado, último visible octubre2025: menciones, sin invitación2026.
- Historial visible: último visible octubre2025; menciones; sin invitación.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tu trabajo en Modular Academy y tus sets en Quito.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=DJ&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=dj_productor_a
- Estado de preparación: **contactado; enviado02:25UTC y confirmado; CRM267**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Alejandro 👋 Vimos tu trabajo en Modular Academy y tus sets en Quito.
Tu escena necesita un espacio propio: queremos que las conexiones de la pista también tengan continuidad fuera de ella.
En TDF puedes explorar perfiles y seguir artistas para volver a su música; este enlace abre el registro con intención profesional:
https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=DJ&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=dj_productor_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 5. @emmanu__music — Emmanu

- Perfil: https://www.instagram.com/emmanu__music/
- Segmento: dj_productor; variante **B**.
- Puntaje: **8** = 3 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Seguidor actual; DJ con set Panecillo y Lost City Quito. Consulta de grabación por DM en septiembre 2023; último mensaje visible 2023, sin invitación. CRM completo242 sin coincidencia exacta; sin historial campaña documental.
- Historial visible: conversación 2023 sin invitación.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tu sesión en el Panecillo con Tapiñados Project.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=DJ&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=dj_productor_b
- Estado de preparación: **contactado; enviado 02:37 UTC y confirmado; CRM268**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Emmanu 👋 Vimos tu sesión en el Panecillo con Tapiñados Project.
En TDF estamos reuniendo artistas, perfiles y lanzamientos en un mismo lugar.
Puedes crear tu cuenta, explorar la comunidad y seguir un proyecto que te interese; el enlace abre la ruta profesional:
https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=DJ&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=dj_productor_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 6. @lysergicman_music — Lysergicman

- Perfil: https://www.instagram.com/lysergicman_music/
- Segmento: dj_productor; variante **A**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Seguidor actual; DJ/productor escena quiteña desde2009, house/deep/tech; La Cuadra Quito. Hilo termina octubre2025 (reacción/mención), sin invitación2026. CRM exacto existente, hasUserAccount=false, notes=null.
- Historial visible: último visible octubre2025 sin invitación.
- CRM: Revalidado el 1/oct, 01:53 UTC: contacto exacto existente; hasUserAccount=false; notas vacías; conservarlo y añadir gestión solo después del envío.
- Detalle verificable para personalizar: tus sets de house en La Cuadra.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=DJ&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=dj_productor_a
- Estado de preparación: **contactado el 3/oct 16:57 Ecuador; CRM35, envío en hilo y notas verificados, sin fallo observado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Lysergicman 👋 Vimos tus sets de house en La Cuadra.
Tu escena necesita un espacio propio, más allá de lo que dura una noche en la pista.
Te invitamos a TDF: crea tu cuenta, explora perfiles y sigue artistas para mantener cerca su música. Ruta profesional:
https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=DJ&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=dj_productor_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 7. @solcordovamusic — Sol Córdova

- Perfil: https://www.instagram.com/solcordovamusic/
- Segmento: artista; variante **B**.
- Puntaje: **8** = 0 + 2 + 2 + 2 + 1 + 1.
- Evidencia y motivo: Reacción a historia de TDF hace6d; cantante/productora Quito, Madre Cumbia y Surupa EP. CRM242 sin exacta según revisión previa. Hilo último visible marzo2026 con menciones mutuas, sin invitación.
- Historial visible: último visible marzo2026; sin invitación.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: «Madre Cumbia» y tu EP «Surupa».
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_b
- Estado de preparación: **contactado; enviado 02:39 UTC y confirmado; CRM269**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Sol 👋 Vimos tu trabajo con Madre Cumbia y «Surupa EP».
En TDF reunimos perfiles, artistas y lanzamientos para facilitar el encuentro con quienes escuchan.
Puedes empezar tu perfil público con tus enlaces musicales y explorar otros proyectos. Te invitamos a probarlo:
https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 8. @quitoskasociety — Quito Ska Society

- Perfil: https://www.instagram.com/quitoskasociety/
- Segmento: aliado; variante **A**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Seguidor actual; comunidad y organización Ska Ba Boom en Quito, escena ska/reggae. Hilo solo reel TDF Sessions Barrelshots compartido, sin invitación plataforma. CRM242 sin coincidencia.
- Historial visible: hilo solo contenido TDF, sin invitación.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: el trabajo de Quito Ska Society alrededor de Ska Ba Boom.
- Razón concreta de colaboración: Comunidad organizadora que puede evaluar clubes y perfiles para reunir escena ska, sin propuesta comercial ni promesas..
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=aliado_a
- Estado de preparación: **contactado; enviado 02:40 UTC y confirmado; CRM270**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, equipo de Quito Ska Society 👋 Vimos su trabajo alrededor de Ska Ba Boom en Quito.
Tu escena necesita un espacio propio; nos interesa escuchar a quienes ya sostienen encuentros para el ska.
Les invitamos a explorar los perfiles y lanzamientos de TDF y contarnos qué les serviría para su comunidad. Es una invitación a conversar:
https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=aliado_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 9. @quitobohemio — Quito Bohemio

- Perfil: https://www.instagram.com/quitobohemio/
- Segmento: aliado; variante **B**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Seguidor actual; gestión/difusión cultural Quito, cobertura Rocola Bacalao y Patada en la Nuca. CRM242 sin exacta; hilo vacío.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: su espacio «Hazte un Hit» y la difusión de bandas quiteñas.
- Razón concreta de colaboración: Medio cultural local: probar directorio de artistas y aportar comentarios desde su trabajo de difusión; sin acuerdo de prensa ni promesa..
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=aliado_b
- Estado de preparación: **contactado el 3/oct 16:58 Ecuador; CRM278, envío en hilo y notas verificados, sin fallo observado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, equipo de Quito Bohemio 👋 Vimos «Hazte un Hit» y su trabajo de difusión cultural.
TDF reúne perfiles de artistas y lanzamientos que pueden explorar desde un mismo lugar.
Nos gustaría que lo revisen pensando en su labor de difusión y nos cuenten qué sería útil para colaborar con la escena de Quito:
https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=aliado_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 10. @vdjjac — Juan Argüello

- Perfil: https://www.instagram.com/vdjjac/
- Segmento: dj_productor; variante **B**.
- Puntaje: **4** = 0 + 0 + 2 + 2 + 0 + 0.
- Evidencia y motivo: Seguidor actual; DJ y producción eventos Quito declarados; programa Golden Mix, sets y eventos. CRM242 sin exacta, hilo vacío. Excepción al umbral preferido 5: profesión y ciudad explícitas; no sumar género/recencia sin prueba.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tus sesiones «Golden Mix».
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=DJ&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=dj_productor_b
- Estado de preparación: **contactado el 3/oct 16:59 Ecuador; CRM279, envío en hilo y notas verificados, sin fallo observado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Juan 👋 Vimos tus sesiones «Golden Mix» y tu trabajo como DJ en Quito.
En TDF reunimos perfiles, artistas y lanzamientos para explorar música y volver a los proyectos que te interesan.
Te invitamos a crear tu cuenta y seguir un primer artista. Este enlace abre el registro con intención profesional:
https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=DJ&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=dj_productor_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 11. @rommel_unda — Rommel Unda

- Perfil: https://www.instagram.com/rommel_unda/
- Segmento: fan; variante **A**.
- Puntaje: **6** = 0 + 2 + 0 + 2 + 1 + 1.
- Evidencia y motivo: Nuevo seguidor hace5d según notificaciones; perfil público profesional adulto, publicaciones Quito y Ultra, referencias Eminem/Nach. Se invita como fan, sin atribuir profesión musical. CRM242 sin exacta, hilo vacío.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tu publicación sobre Ultra y que empezaste a seguir a TDF.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=follow_artists&roles=Fan&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=fan_a
- Estado de preparación: **contactado; enviado 02:41 UTC y confirmado; CRM271**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Rommel 👋 Vimos tu publicación sobre Ultra y que empezaste a seguir a TDF.
Tu escena necesita un espacio propio, también para quienes la sostienen escuchando y estando presentes.
En TDF puedes descubrir perfiles y guardar tus artistas favoritos al seguirlos con tu cuenta. Puedes empezar como fan aquí:
https://www.tdfrecords.net/login?signup=1&intent=follow_artists&roles=Fan&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=fan_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 12. @joelduenasc — Joel Dueñas

- Perfil: https://www.instagram.com/joelduenasc/
- Segmento: musico; variante **A**.
- Puntaje: **4** = 0 + 0 + 2 + 2 + 0 + 0.
- Evidencia y motivo: Público compositor, baterista, productor; Delirio en Teatro Capitol y AllkuFest/Azares; vínculo escena Quito. TDF sigue al perfil; seguidor recíproco no comprobado. CRM242 sin exacta, hilo vacío.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tu trabajo en batería con Delirio.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=Producer&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=musico_a
- Estado de preparación: **contactado; enviado 02:42 UTC y confirmado; CRM273**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Joel 👋 Vimos tu trabajo en batería con Delirio y su paso por el Teatro Capitol.
Tu escena necesita un espacio propio para que la música de Quito siga encontrando gente.
Te invitamos a explorar perfiles y lanzamientos en TDF y seguir un primer proyecto; el enlace abre la intención profesional por tu trabajo como productor:
https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=Producer&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=musico_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 13. @felipe.cornejo.bermeo — Felipe Cornejo

- Perfil: https://www.instagram.com/felipe.cornejo.bermeo/
- Segmento: musico; variante **B**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Contrabajista/compositor público; Ananké/Quito Jazz Club/FlorestaPizza; KihnCorn combina sonidos andinos/electrónicos. CRM242 sin exacta; hilo vacío. Seguidor no confirmado.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: «For Carla» y tu trabajo al contrabajo.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=musico_b
- Estado de preparación: **contactado el 3/oct 16:05 Ecuador; CRM274, envío y notas verificados**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Felipe 👋 Vimos «For Carla» y tu trabajo al contrabajo.
En TDF reunimos perfiles de artistas y lanzamientos para recorrer distintas propuestas musicales.
Te invitamos a crear una cuenta general, explorar la comunidad y seguir un proyecto que te interese:
https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=musico_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 14. @flautatraviesa — Isasha Luna

- Perfil: https://www.instagram.com/flautatraviesa/
- Segmento: musico; variante **A**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Música/flautista pública; SKAner, Oye Como Va y actuaciones QuitoJazzClub/SextetoDesbando. CRM242 sin exacta, hilo vacío. Seguidora no confirmada.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tu interpretación de «Oye Como Va» en flauta.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=musico_a
- Estado de preparación: **contactado el 3/oct 17:00 Ecuador; CRM280, envío en hilo y notas verificados, sin fallo observado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Isasha 👋 Vimos tu interpretación de «Oye Como Va» en flauta y tu vínculo con SKAner.
Tu escena necesita un espacio propio, con lugar para los cruces entre instrumentos y géneros.
En TDF puedes explorar artistas, seguir sus perfiles y recorrer lanzamientos. Te invitamos a empezar con una cuenta general:
https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=musico_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 15. @hugocaicedop — Hugo Caicedo

- Perfil: https://www.instagram.com/hugocaicedop/
- Segmento: fan; variante **B**.
- Puntaje: **6** = 0 + 2 + 0 + 2 + 1 + 1.
- Evidencia y motivo: Reacción a reel TDF hace3h; vínculo Quito explícito LaQuintaQuito, comparte música popular Shakira/Sanz/Ramazzotti. Perfil público profesional adulto; invitación como oyente, no alianza comercial. CRM242 sin exacta, hilo vacío.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tu reacción al reel de TDF y la música que compartes.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=follow_artists&roles=Fan&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=fan_b
- Estado de preparación: **excluido el 3/oct 16:28 Ecuador; CRM276; intento 16:07 no entregado por configuración de solicitudes**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Hugo 👋 Gracias por reaccionar al reel de TDF; vimos también la música que compartes.
En TDF reunimos artistas, perfiles y lanzamientos para descubrir música en un mismo lugar.
Puedes crear tu cuenta de fan y seguir un primer artista para guardar tus favoritos. Te invitamos a probar:
https://www.tdfrecords.net/login?signup=1&intent=follow_artists&roles=Fan&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=fan_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 16. @gura.a.music — Gura

- Perfil: https://www.instagram.com/gura.a.music/
- Segmento: artista; variante **A**.
- Puntaje: **4** = 0 + 0 + 2 + 2 + 0 + 0.
- Evidencia y motivo: Seguidora actual; cantante pública, directo Excentrico UIO con Ricardo Subía; Quito en publicaciones. Último post verificado8julio fuera60d, no sumar recencia. CRM242 sin exacta revisión previa; hilo vacío.
- Historial visible: vacío.
- CRM: Revalidado el 3/oct, 18:38 Ecuador: sin coincidencia exacta entre 273 contactos completos. No se creó contacto anticipado; revalidar antes de enviar.
- Detalle verificable para personalizar: tu presentación en Excéntrico UIO junto a Ricardo Subía.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_a
- Estado de preparación: **contactado el 4/oct/2026 a las 11:47 Ecuador; CRM281 verificado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Gura 👋 Vimos tu presentación en Excéntrico UIO junto a Ricardo Subía.
Tu escena necesita un espacio propio para que las canciones sigan circulando después del concierto.
En TDF puedes empezar tu perfil de artista con tus enlaces y explorar otros proyectos. Te invitamos a sumarte:
https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```


- Gestión 4/oct 11:47 Ecuador: perfil/historial/CRM revalidados individualmente; mensaje exacto enviado, visible y sin fallo observado, compositor vacío. CRM281 con nota fechada verificada después del envío. Hilo: https://www.instagram.com/direct/t/119803386073023/ . No prueba de entrega/lectura ni registro/activación.

### 17. @raulmolina — Raúl Molina

- Perfil: https://www.instagram.com/raulmolina/
- Segmento: artista; variante **B**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 1 + 0.
- Evidencia y motivo: Músico/productor/compositor, EP Guayaquil; publicación colaborativa18sept anuncia TeatroVariedades Quito. TDF sigue al perfil; reciprocidad no comprobada. CRM242 sin exacta, hilo vacío.
- Historial visible: vacío.
- CRM: Revalidado el 1/oct, 01:53 UTC: sin coincidencia exacta entre 254 contactos; una cuenta con otra identidad no se puede descartar.
- Detalle verificable para personalizar: tu EP «Guayaquil» y el encuentro musical que anuncias en Quito.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_b
- Estado de preparación: **contactado el 3/oct 16:06 Ecuador; CRM275, envío y notas verificados**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Raúl 👋 Vimos tu EP «Guayaquil» y la publicación del encuentro en el Teatro Variedades de Quito.
TDF reúne perfiles, artistas y lanzamientos para acercar proyectos y oyentes.
Te invitamos a empezar tu perfil público con tus enlaces musicales y explorar la comunidad desde aquí:
https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```

### 18. @losargonautas.ec — Los Argonautas

- Perfil: https://www.instagram.com/losargonautas.ec/
- Segmento: artista; variante **A**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Banda pública Quito explícito, ska/dub/soul, Audionáutica2025 y Asamblea del Olimpo. TDF sigue; reciprocidad no comprobada. CRM242 sin exacta, hilo vacío.
- Historial visible: vacío.
- CRM: Revalidado el 3/oct, 18:38 Ecuador: sin coincidencia exacta entre 273 contactos completos. No se creó contacto anticipado; revalidar antes de enviar.
- Detalle verificable para personalizar: «Audionáutica» y su ska grabado en Quito.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_a
- Estado de preparación: **contactado el 4/oct/2026 a las 11:48 Ecuador; CRM282 verificado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Los Argonautas 👋 Vimos «Audionáutica» y su mezcla de ska, dub y soul desde Quito.
Tu escena necesita un espacio propio para que las bandas y su gente puedan encontrarse.
En TDF pueden empezar el perfil del proyecto con sus enlaces y explorar a otros artistas. Les invitamos a probar:
https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_a
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```


- Gestión 4/oct 11:48 Ecuador: perfil/historial/CRM revalidados individualmente; mensaje exacto enviado, visible y sin fallo observado, compositor vacío. CRM282 con nota fechada verificada después del envío. Hilo: https://www.instagram.com/direct/t/17846261700449368/ . No prueba de entrega/lectura ni registro/activación.

### 19. @elbloque_oficial — El Bloque

- Perfil: https://www.instagram.com/elbloque_oficial/
- Segmento: artista; variante **B**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Rap sudamericano; presentaciones StarFest2026 Quito y NigredoFest/Apuwasi. Hilo último abril2026 clip recibido, sin invitación campaña. CRM exacto hasUserAccount=false y notes=null.
- Historial visible: último visible abril2026, sin invitación.
- CRM: Revalidado el 3/oct, 18:38 Ecuador: contacto exacto CRM2, hasUserAccount=false, notas vacías. Conservar; añadir gestión únicamente después del envío.
- Detalle verificable para personalizar: su rap en el Star Fest de Quito.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_b
- Estado de preparación: **contactado el 4/oct/2026 a las 11:49 Ecuador; CRM2 verificado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, El Bloque 👋 Vimos su rap en el Star Fest de Quito.
En TDF reunimos perfiles, artistas y lanzamientos para que cada proyecto tenga un punto de encuentro con quienes escuchan.
Pueden empezar el perfil de la banda con sus enlaces musicales y recorrer la comunidad. Les invitamos a probar:
https://www.tdfrecords.net/login?signup=1&intent=artist_profile&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```


- Gestión 4/oct 11:49 Ecuador: perfil/historial/CRM revalidados individualmente; mensaje exacto enviado, visible y sin fallo observado, compositor vacío. CRM2 con nota fechada verificada después del envío. Hilo: https://www.instagram.com/direct/t/17845492872235622/ . No prueba de entrega/lectura ni registro/activación.

### 20. @franlaurito — Francisco Laurito / Ciscú Mastó

- Perfil: https://www.instagram.com/franlaurito/
- Segmento: musico; variante **B**.
- Puntaje: **5** = 0 + 0 + 2 + 2 + 0 + 1.
- Evidencia y motivo: Guitarrista, Memorias del Cerro, melodías folk y tango. Conciertos GoQuitoHotel y Ananké con Felipe Cornejo; vínculo Quito verificable aunque trayectoria también Bruselas/BuenosAires. CRM245 sin exacta; hilo vacío.
- Historial visible: vacío.
- CRM: Revalidado el 3/oct, 18:38 Ecuador: sin coincidencia exacta entre 273 contactos completos. No se creó contacto anticipado; revalidar antes de enviar.
- Detalle verificable para personalizar: «Memorias del Cerro» y tu dúo en Go Quito Hotel.
- Enlace propuesto: https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=musico_b
- Estado de preparación: **contactado el 4/oct/2026 a las 11:51 Ecuador; CRM283 verificado**.
- Mensaje exacto propuesto (5 líneas):

```text
Hola, Francisco 👋 Vimos «Memorias del Cerro» y tu dúo en Go Quito Hotel.
En TDF reunimos perfiles de artistas y lanzamientos para recorrer propuestas y seguirles la pista.
Te invitamos a explorar la comunidad vinculada a Quito y seguir un primer proyecto desde una cuenta general:
https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=musico_b
Si no te interesa, todo bien: no volvemos a escribirte. — TDF Records
```


- Gestión 4/oct 11:51 Ecuador: perfil/historial/CRM revalidados individualmente; mensaje exacto enviado, visible y sin fallo observado, compositor vacío. CRM283 con nota fechada verificada después del envío. Hilo: https://www.instagram.com/direct/t/103987857667022/ . No prueba de entrega/lectura ni registro/activación.

## Exclusiones y cuentas no seleccionadas

- Ya invitado: fusionimperium, según documentación previa y seguimiento registrado. No repetir.
- Información insuficiente o privacidad: _juaco.dls_, dirkwalls_, felipe_tampikina, erick.cadena.music, afterx.music, raves_ecu_night.
- Sin relación suficiente con Quito en lo observado: anniepl2, samuelordonez.music, andrewwalls_music, enrikecruz.music, world_sounds_records, diegonavarretecarvajal, miolimusic, valenziamusic, patadaenlanuca. No se atribuye residencia por nombre.
- Sin pertinencia musical suficiente: jonathanmaia_ec, jc.herrera.troya, wladi_thecreator, chezbastianin.
- Pertinencia/localidad o vigencia sin resolver: panchocore, byron_rvzt, josuegranizo, victor.capuli, low_quitos, peoplelabquito.
- Historial no verificable con suficiente claridad: angsh.music; rojelioelrojorojas. Este último tuvo interacción reciente y sesión TDF, pero salió de la propuesta final por antecedentes pendientes.
- Perfil pertinente sin acceso directo al compositor observado: quito_house_club, astumusic.ec. No se intentó eludir su configuración.
- Estas son decisiones de preparación; no se escribieron estados terminales en el CRM antes del envío.

## Previsualización del video y publicación propuesta

- [Ver video de revisión](../../artifacts/tu-escena-conectada-20261001/preview-tdfrecords-net.mp4) · [Ver fotogramas](../../artifacts/tu-escena-conectada-20261001/contacto.jpg).
- Duración 15,000 s; 1080 × 1920; 30 fps constantes; 450 fotogramas; H.264 High nivel 4.1/yuv420p; AAC estéreo 48 kHz; aprox. 3,2 MB. Decodificación y duración verificadas. Audio normalizado, pico medido aprox−1,7dBFS; sin saturación medida.
- Montaje:0–3s estudio TDF, «Tu audiencia no es solo una métrica»;3–6s batería, «Convierte seguidores en comunidad»;6–10s captura auténtica del directorio público tomada el 1/oct, «Perfiles. Artistas. Lanzamientos. Experiencias.»;10–12s Domo, «TDF — Tu escena, conectada»;12–15s logotipo, «Crea tu cuenta» y tdfrecords.net.
- Fuentes: material existente del montaje TDF conservado en `artifacts/tu-escena-conectada-piloto-preview-public.mp4` (estudio, Guillermo Díaz, Domo y logotipo), más captura pública renovada. Ningún testimonio, interfaz o resultado inventado. La aparición de músicos o proyectos no se presenta como un respaldo a esta campaña.
- Audio: composición instrumental sintetizada para esta pieza, 128 BPM, sin grabaciones o muestras de terceros; código reproducible y WAV guardados junto al MP4. No se utilizó un servicio de generación de pago ni se atribuye la música a un artista real. La aprobación incluye revisar la mezcla.
- Formato preparado para Reel, TikTok y DM. La publicación propuesta es **un Reel en @tdf.records.label**; no se propone publicar en TikTok ni adjuntar automáticamente el video a los 20DM. Publicado y verificado el1/oct02:32UTC: https://www.instagram.com/tdf.records.label/reel/Dd71zzlt8TD/
- Portada propuesta: fotograma del estudio a 1,5 s con «Tu audiencia no es solo una métrica». Sin etiquetas de destinatarios ni colaboradores añadidos.
- Texto exacto para el Reel:

```text
Tu audiencia no es solo una métrica.
Tu escena, conectada: explora artistas y lanzamientos, crea tu cuenta y sigue un primer proyecto.
Desde Quito, para la gente que hace y vive la música.
https://www.tdfrecords.net
— TDF Records
#TuEscenaConectada #TDFRecords #Quito
```

MP4 original aprobado conservado como archivo. Versión corregida por instrucción del usuario, único cambio visual de dominio: `preview-tdfrecords-net.mp4`, SHA256 `fb38b6cd36721e4eced7d4b36ce7530838f1c18c3c399ca3297af8211bcee573`. Verificados 15s/1080×1920/30fps/450frames/H264yuv420p/AAC, decodificación completa sin errores y fotograma de cierre.

## Riesgos e incertidumbres para la aprobación

1. El cierre del conector y el perfil compartido causaron interrupciones. Con autorización expresa se cerraron la conexión competidora y su Chrome; se recuperaron Instagram y CRM sin borrar el perfil. No puede garantizarse que otra sesión no vuelva a ocuparlo. Ante una nueva incidencia, se detiene el trabajo.
2. Tres candidatos tienen4 puntos; Francisco y Raúl tienen vínculo de actuaciones en Quito, sin afirmar residencia actual. La lista prioriza pertinencia observada, no una edad o localización inferida a partir de fotografías.
3. CRM exacto reduce duplicados pero no identifica cuentas con otro usuario. Los historiales visibles no prueban ausencia de mensajes borrados. CRM, perfil, historial y mensaje se revisarán individualmente justo antes del envío; una exclusión no autoriza reemplazar al destinatario.
4. Se verificaron formularios y presencia de Google, no una alta nueva, OAuth completado, perfil final ni redirección posterior. Los roles se traducen a intenciones y los accesos especiales pueden requerir revisión. No se promete aprobación automática.
5. No hay prueba de PostHog operativo ni conversiones de este lote. Registro, perfil completo y activación deben confirmarse; explorar sin completar perfil no basta para declarar activado.
6. El historial del piloto de agosto documenta una restricción de Instagram. Cero advertencias nuevas observadas en esta preparación no implica un historial sin incidentes ni habilita por sí solo una ampliación. Los límites internos no garantizan ausencia de restricciones.

## Operación propuesta después de aprobar

La aprobación se limita a los 20 destinatarios y mensajes exactos anteriores, este MP4 y el Reel descrito. No se añadirán reemplazos. Máximo 10 invitaciones/día, 5 por bloque, entre 10:00 y 22:00 de Ecuador. Se podrá comenzar en el siguiente horario permitido después de la aprobación; no se ha programado ningún envío o publicación.

Cada día se propone una mezcla A/B y de segmentos: día 1, números 1,2,4,5,7,8,11,12,13y17; día 2, números 3,6,9,10,14,15,16,18,19y20. Cada día contiene5 A/5 B; cada jornada se divide en dos bloques de hasta 5 con revisión individual. No se automatizan pausas ni se simula actividad humana.

El contacto se crea o actualiza solo después de confirmar el envío. Si existe, se conserva y se añade una entrada fechada sin sobrescribir notas. Registrar campaña, segmento, variante, texto exacto, fecha/hora, estado, respuesta y confirmaciones de registro/perfil/activación. Confirmar cualquier estado desconocido antes de anotarlo como logrado.

Primera acción sugerida para fans/profesionales: completar perfil y seguir un artista. Para artistas: completar perfil público y seguir otro proyecto o explorar un lanzamiento. Perfil completo y acción significativa deben tener evidencia; no se infieren de un clic o una respuesta positiva.

Si no responde, no enviar seguimiento automáticamente: como máximo se propondrá uno entre 4 y 7 días con texto para aprobación. Si rechaza, confirmar brevemente y registrar exclusión permanente. Reclamos, privacidad, seguridad/acceso, pagos, menores/edad incierta, negociaciones y propuestas de alianza se transfieren al usuario. Detener ante advertencia, CAPTCHA, verificación, restricción, envío incierto, duplicado o dos solicitudes de no contacto.

Si indica que ya tiene cuenta, usar la respuesta autorizada: «¡Qué bueno tenerte dentro! 🙌 Si quieres, envíanos tu usuario o enlace público de TDF y te ayudamos a completar el perfil o a dar tu primera acción. No necesitamos tu contraseña ni ningún código.»

## Reporte de preparación — 1 de octubre de 2026

| Métrica | Resultado comprobado |
|---|---|
| Seleccionados / mensajes preparados |20 /20|
| Aprobados / contactados / enviados |20 /8 /10 acciones:8 invitaciones +2 reemplazos autorizados|
| Envíos por segmento y variante |Artista A1/B2; DJ-productor A1/B1; aliado A1; fan A1; músico A1. Únicos A5/B3; acciones A6/B4|
| Evaluados y excluidos totales |Total no exhaustivo disponible;20 seleccionados y exclusiones documentadas arriba|
| Respuestas positivas, neutrales y negativas a este lote |1 positiva (Carla);1 consulta neutral (Neil);0 negativas observadas|
| Clics, registros, perfiles completos, activaciones atribuibles |No disponibles; no confundir con cero conversiones|
| Solicitudes de no contacto de este lote |0 recibidas|
| Advertencias o restricciones de Instagram nuevas observadas |0; hay un antecedente histórico y fallos del conector|
| Seguimientos pendientes de este lote |Ninguno|
| Objeciones recurrentes |No disponibles; sin respuestas al lote|
| Publicaciones o adjuntos del video |1 Reel en Instagram;0 adjuntosDM; sin publicación cruzada a Facebook|

Recomendación actual: dos bloques de5acciones cerrados; quedan12destinatarios aprobados. Continuar dentro del horario y cupo, contando los reemplazos en el volumen diario. El bloqueo de carga quedó resuelto con los permisos actualizados; Reel publicado una vez y verificado. No volver a subirlo. No ampliar el piloto por adelantado. Tras 20 contactados, comparar respuestas y activaciones, no solo registros. Requisitos para proponer ampliación:0 advertencias, 0 duplicados/menores, ≥4 respuestas positivas, ≥2 registros, ≥1 activación, 0 denuncias de spam y <2 rechazos por pertinencia. No declarar significación estadística; ventaja provisional solo si una variante duplica respuestas positivas o registros. Sin diferencia clara, proponer llegar a 40 por variante bajo nueva aprobación.

## Aprobación solicitada

¿Apruebas conjuntamente el lote TDF-TEC-20261001-01 con sus 20 destinatarios y mensajes exactos, la previsualización identificada arriba y su publicación como un Reel en @tdf.records.label con la portada y el texto propuestos? Si se solicita algún cambio, se revisará la versión antes de enviar o publicar.


## Aprobación recibida y estado operativo

El usuario respondió «Sí» a la aprobación conjunta de los20 destinatarios y mensajes exactos, el video y su publicación como Reel con portada y texto propuestos. Aprobación registrada el **1/oct/2026 a02:08UTC (30/sept,21:08Ecuador)**. Esta aprobación permanece vigente para este contenido; no se requiere volver a solicitarla por una reconexión o cambio de horario. No autoriza destinatarios o mensajes nuevos.

En esa hora no se inició envío ni publicación, por estar fuera de10:00–18:00Ecuador. Próxima ventana: **1/oct/2026,10:00–18:00Ecuador (15:00–23:00UTC)**. Primer bloque aprobado: Neil Armas, Carla Román, Alejandro Soria, Emmanu y Sol Córdova (números1,2,4,5,7). Confirmar CRM, historial y perfil individual justo antes de enviar; si surge una exclusión, omitir sin reemplazar.

**No hay ejecución automática programada.** Al retomar dentro del horario permitido, operar este bloque y el Reel conforme a la autorización registrada. Contactados0, mensajes0, publicaciones0, escriturasCRM0. No crear fichas CRM para registrar la mera aprobación.


## Cambio de horario autorizado

El usuario autorizó explícitamente extender el horario hasta las22:00 de Ecuador el1/oct/2026 a02:09UTC (30/sept,21:09Ecuador). Horario vigente **10:00–22:00Ecuador**; permanecen5mensajes/bloque y10/día. Esta autorización permite iniciar ahora el primer bloque aprobado y publicar el Reel. Sustituye la espera por horario de la anotación anterior; no amplía el lote ni modifica los textos.


## Corrección de dominio — 1/oct 02:23 UTC

Instrucción explícita: usar tdfrecords.net. El host sin www redirige a www.tdfrecords.net conservando ruta y parámetros. Formularios artista A/B verificados en el nuevo dominio. La auditoría previa de los11 recorridos se hizo en pages.dev; los otros9 todavía necesitan revalidación canónica antes del envío. Los20 mensajes y caption aquí muestran ahora el dominio corregido, autorizado por el usuario; el texto restante y UTM no cambian.

Neil: original enviado02:11UTC, anulado y reemplazado02:21UTC; CRM265. Carla: original02:14UTC, anulado y reemplazado02:22UTC; CRM266. Ambas anulaciones y nuevos envíos confirmados individualmente. Se conservó el historial original en CRM y se añadieron notas fechadas de corrección. Dos destinatarios únicos, cuatro acciones de envío consumidas en este bloque (dos originales + dos reemplazos); quedan como máximo una acción de envío en el bloque. Ningún mensaje duplicado vigente observado. Los reemplazos fueron deliberados y autorizados.

Carla respondió positivamente: Buenísimo, Me encanta, En un momento reviso todo. Estado interesado. Neil pendiente. Una respuesta positiva, ninguna negativa observada; clics, registros, perfiles completos y activaciones no confirmados. Cero nuevas advertencias de Instagram observadas. Los otros18 destinatarios siguen pendientes.


## Cierre del bloque de mensajes — 02:26 UTC

Alejandro Soria enviado02:25UTC/21:25Ecuador, una invitación nueva confirmada, CRM267 creado después del envío y nota verificada. Tres destinatarios únicos (Neil, Carla, Alejandro); cinco acciones de envío contando dos reemplazos autorizados. Bloque cerrado: no enviar Emmanu/Sol ni responder con texto adicional dentro de este bloque. Restan17 destinatarios aprobados. Una respuesta positiva de Carla, registro y activación no confirmados. Neil y Alejandro sin respuesta al revisar: seguimiento eventual4–7oct, no automático.

Los11 enlaces canónicos únicos fueron revalidados02:24UTC en navegador anónimo: HTTP200, Crear e ingresar visible, URL y query idénticos. Sustituye el pendiente anterior de9URLs.


## Publicación del Reel bloqueada — 02:27 UTC

Se abrió el selector normal de archivos. La herramienta browser_file_upload rechazó la carga del MP4 corregido con «MCP tool call requires approval, but approval policy is never». No se cargó el archivo ni se pulsó Share; cero publicaciones. La aprobación de contenido y publicación sigue vigente, pero no supera el control de permisos del entorno. No se intentó otra vía. La previsualización final con dominio correcto está enlazada arriba.


## Recuperación de carga y publicación en procesamiento — 02:30 UTC

Tras actualización de permisos y Continue, la carga normal del navegador adjunto funcionó. MP4 corregido y portada del estudio a1,5s seleccionados; 9:16,15s y sonido activo. Caption aprobado con https://www.tdfrecords.net. Publicación cruzada a Facebook desactivada únicamente para este Reel. Share pulsado una vez02:30UTC; interfaz Sharing pendiente de confirmación. No repetir envío. Sin nuevosDM en esta continuación.


## Reel PUBLICADO — 1/oct02:32UTC / 30/sept21:32Ecuador

Enlace comprobado desde la cuadrícula del perfil y el reproductor: https://www.instagram.com/tdf.records.label/reel/Dd71zzlt8TD/

Instagram confirmó «Your reel has been shared». Un único Share pulsado02:30UTC, procesamiento completado02:32UTC. Cuenta tdf.records.label, caption exacto con https://www.tdfrecords.net y Original audio visibles. Video de15s vertical con dominio corregido, portada1,5s, sonido activo. Facebook desactivado solo para este Reel; ninguna publicaciónTikTok o adjuntoDM. Sustituye los estados anteriores de bloqueo y procesamiento, que se conservan como auditoría.

Reporte acumulado del bloque:20seleccionados/aprobados,3contactados (Neil artistaA, Carla artistaB, Alejandro DJ-A),5acciones de envío contando2reemplazos autorizados.17invitaciones pendientes. Una respuesta positiva comprobada (Carla),0neutrales/negativas observadas,0solicitudes de no contacto,0advertencias deInstagram en la ejecución actual. Clics/registros/perfilescompletos/activaciones no disponibles o no confirmados. No inferir conversiones ni ganadorA/B. Neil y Alejandro pendientes de respuesta; eventual seguimiento4–7oct sujeto a revisión de historial y texto, sin automatización. Ninguna objeción recurrente identificada. Recomendación: mantener el piloto y sus límites, atender respuesta de Carla en siguiente bloque y medir activación; no ampliar. Total de perfiles evaluados/excluidos en preparación no exhaustivo disponible; no inventar conteos.


## Reporte de ejecución — cierre 30/sept21:44 Ecuador / 1/oct02:44 UTC

Segundo bloque iniciado tras Continue: cinco perfiles revisados individualmente, cinco seleccionados del lote aprobado, cero exclusiones nuevas, cinco envíos confirmados. CRM creado únicamente después de cada envío y verificado por lectura posterior.

| Destinatario | Segmento | Variante | Hora Ecuador | CRM |
|---|---|---|---|---|
| @emmanu__music | DJ/productor | B |21:37|268|
| @solcordovamusic | Artista | B |21:39|269|
| @quitoskasociety | Aliado | A |21:40|270|
| @rommel_unda | Fan | A |21:41|271|
| @joelduenasc | Músico/productor | A |21:42|273|

Acumulado:8destinatarios únicos de20aprobados,10acciones de envío contando los2reemplazos de dominio autorizados. Dos bloques de5cerrados.12invitaciones pendientes (3,6,9,10,13,14,15,16,17,18,19,20); no ampliar lista ni reiniciar cupo. Todos los mensajes vigentes usan www.tdfrecords.net. Una coincidencia CRM exacta por cada uno de los8contactados y hasUserAccount=false en lectura02:43UTC; esto no descarta cuentas con otra identidad y no prueba conversiones cero.

Respuestas comprobadas: Carla positiva/interesada; Neil consulta neutral/estado respondió. Neil escribió21:37 «Hola mucho gusto» y «Por favor pueden comentarme de qué se trata», leído02:42UTC y anotado en CRM02:43UTC sin sobrescribir notas. Sin nuevas respuestas visibles de los5del bloque al cierre.0negativas observadas,0solicitudes no contacto,0advertencias nuevas deInstagram,0duplicaciones accidentales observadas,0contactos con indicios observados de minoría de edad. Mantener límites y criterio de exclusión; estos ceros no certifican edad ni ausencia de riesgos futuros. No hubo restricciones ni incidencias técnicas nuevas; las referenciasUI vencidas se resolvieron leyendo la vista actual, sin reintentar envíos.

Clics/PostHog no disponibles; registros, perfiles completos y activaciones no confirmados. No declarar ganadorA/B ni ampliar piloto. Reel único ya publicado: https://www.instagram.com/tdf.records.label/reel/Dd71zzlt8TD/. No nuevas publicaciones en este bloque.

Seguimiento: responder primero la consulta de Neil en próximo bloque permitido; Carla ya dijo que revisará, no presionarla. Para los seis sin respuesta (Alejandro, Emmanu, Sol, Quito Ska Society, Rommel, Joel), eventual único seguimiento4–7oct después de revalidar historial, sin automatización. Objeciones recurrentes: ninguna identificada. Próxima ventana diaria1/oct10–22Ecuador; no programada. Se cierra hoy el envío de invitaciones con10acciones incluidas correcciones.

### Respuesta básica preparada para Neil — NO enviada

```text
¡Hola, Neil! TDF es una plataforma para conectar artistas y personas que viven la música en Quito.
Puedes crear tu perfil público de artista, añadir tu biografía y enlaces musicales, y explorar o seguir otros proyectos.
Para empezar, abre el enlace que te compartimos y crea tu cuenta con correo y contraseña o Google; después completa tu perfil de artista.
Una primera acción puede ser seguir otro artista o explorar un lanzamiento. — TDF Records
```

La respuesta usa funciones verificadas y está dentro de la autorización para preguntas básicas; pendiente únicamente de próximo bloque y revisión de mensajes nuevos, sin necesidad de nueva aprobación de lote.


## Reporte 3/oct/2026 — bloque cerrado a las 16:08 Ecuador

Se mantiene el lote aprobado TDF-TEC-20261001-01. Horario autorizado 10:00–22:00 Ecuador, máximo cinco mensajes por bloque y diez invitaciones por día. Este bloque llegó a cinco mensajes, incluidos dos de ayuda; no se inició otro bloque en esta sesión.

- Evaluados/revalidados para invitación: 3; seleccionados del lote existente: 3; nuevas exclusiones: 0; nuevos contactados: 3. Sin nuevos destinatarios fuera del lote.
- Felipe @felipe.cornejo.bermeo: musico B, enviado 16:05, CRM274. Perfil público, For Carla y contrabajo corroborados; historial vacío y sin coincidencia exacta previa.
- Raúl @raulmolina: artista B, enviado 16:06, CRM275. EP Guayaquil y Teatro Variedades Quito corroborados, sin inferir residencia; historial vacío y sin coincidencia exacta previa.
- Hugo @hugocaicedop: fan B, enviado 16:07, CRM276. Música popular compartida y vínculo UIO corroborados; reacción TDF documentada en selección previa; historial vacío y sin coincidencia exacta previa. Invitación como oyente.
- Invitaciones de hoy: A0/B3; segmentos musico1/artista1/fan1. Acumulado: 11 destinatarios únicos, A5/B6. Faltan nueve aprobados: 3,6,9,10,14,16,18,19,20.
- Dos respuestas de ayuda confirmadas: Neil 16:03 (CRM265), explicando plataforma, cuenta y primera acción; Carla 16:04 (CRM266), usando la respuesta autorizada para quien indica que ya está dentro. Notas anexadas y verificadas, sin sobrescribir antecedentes.
- Carla: nueva evidencia recibida miércoles 21:42 y revisada hoy: «Ya estoy aquí» más captura de cuenta activa y creación pendiente del perfil de artista. Registro declarado y respaldado por captura; identidad de cuenta pendiente de vinculación/verificación, CRM exacto sigue hasUserAccount=false. No se alteró ese campo ni se contó activación. Se solicitó únicamente usuario/enlace público.
- Respuestas observadas acumuladas: una positiva (Carla), una neutral/informativa (Neil), cero negativas observadas. Hoy se releyeron estos dos hilos; no se revalidaron todos los otros seis hilos del primer día, así que no se afirma ausencia global de nuevas respuestas o bajas.
- Clics atribuibles/PostHog: no disponibles. Registros: uno informado con captura, cero nuevos verificados contra cuenta exacta en CRM durante este bloque. Perfiles completos y activaciones: no confirmados. No se infiere conversión por un clic, interés o captura de alta solamente.
- Enlaces canónicos revalidados en producción: artista A, artista B, musico B y fan B; formulario Crear e ingresar visible y parámetros preservados. No se completó un alta de prueba. Google no se volvió a confirmar, por eso se omitió de la explicación a Neil.
- Seguimientos por silencio: ninguno enviado. Para contactos del 30/sept, ventana 4–7/oct, tras revisar respuestas y exclusiones. Para nuevas invitaciones del 3/oct, ventana 7–10/oct.
- Riesgos observados en este bloque: cero advertencias/restricciones de Instagram, cero envíos inciertos, cero duplicados accidentales y cero solicitudes de no contacto en los cinco hilos revisados. No es garantía sobre hilos no revisados ni sobre límites seguros de plataforma.
- Objeciones de campaña: ninguna nueva observada; Neil necesitaba una explicación básica. Fuera del piloto apareció en la vista previa de bandeja una consulta sobre diferencia $500/$150, escalada al operador sin responder sobre precios ni condiciones. No se incorpora al CRM de campaña ni a sus métricas.
- Recomendación para el siguiente bloque ya autorizado: primero revisar respuestas pendientes de los otros seis contactados, luego continuar hasta cinco mensajes con perfiles aprobados pendientes. Priorizar ayudar a Carla a completar perfil y primera acción. Sin ganador A/B ni ampliación propuesta aún.

El Reel ya publicado permanece único: https://www.instagram.com/tdf.records.label/reel/Dd71zzlt8TD/ . No se publicó ni adjuntó otro video hoy.


## Corrección obligatoria — 3/oct 16:28 Ecuador

Al continuar tras la aclaración de precios, las pestañas CRM e Instagram volvieron a estar disponibles junto a about:blank. Se seleccionó la pestaña existente de Instagram, sin reconexión ni credenciales. En el hilo de Hugo apareció el aviso: «This account can't receive your message because they don't allow new message requests from everyone.»

Se detuvo la operación sin reintentar ni buscar otro canal. CRM276 recibió una nota fechada correctiva, anexada y verificada. El registro anterior se conserva como auditoría del intento, pero su aparición inicial en el hilo NO demuestra entrega. Hugo queda excluido por configuración de solicitudes y no debe recibir seguimiento.

Corrección de métricas: 11 intentos únicos; 10 contactos con envío confirmado sin fallo observado, A5/B5; un intento no entregado, fan B; nueve aprobados pendientes. Hoy dos nuevas invitaciones sin fallo observado (Felipe y Raúl), un intento no entregado (Hugo) y dos respuestas de ayuda. Los cinco intentos de salida siguen contando para el límite del bloque. No se afirma entrega o lectura de los otros diez más allá de la evidencia disponible.

El aviso describe la configuración del destinatario, no acredita una sanción global a TDF. No hubo envíos nuevos en esta continuación. La aclaración de precios del operador ($500 por 16 horas individuales; página del curso grupal intensivo) sigue preparada y pendiente de envío. Próxima acción requiere atender esta detención antes de retomar operación; no sustituir a Hugo por otro destinatario sin aprobación.


## Consulta externa al piloto transferida — 3/oct 16:30 Ecuador

Tras Continue del operador se retomó la lectura, manteniendo a Hugo excluido. La consulta comercial quedó identificada y se revisó su historial: respuesta anterior de clases DJ individual en vez del curso grupal consultado. El perfil es privado y la información visible no permite evaluar edad. Se transfirió al operador según la regla de edad incierta, sin afirmar que sea menor. No hubo envío, creación de contacto ni cambio en las métricas del piloto. La aclaración de modalidades permanece preparada para atención humana.


## Reporte 3/oct/2026 — segundo bloque del día, cerrado 17:01 Ecuador

Reanudación solicitada por el operador con Continue; Hugo permanece excluido y Danna transferida al operador. No se reintentó el mensaje fallido ni se añadieron destinatarios nuevos.

| Destinatario aprobado | Segmento | Variante | Hora Ecuador | CRM |
|---|---|---|---|---|
| #3 Pakul | artista | A | 16:55 | 277 |
| #6 Lysergicman | dj_productor | A | 16:57 | 35 existente |
| #9 Quito Bohemio | aliado | B | 16:58 | 278 |
| #10 Juan Argüello | dj_productor | B | 16:59 | 279 |
| #14 Isasha Luna | musico | A | 17:00 | 280 |

Cinco perfiles revalidados individualmente, cinco elegibles y enviados, cero exclusiones nuevas. Identidad/detalles y relación musical local corroborados; sin señales observadas de minoría de edad. Hilos vacíos salvo Lysergicman, cuyo último historial2025 no contenía invitación. CRM exacto normalizado completo: cuatro sin coincidencia y Lysergicman35 hasUserAccount=false, notasnull antes de gestión. Formularios canónicos de los cinco enlaces abiertos en producción, Crear e ingresar visible, UTM preservados. No alta de prueba ni medición PostHog. Mensajes idénticos al lote aprobado; cinco confirmaciones visuales en hilo y compositor vacío; al cierre la bandeja todavía muestra esas cinco salidas sin aviso de fallo. No equivale a lectura o entrega demostrada al dispositivo.

CRM creado/actualizado después de cada envío y notas verificadas. Ninguna nota anterior sobrescrita. Cierre al quinto mensaje del bloque. Total diario conservador: diez salidas, ocho intentos de invitación (siete sin fallo observado y Hugo no entregado) y dos respuestas de ayuda. No enviar más hoy para mantener ese cómputo conservador; no se amplió volumen.

### Respuestas y métricas revisadas

Se revisaron los seis hilos antiguos pendientes: Alejandro, Emmanu, Sol, Quito Ska Society, Rommel y Joel. Se anexó el resultado en sus CRM267/268/269/270/271/273.

- Alejandro: «Esta cool ya lo voy a chequear friends» y «🙏🏻»; positivo, interesado.
- Sol: «Hola, muchas gracias por pensar en mi música, qué buena iniciativa. Ahora la miro. Buena noche!»; positivo, interesado.
- Rommel: «Nice!»; respuesta de tono positivo breve, estado respondió. No implica intención explícita de registro.
- Emmanu: invitación con icono corazón, sin nueva respuesta escrita. El icono no identifica por sí solo al autor; se registra reacción visible sin sumarla como respuesta positiva explícita.
- Quito Ska Society y Joel: Seen, sin respuesta escrita; permanecen contactado. No se envió seguimiento.
- Acumulado escrito observado: cuatro positivos (Carla, Alejandro, Sol y el acuse breve de Rommel), uno neutral/informativo (Neil), cero negativos observados. Las revisiones ocurrieron en momentos distintos, no constituyen monitorización continua.
- Acumulado de intentos únicos:16; contactos sin fallo observado:15 (A8/B7); no entregado/excluido:1 (Hugo B); pendientes del lote:4 (#16 Gura A, #18 Los Argonautas A, #19 El Bloque B, #20 Francisco B).
- Registros: Carla lo informó con captura de cuenta activa; vinculación con cuenta exacta pendiente, no se cambió hasUserAccount. Cero nuevas altas confirmadas en las comprobaciones exactas del CRM. No equiparar hasUserAccount=false con certeza de que no exista cuenta bajo otra identidad.
- Perfiles completos, activaciones y clics atribuibles: no disponibles/no confirmados. Ninguna cifra inventada.
- Solicitudes de no contacto/rechazos de pertinencia: cero observados en los hilos revisados. Duplicados accidentales: cero observados. Advertencias o restricciones nuevas: ninguna en este bloque; se conserva el incidente anterior de configuración de solicitudes de Hugo, que no prueba una sanción global.
- Segmentos enviados hoy sin fallo observado: artista2, musico2, dj_productor2, aliado1; siete invitaciones. Por variante A3/B4. Añadir por separado el intento fallido fan B y dos respuestas de ayuda (artista A/B).

### Pendientes y recomendaciones

Cuatro invitaciones aprobadas para un próximo día/bloque, tras revalidar historial/CRM. Hugo no se sustituye sin aprobación. Danna permanece fuera del piloto y transferida al operador, sin respuesta enviada por el agente.

Seguimientos: ninguno enviado. Revisar los contactos sin respuesta del 30/sept dentro de 4–7/oct, con máximo un seguimiento; hoy no corresponde. Nuevos contactos del 3/oct: ventana7–10/oct. No seguir a Hugo. Priorizar verificar la cuenta pública que Carla comparta y ayudarla con perfil/primera acción; respetar silencio de los demás.

No se propone ampliación: faltan registros verificados/activación y terminar el lote. No ganador A/B: dos respuestas de tono positivo por variante; el volumen y la fuerza de intención difieren. No declarar significancia. Reel sin nueva publicación ni adjuntos.


## Revisión sin envíos — 3/oct 18:40 Ecuador

Continúa el cierre conservador de envíos del día. Cero mensajes, publicaciones, seguimientos o invitaciones enviados en esta revisión. El cupo original se expresa como diez invitaciones/día; la operación de hoy adoptó expresamente el cómputo más conservador de diez salidas totales, incluidas dos respuestas y el intento fallido. No se presenta esa decisión como una restricción universal de Instagram.

CRM disponible:273 contactos, lectura completa sin truncamiento. Los16 contactos ya gestionados y el contacto previo El Bloque siguen hasUserAccount=false en sus coincidencias exactas; no duplicados exactos observados. Esto no descarta registros bajo otra identidad. Gura/Argonautas/Francisco sin coincidencia exacta; El Bloque CRM2 notas vacías. No se creó ni modificó ninguno de los cuatro pendientes.

Se reabrieron y leyeron los hilos Carla, Neil, Felipe y Raúl: no nuevos mensajes frente al último registro; Carla no ha aportado usuario/enlace público. La vista de bandeja recargada muestra las cinco invitaciones del bloque17:00 como últimas salidas sin nueva respuesta escrita ni aviso de fallo en esas vistas. No se releen los otros seis hilos antiguos en esta revisión, por lo que no se afirma ausencia global de novedades. La restricción conocida de Hugo permanece excluida, sin reintento. Sin alertas nuevas observadas.

Las cuatro invitaciones restantes conservan su texto exacto aprobado de cinco líneas, dominio www.tdfrecords.net y UTM verificados en el documento: Gura artistaA, Los Argonautas artistaA, El Bloque artistaB y Francisco musicoB. Preparadas para la próxima jornada (4/oct,10:00–22:00 Ecuador), sujetas a revalidar perfiles/historial/CRM antes de envío. No requiere repetir aprobación del mismo lote; no se creó una tarea programada ni se promete ejecución automática.

Métricas sin cambios comprobados:15 contactados sin fallo observado,1excluido por solicitud no entregada,4pendientes;4respuestas escritas de tono positivo conocidas y1neutral; registro de Carla informado con captura pero no vinculado; perfiles completos/activaciones/clics no confirmados. No atribuir cero conversiones reales a falta de medición. Se recomienda completar las cuatro invitaciones antes de proponer ampliación y verificar la identidad pública de Carla cuando responda.


## Inicio diario solicitado — 3/oct/2026

Diego solicita inicio automático permanente a las 08:00 de Ecuador. Se actualiza la ventana a 08:00–22:00 America/Guayaquil; no se amplían los cupos ni el lote aprobado. Próximo inicio solicitado: 4/oct/2026 08:00 Ecuador (13:00 UTC). No se enviaron mensajes al cambiar el horario.

Estado técnico: no hay herramienta de tareas programadas del chat disponible en esta sesión. Las herramientas de programación encontradas pertenecen a Pages o Sites y no acreditan acceso al navegador adjunto de esta campaña; no se creó una automatización sustitutiva ni un bot externo. La documentación oficial indica que las tareas se gestionan en Scheduled de ChatGPT web/desktop; esta comprobación no equivale a activar una tarea. Fuente: https://learn.chatgpt.com/docs/automations .

Configuración preparada, aún NO activada: recurrencia diaria a las 08:00, zona America/Guayaquil, sin fecha de fin, en el mismo chat con acceso al proyecto y navegador adjunto. Instrucción:

> Retoma la campaña Tu escena, conectada de TDF Records. Lee el estado y las autorizaciones vigentes en docs/campaigns/tu-escena-conectada-revision-2026-09-30.md y las notas recientes. Opera solo entre 08:00 y 22:00 America/Guayaquil. Revisa respuestas, CRM e historial y continúa únicamente los destinatarios y textos ya aprobados que sigan pendientes. Usa exclusivamente el navegador adjunto para Instagram, sin scripts, bots, APIs ni macros. Máximo 5 mensajes por bloque y 10 invitaciones por día; cuenta acciones previas antes de actuar. Revalida individualmente perfil, exclusiones y coincidencia exacta Instagram/hasUserAccount en CRM antes de cada envío; registra gestión solo después de confirmar el envío, conservando notas. Usa www.tdfrecords.net. No repitas mensajes, no reemplaces excluidos, no publiques otra vez el Reel y no contactes a Danna ni a Hugo. Si falta navegador/CRM o aparece advertencia, CAPTCHA, verificación, envío incierto u otra condición de detención, detente e informa sin reintentar ni buscar otro canal. Al agotarse el lote, informa y prepara el siguiente para aprobación, sin enviarlo. Respeta los seguimientos de 4–7 días y su aprobación pendiente de texto. Entrega el informe diario con datos comprobados y limitaciones; escala los casos sensibles.


## Automatización ACTIVADA — 4/oct/2026 11:31 Ecuador

Tras el usuario iniciar sesión, se creó y verificó por la interfaz normal de ChatGPT Scheduled una tarea diaria para TDF. ID: 6ac27f594a088190980ae08e3ae4fd69. Recurrencia Daily, hora 8:00 AM Ecuador Time, End repeat Never, Start each run in new chat desactivado; modelo/esfuerzo por defecto sin cambiar. La confirmación muestra: “Your task is scheduled. Its next run is Oct 5, 8:00 AM GMT-5.” La lista Upcoming muestra Tomorrow 8 AM · Daily.

Enlace: https://chatgpt.com/scheduled?automationId=6ac27f594a088190980ae08e3ae4fd69&automationSource=cloud

Esta verificación sustituye los bloqueos históricos de programación descritos arriba. Se guardaron instrucciones autosuficientes, estado previo, exclusiones, límites10invitaciones/día y5mensajes/bloque, controlesCRM/historial, prohibición de duplicar Reel/invitaciones y los cuatro textos exactos aprobados pendientes. No autoriza nuevos lotes. No se ejecutó Run now ni se enviaron mensajes de Instagram en este turno.

Limitación material: la tarea es CLOUD. Programación activa confirmada NO prueba acceso futuro al navegador adjunto local ni al CRM. El prompt exige comprobar ambos al inicio y detenerse/reportar si faltan; no puede usar navegador alternativo, APIs de Instagram ni credenciales compartidas. No se garantiza ejecución de DMs hasta comprobar esos accesos. No se ha sustituido la tarea por un recordatorio ni se ha afirmado que la primera ejecución ya ocurrió.


## Informe domingo 4/oct/2026 — 11:53 Ecuador

Usuario ordenó incluir domingos y comenzar ahora. La tarea ya era diaria; Run now ejecutado a11:46, resultado verificable https://chatgpt.com/c/6ac282c7-51a0-83e8-a38f-c60207b0cbff : la ejecución cloud reportó falta de navegador autorizado,0envíos/0CRM. Es un bloqueo de acceso del entorno cloud, NO una restricción de Instagram. Se continuó en esta sesión local con el navegador adjunto recuperado, tras comprobar sesión oficial y CRM disponible.

CRM al inicio:273contactos completos; match exactoElBloque2hasUserAccountfalse, restantes3sinmatch. Revalidación individual de4perfiles y4historiales sin invitación/rechazo; sin nuevas dudas de edad/identidad/pertinencia observadas. Enviados4mensajes exactos aprobados de5líneas con dominio www.tdfrecords.net yUTM; ninguna sustitución de destinatario. Confirmación de salida en cada hilo y compositorvacío sin fallo observado; bandeja final muestra los4mensajes. Esto NO acredita entrega o lectura.

| Destinatario | Segmento/variante | Hora Ecuador | CRM |
|---|---|---|---|
| gura.a.music | artista A | 11:47 | 281 creado |
| losargonautas.ec | artista A | 11:48 | 282 creado como empresa/banda |
| elbloque_oficial | artista B | 11:49 | 2 existente conservado |
| franlaurito | musico B | 11:51 | 283 creado |

Notas fechadas y mensajes completos verificados en CRM, sin sobrescribir. Hoy4evaluados/revalidados,4seleccionadosyaaprobados,0nuevosexcluidos,4contactados; bloque cerrado. Acumulado19contactadossinfalloA10/B9 y1fallidoexcluidoHugoB; cero nuevos pendientes dentro del lote. No reintentos, duplicados ni menores observados; no nuevos avisos/restricciones de Instagram observados. Cero respuestas nuevas observadas en estos4hilos al cierre; los15anteriores NO se reevaluaron en esta sesión. La base histórica sigue4positivasescritas/1neutral, no confundir con revisión diaria completa. No nuevasobjecionesobservadas en estos4. Clics no disponibles/PostHogsinverificar; Carla mantiene registro autoreportado con captura pendiente de identidad; nuevos registros, perfiles completos y activaciones NO comprobados hoy.

Seguimientos: primeros30sept ventana4–7oct; grupo3oct7–10oct; estos4del4oct8–11oct. Ninguno enviado; texto requiere aprobación conforme al documento vigente. No reemplazarHugo; Danna permanece escalada. Recomendación: observar respuestas y verificar cuentas/activación antes de plantear ampliación; todavía no se acreditan dosregistros yunaactivación. Sin ganadorA/B. No nuevo lote enviado ni aprobado.

Tarea6ac27f594a088190980ae08e3ae4fd69 actualizada medianteEdit/Save: nombre TDF — Tu escena, conectada · diario08:00, Daily08:00Ecuador/Never. Retirados los4textos pendientes del prompt; estado19+1 y cero invitaciones autorizadas pendientes, objetivo revisarrespuestas/activar/reportar y preparar propuesta para aprobación. Domingos explícitos. Cloud queda programada pero SIN acceso al navegador: esa dependencia debe resolverse para operación desatendida; no prometer DMsautomáticos. Próximarun5oct08:00. No nuevaejecucióncloud disparada despuésdelSave.


## Revisión de los 19 hilos — domingo4/oct,19:48 Ecuador

Revisión individual mediante navegador adjunto de los19contactados. CRMcompleto276contactos:19matchesexactos, cadauno hasUserAccount=false; sin duplicados exactos detectados. Esto NO demuestra ausencia de registros realizados con otrosdatos. No se consultaron datos privados ajenos ni se cambióhasUserAccount.

Novedades: Francisco(franlaurito,CRM283,musicoB) respondió14:10 «Hola que tal? Suena interesante», «Cuénteme un poco mas», «Saludos!». Se respondió19:42 con información ya verificada de perfiles, registrocorreo/contraseña yseguirprimerproyecto; salida única visible sinfallo ycompositorvacío, notaCRMverificada. Estadointeresado. Carla(CRM266,artistaB) respondió08:20 «Super !! Gracias»; sinusuariopúblico/enlaceTDF, no nuevaconversiónconfirmada; notaCRMappendverificada, sinotroenvío.

Francisco respuesta exacta enviada19:42:

```text
¡Hola, Francisco! TDF es una plataforma para conectar proyectos musicales y gente que vive la escena de Quito.
Puedes explorar perfiles de artistas y lanzamientos, seguir proyectos y crear tu perfil público de artista con biografía y enlaces a tu música.
El enlace que te compartimos abre el registro general: puedes crear tu cuenta con correo y contraseña, elegir el rol que te corresponda y empezar siguiendo un proyecto.
Si te animas a probarla, cuéntanos qué te resulta útil o qué te falta para tu música. — TDF Records
```

Sin nuevos textos en Neil, Emmanu, Alejandro, Sol, Rommel, QuitoSka, Joel, Gura, Argonautas, Bloque, Pakul, Lysergic, QuitoBohemio, Juan, Felipe, Isasha yRaúl. Emmanu conservacorazónsinverificarautor(no nuevopositivo). QuitoSka/JoelSeen; Pakul/LysergicSeen yesterday. No se enviaron seguimientos. Hilos ajenos al piloto aparecieron en bandeja pero no se gestionaron/contabilizaron.

Métricas actualizadas:19contactadossinfalloA10/B9 y1HugofallidoexcluidoB;5personasconrespuestaescritapositiva(A2/B3:Carla,Alejandro,Sol,Rommelbreve,Francisco),1neutralNeil;0negativas/solicitudesnocontacto observadas en estos19hilos. Carla yFrancisco tienen nuevasrespuestaspositivashoy, pero soloFrancisco sumaunapersonapositivanueva. ClicsPostHogdesconocidos.0nuevosregistrosvinculadosporCRM;1registroautoreportadoconcapturaCarla pendienteidentidad; perfilescompletos/activacionesno confirmados(no afirmar cero reales). No nuevosavisos/restriccionesInstagram/fallosdeenvíoobservados. Sin nuevasobjecionesrecurrentes; pregunta recurrente comprobadaNeil/Francisco:«dequésetrata».

Hoytotal4invitaciones+1respuestaayuda=5salidas,2bloques(4mañana+1tarde). Esta continuación0nuevosseleccionados/excluidos/invitaciones,19hilosrevisados,1respuestaayuda. No creaciónCRMadicional. Recomendación: aclararbeneficio/primeraacción en próximosmensajes; priorizarvincularregistroCarla yorientarFrancisco, sin insistir cuando nohaypregunta. No ampliarenvíos: faltan2registrosconfirmados/1activación yperiodoobservación; A2vsB3 no duplica, no ganador.

Seguimientos iniciales posibles5–7oct (≥4días completos desde30septnoche), solo unoytextoaprobado; restogrupo3oct7–10oct,grupo4oct8–11oct. Para evitarcontactos innecesarios, propuestos soloJoel yQuitoSka(sinrespuesta), excluyendo agradecimientospositivosyEmmanureacción. Necesitaaprobaciónexpresa de losdos textos siguientes yrevalidaciónantesenvío. No son nuevo lote de adquisición ni autorizaciónparaampliarpiloto.

### Seguimiento único — APROBADO 4/oct 19:50 / NO enviado

**joelduenasc · musico A · CRM273 sincuenta · enviar5–7oct**

```text
Hola, Joel 👋 Te dejamos un único recordatorio de la invitación que te enviamos por tu trabajo con Delirio.
Puedes empezar explorando un proyecto y seguirlo desde tu cuenta de TDF:
https://www.tdfrecords.net/login?signup=1&intent=professional_tools&roles=Producer&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=musico_a
Si no te interesa, todo bien; aquí dejamos la invitación y no volvemos a escribirte. — TDF Records
```

**quitoskasociety · aliado A · CRM270 sincuenta · enviar5–7oct**

```text
Hola, equipo de Quito Ska Society 👋 Un único recordatorio de nuestra invitación, pensando en su trabajo alrededor de Ska Ba Boom.
Pueden explorar los perfiles y lanzamientos de TDF y contarnos si les servirían para su labor con la escena:
https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=aliado_a
Si no les interesa, todo bien; aquí dejamos la invitación y no volvemos a escribirles. — TDF Records
```


## Aprobación de dos seguimientos — 4/oct/2026 19:50 Ecuador

Diego respondió «Aprobado» después de recibir en el chat los dos textos íntegros de Joel(@joelduenasc,CRM273,musicoA) y Quito Ska Society(@quitoskasociety,CRM270,aliadoA). Autoriza únicamente esos destinatarios y textos exactos, un seguimiento por persona entre5–7oct2026, horario08:00–22:00Ecuador. Revalidarhistorial/CRM/perfil antesdeenvío, omitirsiyarespondió/cuentacreada/nocontacto/seguimientoprevio/otraexclusión. No pedir nuevamente aprobación; no amplía el piloto. Hoy4octnoenvío por ventanafutura. No se escribe gestiónCRM por meraaprobación.

TareaScheduled6ac27f594a088190980ae08e3ae4fd69 actualizada porUIEdit/Save a19:51 con ambosmensajesexactos, autorizaciónfechada, ventana5–7oct, controlesdeduplicación ycaducidad. SigueDaily08:00Ecuador/Never/domingo incluido. Accesocloudalnavegadorcontinúabloqueado segúnpruebaanterior; guardardosseguimientosnoresuelveelbloqueo. No se disparó Run now ni se enviaron mensajes en este turno. Métricas19contactos/1excluido/5positivas/1neutral,activaciónsinconfirmar.


## Revisión de cierre — 4/oct/2026 21:50–21:56 Ecuador

CRM disponible: 277 contactos en respuesta completa, coincidencias exactas Carla 266, Quito Ska Society 270, Joel 273 y Francisco 283; hasUserAccount=false en los cuatro. El cambio de cantidad total del CRM no se atribuye a la campaña. Bandeja y tres hilos abiertos individualmente mediante navegador adjunto: Joel y Quito Ska Society siguen sin respuesta ni seguimiento; Carla mantiene «Super !! Gracias» de las 08:20 sin usuario/enlace público TDF adicional. Francisco conserva como último mensaje visible en bandeja nuestra respuesta de las 19:42. Sin nuevas señales visibles de restricción de Instagram.

Esta revisión: 0 envíos, 0 contactos nuevos, 0 modificaciones CRM. Los dos seguimientos autorizados siguen NO enviados; ventana 5–7/oct, 08:00–22:00 Ecuador, sujetos a revalidación antes del envío. Acumulado sin cambios: 19 contactados sin fallo observado, 1 intento no entregado/excluido, 5 respuestas escritas positivas, 1 neutral; registro de Carla autorreportado pendiente de vinculación, perfiles completos y activaciones sin confirmar. Automatización diaria activa; persiste la limitación ya comprobada de acceso del entorno cloud al navegador adjunto.


## Campaña: acceso bloqueado al inicio — 5/oct/2026 08:02 Ecuador

La continuación comenzó dentro de la ventana autorizada. Se releyeron los dos textos aprobados para Joel (CRM273) y Quito Ska Society (CRM270), autorizados 5–7/oct, 08:00–22:00. Primer intento de listar pestañas del navegador adjunto falló: Browser is already in use for el perfil tdf-instagram. No se usó modo aislado, no se cerraron procesos ni se intentó eludir el bloqueo, siguiendo la detención obligatoria indicada por Diego.

No fue posible revalidar hoy CRM, perfiles, conversaciones ni resultados de la tarea cloud. Esta ejecución: 0 mensajes, 0 contactos nuevos, 0 modificaciones CRM. Los dos seguimientos continúan pendientes; aprobación vigente, sin necesidad de volver a pedirla. Los datos acumulados al cierre del 4/oct son históricos y no constituyen una revisión del 5/oct. Se necesita restablecer el acceso al navegador adjunto antes de continuar. Este error de conexión no demuestra una restricción de Instagram.


## Reconexión y seguimientos completados — 5/oct/2026 09:02–09:08 Ecuador

Diego autorizó explícitamente reconectar el navegador y continuar. Se cerró con SIGTERM únicamente el proceso Chrome11100 identificado como propietario exacto del perfil tdf-instagram, conservando todos los datos; navegador adjunto recuperado y sesiones TDF/Instagram activas. No modo aislado ni lectura de credenciales. CRM completo277 contactos: coincidencias exactas273joelduenasc y270quitoskasociety, amboshasUserAccount=false y sin seguimiento anterior. Perfiles públicos e historiales revalidados individualmente.

Seguimientos exactos aprobados el4/oct enviados UNA VEZ: Joel09:04 (musico A,CRM273), Quito Ska Society09:06 (aliado A,CRM270). Ambos visibles en el hilo con compositor vacío y sin fallo de envío visible. CRM actualizado tras cada envío, lectura de verificación correcta, notas anteriores preservadas. Seguimiento único CONSUMIDO para ambos: no volver a enviar recordatorios. La etiqueta histórica «NO enviado» arriba queda supersedida por esta ejecución.

Nueva respuesta positiva de Quito Bohemio, varianteB,CRM278: agradece, declara haber revisado la página, propone amplificar la difusión de artistas y produccionesTDF y pregunta «Cuéntame cómo le tienen pensado ustedes la viabilidad de trabajo en conjunto?». Estado escalado, registrado enCRM y transferido aDiego enchat inmediatamente, sin respuestaInstagram ni promesas. No enviarle seguimiento. Visita autorreportada; no es clic medido.

Reporte parcial5/oct:2 perfiles/hilos de seguimiento revalidados+1hilo con propuesta leído;0 nuevos seleccionados/excluidos/contactados únicos;2seguimientos(A2/B0:musico1/aliado1),1nuevo positivo aliadoB escalado,0nuevos neutros/negativos/optouts observados en estos hilos. Acumulado conocido19contactadossinfalloA10/B9+1Hugofallidoexcluido;6respuestaspositivas escritasA2/B4(incluyeRommelbreve yQuitoBohemioalianza),1neutralNeil. B duplica A de forma descriptiva; muestra pequeña, sin ganador estadístico ni autorización para ampliar. RegistroCarlaautorreportado sinvinculación; registrosconfirmados/perfilescompletos/activaciones siguen sinconfirmar. Clics atribuibles no consultados. Sin nuevas advertencias/restricciones visibles durante esta ejecución. La reconexión local NO prueba acceso de la tarea cloud. No quedan seguimientos aprobados pendientes.


## Quito Bohemio: respuesta autorizada enviada — 5/oct/2026 09:12 Ecuador

Diego aprobó el texto completo propuesto con «Ok, envíalo». CRM278 exacto quitobohemio revalidado (hasUserAccount=false), hilo sin nuevas respuestas ni duplicados. Enviado una vez a09:12 y confirmado visible con compositor vacío, sin fallo de envío observado. CRM278 actualizado con texto íntegro, autorización y verificación, preservando notas anteriores.

Mensaje: «¡Gracias por revisar la plataforma, amigos! Nos interesa esa idea de amplificar lo que está pasando en la escena de Quito 🤟

Podríamos empezar con una colaboración puntual alrededor de un lanzamiento: conectar al proyecto con ustedes y acordar juntos un formato de difusión que tenga sentido para su comunidad.

¿Cómo suelen trabajar estas colaboraciones? Nos serviría conocer sus formatos, qué necesitarían de TDF y si manejan tarifas o intercambio, para armar una propuesta concreta.

— TDF Records»

Estado escalado; pendiente respuesta sobre formatos/necesidades/tarifas/intercambio. No hay alianza cerrada ni autorización general de negociación; nuevas condiciones a Diego. Total salidas5/oct:3 (2seguimientosA+1respuestaaliadoB),0nuevasinvitaciones, mismo bloque matinal por criterio conservador, dentro del límite5. Acumulado19contactados/6positivas sin cambios; no nuevas métricas de registro/activación.


## Revisión posterior — 5/oct/2026 09:14–09:15 Ecuador

CRM completo277:20 coincidencias exactas del lote (incluidoHugoexcluido), todas hasUserAccount=false. Esto significa sin cuenta vinculada en esos contactos; NO prueba que no haya registros con otra identidad. Bandeja actualizada y hilos QuitoBohemio/Carla abiertos individualmente. QuitoBohemio «Liked a message» atribuido en bandeja, sin nuevo texto tras respuesta09:12; nota CRM278 añadida/verificada. No sumar nueva persona positiva ni interpretar como acuerdo. Carla mantiene último «Super !! Gracias» domingo08:20 sin usuario/enlace público adicional; no insistencia. Joel/QuitoSka/Francisco muestran nuestras respuestas como últimas en bandeja. Hilos publicitarios ajenos al piloto no gestionados.

Esta continuación0envíos/0nuevoscontactos/0nuevosexcluidos. Totalhoy3salidas (2seguimientosA,1respuestaaliadoB),0invitacionesnuevas. Acumulado19contactadossinfallo+1excluido,6positivasA2/B4,1neutral conocido; sin nuevosrechazos/optouts/avisos visibles enloshilosrevisados. Clics no consultados, perfilescompletos/activaciones no confirmados, Carla registroautorreportado pendientevincular. Ningún seguimiento aprobado pendiente. Pendiente: condiciones QuitoBohemio paraDiego y respuestas espontáneas; no ampliar piloto sin criterioscumplidos y nuevaaprobación.


## Revisión nocturna — 5/oct/2026 20:20–20:23 Ecuador

Navegador adjunto disponible, sesión CRM/Instagram activa. CRM completo278contactos:20coincidencias exactas del piloto, todas hasUserAccount=false. Aumento277→278noatribuidoacampaña; cuentas sinvincular no equivalen a ausencia absoluta de registros. Bandeja y dos hilos de alianza revisados individualmente.

QuitoBohemio propone pruebas gratuitas para unas3producciones; tarifa habitual mencionada$20 por difusión en colaboración/historias/web, unidad/entregables/calendario/métricas sinaclarar. QuitoSkaSociety respondió19:38: no entiende propuesta, suponeposiblegrabaciónenTDFconMariannoGoldenstein, autogestiónesteaño y aperturaaideas. Ambas propuestas escaladas inmediatamente aDiego; CRM278/270 con entradas fechadas añadidas y verificadas, notaspreviaspreservadas. No respuesta ni aceptación comercial. QuitoSka clasificado neutral conapertura, no rechazo/no-contacto ni interésenregistro confirmado.

Reporte: esta continuación0envíos/0nuevosseleccionados/excluidos/contactados;2hilos nuevosgestionados. Totalhoy3salidas (musicoAseguimiento1,aliadoAseguimiento1,aliadoBrespuesta1),0nuevasinvitaciones. Respuestas nuevas dehoy:1personapositivaQuitoBohemio y1neutralabiertaQuitoSka; no sumar reacción/segundarespuestaBohemio como nueva persona. Acumulado19contactadosA10/B9+1Hugointentonotentregadoexcluido;6positivasA2/B4,2neutrales conocidosNeil/QuitoSka,0rechazos/optouts nuevosobservados. Registrosvinculadosno confirmados;Carlaautorreportadopendientevincular; perfilescompletos/activacionesnoconsignados, clicsPostHogno consultados. Sin advertencias/restriccionesInstagram visibles. Bandeja Joel/Francisco sigueúltimosmensajessalientes; no revisiónexhaustiva19hilosesta noche.

Riesgo concreto: invitación a plataforma interpretada como colaboración de estudio. Recomendación: aclarar aQuitoSka el propósito plataforma y pedirnecesidadconcreta antesdeofrecergrabación; conBohemio definirformatos/fechas/materiales/medición antesdecomprometer3producciones. PendientedecisiónDiego; no quedan seguimientosautorizadospendientes, no ampliaciónpiloto.


## Dos respuestas de alianza autorizadas y enviadas — 5/oct/2026 20:27 Ecuador

Diego aprobó ambos textos completos con «Ok, envíalos». CRM disponible completo278, coincidencias exactas278quitobohemio/270quitoskasociety hasUserAccount=false. Hilos revalidados individualmente sin nuevas respuestas ni duplicados. Ambos textos enviados UNA VEZ a20:27, visibles en conversación con compositor vacío y sin fallo de envío visible. Notas CRM añadidas después de cada envío y verificadas, conservando todo el historial. Estado escalado bajo decisión deDiego; no negociación general autorizada, proyectos/fechas/contraprestaciones pendientes.

Quito Bohemio (aliado B):
¡Gracias, amigos! Nos interesa explorar esa prueba con tres producciones 🤟
Antes de confirmar los proyectos, ¿qué incluiría la difusión de cada uno: publicación colaborativa, historias y nota en la web?
Cuéntennos también qué materiales necesitan, qué fechas proponen y qué métricas podrían compartir después.
Con eso podemos revisar los proyectos y concretar juntos el alcance de la prueba gratuita.
— TDF Records

Quito Ska Society (aliado A):
¡Gracias por contarnos, equipo! Nos faltó explicar mejor la invitación.
Les escribíamos para conocer la plataforma de TDF, donde pueden explorar perfiles de artistas y lanzamientos, y decirnos qué les serviría para su comunidad.
La idea de grabar con Marianno sería una propuesta aparte, que tendríamos que revisar con Diego.
Pensando en Ska Ba Boom y su autogestión, ¿qué apoyo concreto están buscando? Así podemos evaluar una colaboración con sentido para ambos.
— TDF Records

Total5/oct:5salidas de campaña (2seguimientosA+2respuestasaliadoB+1respuestaaliadoA),0nuevasinvitaciones, bloques3mañana/2noche. Respuestas a consultas, no nuevos recordatorios. Acumulado19contactados+1excluido,6positivas/2neutrales conocidas; registro/perfil/activación sin nueva confirmación. Sin nuevosavisosInstagram visibles. No quedan mensajes aprobados pendientes. Esperar respuestas y escalar condiciones aDiego.


## Cambio posterior detectado — 5/oct/2026 20:29 Ecuador

Continuación solicitada por Diego. Primer listado muestra pestañaInstagram en hilo distinto100439431354429, no gestionado por este agente. Se abre bandeja normal: QuitoBohemio conserva último texto aprobado enviado20:27; QuitoSkaSociety muestra ahora una modificación: «Les escribíamos para conocer https://www.tdfrecords.net , la plataforma de TDF…». El envío original20:27 verificado por este agente decía «Les escribíamos para conocer la plataforma de TDF…», sin ese enlace. No atribuir cambio a usuario/otrooperador/automatización sinconfirmación; no se detectó duplicado ni restricción. Bandeja no muestra nuevas respuestas enambos, Joel/Francisco conservanúltimosmensajesnuestros.

Se detuvo la operación ante cambio posterior inesperado segúnreglaDiego, sin nuevosenvíos/ediciones/reintentos ni cambiosCRM. No reescribir registroCRM del envío original correcto con una edición deautor/origen desconocido. Necesaria aclaración sobre edición posterior/operador antes de continuar. Totalenvíos propios hoy5sin cambios; no revisiónnuevaCRM eneste turno.


## Edición manual aclarada y enlace recomendado — 5/oct/2026 20:35 Ecuador

[2026-10-05 20:35 America/Guayaquil]
Diego confirmó que añadió personalmente https://www.tdfrecords.net al mensaje20:27 y autorizó sustituirlo por URL más apropiada. Cambio manual confirmado: «Les escribíamos para conocer https://www.tdfrecords.net , la plataforma de TDF…»; resto observado sin cambio. Se conserva el registro del envío original. Enlace sugerido de registro general sin rol impuesto: https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=aliado_a
Código LoginPage admite signup=1; producción con sesión activa redirige correctamente a/fans. No se probó alta nueva. Menú del mensaje en navegador adjunto solo ofrece Forward, Copy, Translate, Pin y Unsend; no Editar. NO se modificó, anuló ni reenvió el mensaje. La autorización de edición no autoriza anular/reenviar. Enlace exacto entregado a Diego para posible edición desde app. Ningún envío adicional; origen del cambio aclarado, bloqueo anterior resuelto.


## Sustitución autorizada completada — 5/oct/2026 20:39 Ecuador

[2026-10-05 20:39 America/Guayaquil]
Campaña tu_escena_conectada_piloto; aliado A; estado escalado. Autorización explícita de Diego: «Haz Unsend y vuélve a enviar con el URL apropiado». CRM exacto e historial revalidados, sin nuevas respuestas. Se anuló SOLO el mensaje20:27 posteriormente editado por Diego con URL raíz; confirmación Unsend aceptada y ausencia del mensaje verificada antes del reenvío. Se envió UNA VEZ el reemplazo20:39, sustituyendo únicamente URL por registro general+redirect/fans+UTM aliado_a y conservando resto del texto. Nuevo mensaje visible, enlace exacto clicable, compositor vacío, sin fallo de envío visible. No duplicado accidental ni nuevo seguimiento; corrección autorizada. Conservados registros históricos de original/edición. Total salidas campaña5/oct=6(incluye original anulado y reemplazo); 3bloque matinal+3nocturno, 0nuevasinvitaciones. Condiciones de colaboración siguen pendientes de Diego.
Mensaje sustituto enviado:
¡Gracias por contarnos, equipo! Nos faltó explicar mejor la invitación.
Les escribíamos para conocer https://www.tdfrecords.net/login?signup=1&redirect=%2Ffans&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=aliado_a , la plataforma de TDF, donde pueden explorar perfiles de artistas y lanzamientos, y decirnos qué les serviría para su comunidad.
La idea de grabar con Marianno sería una propuesta aparte, que tendríamos que revisar con Diego.
Pensando en Ska Ba Boom y su autogestión, ¿qué apoyo concreto están buscando? Así podemos evaluar una colaboración con sentido para ambos.
— TDF Records


## Revisión de cierre — 5/oct/2026 22:43–22:44 Ecuador

[2026-10-05 22:44 America/Guayaquil]
Campaña tu_escena_conectada_piloto; aliado B; estado escalado. Dos respuestas recibidas21:35 y leídas22:43: «A ver en este aspecto nos conviene ver las métricas de Instagram entonces para poner en colaboración todo y ver desde ahí las métricas»; «De nuestra parte podemos complementar con posteos en la web y notas de prensa complementarias para afianzar la comunicación».
Se transferió a Diego sin responder ni aceptar condiciones. Mantienen propuesta de publicaciones colaborativas con medición Instagram y complementos web/prensa; todavía sin calendario, materiales, cantidad por producción, métricas específicas o fecha de reporte. No se ha aceptado la prueba ni confirmado artistas. Fuera de horario de envíos08–22, sin nuevos envíos. No suma nueva persona positiva; registro/perfil/activación sin confirmar.

Bandeja QuitoSka conserva reemplazo20:39 comoúltimomensaje; Joel/Francisco conservanúltimasrespuestas nuestras. No revisiónexhaustiva19hilos.0nuevosenvíos/seleccionados/excluidos; totalcampañahoy6salidas(incluyeoriginalanulado+reemplazo),0nuevasinvitaciones. NuevasrespuestaBohemio no aumentan6positivasA2/B4;2neutralesconocidosNeil/QuitoSka. Sin advertenciasInstagram/optouts nuevos visibles. Clics/registros/perfiles/activaciones no revalidados, sin métricas inventadas. Pendiente definir conDiego alcance colaboración y preparar respuesta concreta para aprobación, enviable dentro08–22. CRM278 notas añadidas/verificadas, historialpreservado.
