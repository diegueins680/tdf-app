# Tu Escena Conectada — seguidores 2026-09-11

Estado final: se confirmaron cinco invitaciones nuevas y cinco contactos CRM únicos. Cuatro DMs se enviaron dentro de la ventana autorizada; el quinto se envió a las 22:32 de `America/Guayaquil`, fuera de la ventana 09:00–20:00, porque no se revalidó el reloj al retomar una ejecución interrumpida. Se detuvo todo el outreach inmediatamente después de detectar la excepción.

## Resultado

- DMs nuevos confirmados: **5**.
- Contactos CRM únicos revalidados: **5**.
- Conversiones atribuidas a este bloque: **0**; los cinco contactos tienen `hasUserAccount=false`.
- Límite del bloque: alcanzado en cinco mensajes; no se enviaron más.
- Límite diario absoluto: no superado.
- Excepción de cumplimiento: `miguelgallardokeys` recibió el quinto DM a las 22:32, fuera de la ventana permanente. El mensaje ya entregado no se retiró ni se reintentó.

## Ejecución individual

### `comounmadrigal` — antes de las 15:22

- Perfil público y activo de Madrigal, artista solista y vocalista de Swing Original Monks; personalización basada en su publicación reciente “Lo borré” con Mauro Samaniego.
- Se verificó como seguidor actual y sin contacto CRM previo. El hilo estaba vacío; se escogió una sola vez la opción normal `Send message request`, no la alternativa prioritaria pagada.
- Se confirmó una burbuja, compositor vacío y ausencia de alertas.
- CRM: `Madrigal`, Empresa, `partyId 235`, Instagram exacto y `hasUserAccount=false`.
- El minuto exacto no se volvió a abrir en el cierre porque el perfil volvió a presentar el selector entre mensaje prioritario y solicitud normal; se conserva la confirmación visual de la sesión original.

### `da_pawn` — 15:22

- Perfil público y activo de Da Pawn / El Peón; personalización sobre su regreso a escenarios y las fechas anunciadas en Quito y Guayaquil.
- Seguidor actual y CRM libre antes del envío. El hilo solo mostraba dos mensajes históricos no compatibles de 2021, sin campaña previa, objeción ni baja.
- Se confirmó una sola burbuja, un único enlace rastreado, compositor vacío y ausencia de alertas.
- CRM: `Da Pawn`, Empresa, `partyId 233`, Instagram exacto y `hasUserAccount=false`.

### `phia.dj.music` — 15:34

- Perfil público, adulto y activo de DJ; personalización sobre su propuesta de géneros electrónicos.
- Seguidor actual, CRM libre e hilo vacío antes del envío.
- Se confirmó una sola burbuja, un único enlace rastreado, compositor vacío y ausencia de alertas.
- CRM: `PHIA dj`, Persona, `partyId 234`, Instagram exacto y `hasUserAccount=false`.

### `yosoysheen` — 15:42

- Perfil público, adulto y activo de Sheen; personalización sobre `HARD WORK`, Beat Mica Belika y Pezciego.
- Seguidor actual, CRM libre e hilo vacío antes del envío.
- Se confirmó una sola burbuja, un único enlace rastreado, compositor vacío y ausencia de alertas.
- CRM: `Sheen`, Persona, `partyId 236`, Instagram exacto y `hasUserAccount=false`.

### `miguelgallardokeys` — 22:32

- Perfil público, adulto y activo de Miguel Gallardo, pianista de jazz vinculado a Jazz The Roots, con créditos y presentaciones visibles de 2026.
- La verificación inmediata en el modal de seguidores devolvió exactamente `miguelgallardokeys` y `Remove`. La consulta completa de CRM no encontró coincidencia por handle ni por `Miguel Gallardo`.
- El hilo exacto estaba vacío y abrió un compositor normal, sin campaña previa, objeción, baja, alerta ni mensajería prioritaria.
- Se confirmó una sola burbuja mediante una aparición del texto personalizado, una del `utm_content`, una de la línea de baja y una imagen del emoji; el compositor quedó vacío y no hubo alertas.
- CRM: `Miguel Gallardo`, Persona, `partyId 238`, Instagram exacto y `hasUserAccount=false`.
- **Excepción:** el envío ocurrió fuera de la ventana 09:00–20:00. Al detectarlo en el control horario posterior, se detuvo toda actividad saliente.

## Contenido

Cada DM fue un único mensaje en español con:

1. Entre Panas en el Domo con Llama Este Pez — sábado 12 de septiembre, 15:00–18:00, Domo del Pululahua, entrada de 5 dólares y reservas al 0984755301.
2. Invitación a crear el perfil público de artista y reunir la música en TDF.
3. Enlace individual con `utm_content` igual al handle exacto.
4. Solicitud de errores o sugerencias sin compartir contraseñas, códigos ni información sensible.
5. Aclaración de contacto único y baja: si no interesa, TDF no volverá a escribir.

No se promocionó el Listening Party del 10 de septiembre porque ya había vencido.

## Exclusiones destacadas

- CRM existente: `armadadejuguete`, `oskarfrancisdj`, `17coficial`, `fusionimperium`, `byobgb` / Oldboy, `chuleizurieta` y `jennypao2019` / Paola Estévez. No se abrió contacto nuevo.
- Hilo ya contactado: `astumusic.ec` tenía el DM del 4 de septiembre y una respuesta positiva; no se duplicó.
- Entrega ambigua: `aguito1877` pasó los filtros de seguidor y CRM, pero no mostró control `Message` y no resolvió el destinatario exacto en el selector; no se forzó el hilo.
- Fuera del objetivo de artista musical: `cachocachocacho`, `josuerichh`, `dani_paezp`, `goddamn_arttt`, `calandriasincendiandoelespejo`, `psyche_noise_`, `amwordhouse` y otras cuentas personales, gastronómicas, visuales o de medios.
- Privados, inactivos, sin publicaciones o no disponibles: `bkings_777`, `matteo_corttez`, `joseito.ac`, `luisfe_aguirre`, `ptr1903`, `hensan2711`, `lucasxashes`, `itsbrknmind`, `chelovill666`, `chelouio777`, `mati_patel` y otros perfiles descartados durante la lectura.

## Auditoría CRM y señales entrantes

- La lectura final de `GET /parties?limit=500&offset=0` devolvió exactamente una ficha para cada uno de los cinco handles y los tipos esperados: Empresas para Madrigal y Da Pawn; Personas para PHIA dj, Sheen y Miguel Gallardo.
- La interfaz visible del CRM limita la lista a 200 filas. Dos altas duplicadas creadas durante el diagnóstico fueron corregidas sin borrar registros: `partyId 233` se reutilizó para Da Pawn y `partyId 234` para PHIA dj; `partyId 235` quedó como Madrigal. La auditoría completa confirmó unicidad final.
- `partyId 237`, Xavier Nacaza, apareció de forma concurrente como usuario con `hasUserAccount=true`; no pertenece a las cinco altas de este bloque y no se modificó.
- Se observó una respuesta positiva de Nacaza indicando que creó su perfil y quedó en revisión, y una reacción 👍 de Sheen. No se respondió automáticamente y ninguna de estas señales se contó como conversión atribuida al bloque de cinco.

**Cierre auditado:** 5 DMs confirmados, 5 contactos CRM únicos, 0 conversiones atribuidas y 1 excepción horaria documentada.
