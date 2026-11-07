# PATCH CULTURE Vol. 1 — campaña 12–24 de octubre

Evento TDF 141 · sábado 24 de octubre de 2026 · Andes Brewing — La Pradera · USD 20 · 20 cupos · +18.
Estado al 11/oct 22:45 (Ecuador): **20 de 20 cupos disponibles en TDF** (0 vendidos), 12 días para el evento.

## Cambio de cartel

Oracle Saint Rigel (@oracle.saint.rigel) se suma al showcase junto a Llama Este Pez.

- Ficha del evento en TDF actualizada (descripción): «La jornada cierra con los showcases de Oracle Saint Rigel y Llama Este Pez».
- Captions editados en Instagram: carrusel `DeLSzJTDhpB` y reel `DeYKvDajHVT`.
- Pendiente: Oracle Saint Rigel no tiene perfil de artista en TDF (crear uno exige una ficha de persona propia); el nombre del tier sigue en singular («Taller + jam + showcase + pinta»).

Lo que se dice de él en las piezas sale de su perfil público: «Experimental Producer & Graphic Designer», «Lab @elemental.zoser», y su propia descripción de un live («sonido, pulsos y atmósferas»).

## Diagnóstico

1. **Dos taquillas abiertas.** Los enlaces de bio de @elemental.zoser y @oracle.saint.rigel llevan a Meet2Go (`meet2go.com/ev/patchculturevol1`), que vende «General (Final) $20». La decisión del 6/oct era cero ventas en Meet2Go y redirigir a TDF. Son dos inventarios de 20 para un taller de 20, y Meet2Go anuncia jam y live act «abiertos al público», distinto de los términos de TDF.
2. **Cero ventas en TDF** con dos publicaciones hechas (carrusel del 6/oct y reel). El reel estuvo en silencio hasta el 11/oct.
3. El enlace de bio de @tdf.records.label sí incluye el evento (segundo en el Linktree).
4. Las piezas anteriores explican el formato pero no responden objeciones (experiencia, equipo, pago, reembolso) ni dan motivo para decidir ahora.

## Estrategia

- **Objetivo:** vender los 20 cupos; meta intermedia de 10 al domingo 18.
- **Público:** curiosos de la electrónica sin experiencia (objeción: «no sé nada»), músicos y productores que quieren tocar un modular, y las audiencias propias de los dos showcases.
- **Mensajes, en orden:** novedad (segundo showcase) → qué vas a aprender (demostración con sonido) → objeciones resueltas → prueba social y cartel → urgencia real (20 cupos, cierre de transferencias el jueves 22 a las 18:00) → logística.
- **Palancas:** invitaciones de colaborador a los artistas para llegar a sus audiencias; una sola llamada a la acción (link en bio → tdfrecords.net/eventos/141); reels con audio propio sincronizado con la imagen.
- **Regla de veracidad:** ninguna pieza afirma cupos vendidos. Las piezas de venta no deben salir si el evento se agota.

## Calendario (hora de Ecuador)

| Fecha | Pieza | Formato | Objetivo |
|---|---|---|---|
| Lun 12, 19:00 | `lineup-01…04` | Carrusel + historia `story-lineup` | Anunciar a Oracle Saint Rigel; colaboradores: oracle.saint.rigel, elemental.zoser, jam_estepez |
| Mié 14, 19:00 | `main-01…07` | Carrusel principal + historia `story-main` (app, Link Sticker) | Conversión: «No vienes a ver un modular. Vienes a tocarlo.» Brief: `patch-culture-vol-1-brief-instagram-2026-10-12.md`. Colaboradores: oracle.saint.rigel, elemental.zoser, jam_estepez |
| Jue 15, 19:00 | `reel-modulos.mp4` (31 s) | Reel | Oscilador, filtro, envolvente y modulación, con el osciloscopio dibujando el audio real; pide guardar |
| Vie 16, 12:30 | `faq-01…06` | Carrusel + historia `story-faq` | Cuatro objeciones: experiencia, equipo, qué incluye, pago y reembolso |
| Dom 18, 18:00 | `reel-lineup.mp4` (21 s) | Reel | Cartel completo con la pista sintetizada; mismos colaboradores |
| Mar 20, 19:00 | `ultimos-dias` | Imagen + historia | «Este sábado. 20 cupos, no hay más» |
| Jue 22, 10:00 | `story-transferencia` | Historia | Último día de transferencia (18:00) |
| Vie 23, 18:00 | `manana` | Imagen + historia `story-manana` | Logística; caption alterno si se agotó |
| Sáb 24, 09:30 | `story-hoy` | Historia | Día del evento |

Piezas, captions y fuentes: `~/Downloads/PATCH-CULTURE-instagram/campana-oct12-24/` (`captions/`, `fuentes/`).

Las historias con Link Sticker, encuesta o cuenta regresiva (paquete `story-01…04` del 6/oct) solo se pueden publicar desde la app.

## Comunidad

- Responder comentarios y mensajes sobre el evento el mismo día; el carrusel de preguntas invita a dejar dudas.
- Pedir a @oracle.saint.rigel, @elemental.zoser, @jam_estepez y @andesbrewing que acepten las invitaciones de colaborador y compartan cada pieza en historias con el enlace de TDF.
- Mensajes directos a músicos y productores de Quito: no forman parte de esta ejecución; la campaña «Tu escena, conectada» tiene sus propios lotes y aprobaciones.

## Medición

- Diario: cupos restantes (`/public/events/141/tickets`), guardados y compartidos por pieza, clics del Linktree.
- Si el domingo 18 hay menos de 10 vendidos: historia diaria con Link Sticker desde la app y pedir a Andes Brewing difusión en su cuenta.

## Ejecución

- Hecho el 11/oct: ficha del evento, dos captions, 18 imágenes, 2 reels, 7 captions, calendario y publicador (`fuentes/publish.py`, con salto automático de piezas de venta si se agota).
- Programador aprobado por Diego e instalado el 11/oct 23:05: `tdf-ig-campaign.timer` (cada 5 minutos) ejecuta `/opt/tdf/production/instagram/campaign/publish.py`; estado en `state.json`, registro en `log.txt`. La historia del cartel pasó al lunes 12 a las 12:30. Para detenerlo: `systemctl disable --now tdf-ig-campaign.timer`.
- Perfil de artista 52 «Oracle Saint Rigel» creado y vinculado al evento.
- Pendiente: los enlaces de bio de @elemental.zoser y @oracle.saint.rigel siguen apuntando a Meet2Go.

- 12/oct 11:50: carrusel principal renderizado (`fuentes/render-main.mjs`, 7 slides + `story-main`), caption `captions/main.txt`, `schedule.json` local con `main` el mié 14 19:00 y `reel-modulos` movido al jue 15 19:00. Subido al servidor el 12/oct 12:05 con autorización de Diego; `publish.py --check` en verde (13 piezas, 20 cupos). Respaldo del calendario anterior: `schedule.json.bak-20261012`.
