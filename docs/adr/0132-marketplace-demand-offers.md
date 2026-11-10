# ADR 0132: Marketplace de demanda y oferta sobre clasificados

Estado: propuesto para implementación. Fecha: 2026-11-08. Extiende ADR 0105.

## Contexto

TDF quiere que la Comunidad y los anuncios converjan en un marketplace bilateral: quien busca
(wedding planner, banda, venue, organizador, músico) publica una necesidad con lugar, fecha y
presupuesto; quienes ofrecen responden con precio; el autor compara y elige; al aceptar hay reserva,
pago en la app y comisión para TDF; después, reputación. El directorio musical ya modela anuncios
(`classified`) con categorías de búsqueda y oferta, postulaciones con monto propuesto
(`classified_application.proposed_amount_minor`), invitaciones, DM contextual, favoritos, alertas,
reseñas verificadas, verificación y moderación. El stack de comercio ya modela checkout enlazado a
dominio, comisión de plataforma, depósitos, reembolsos y varios proveedores de pago.

## Decisión

1. **No hay entidad nueva de "anuncio" ni de "oferta"**. `classified` sigue siendo el anuncio y
   `classified_application` la oferta. La intención (`seeking`/`offering`) es un atributo de la
   categoría (`classified_category.intent`), no una tabla paralela.
2. **La negociación es estructurada y alternada.** Cada propuesta o contraoferta es una fila de
   `classified_offer_round` con monto, moneda, términos y vencimiento; nunca se negocia el precio
   por chat. La alternancia estricta y el tope de 10 rondas viven en `TDF.Directory.Policy` y se
   prueban con propiedades.
3. **Cupos y cierre.** El anuncio declara `slots`; aceptar una oferta consume un cupo de forma
   atómica; al agotarse, el anuncio pasa a `filled` y las ofertas abiertas se rechazan con motivo.
   Retirar una oferta aceptada libera el cupo.
4. **Modos de precio.** `open` (solo ofertas), `suggested` (referencia negociable) y `fixed`
   (solo aceptación del precio). El presupuesto reutiliza `budget_min/max_minor` y
   `compensation_type`.
5. **Confidencialidad (I08) se mantiene.** Un oferente solo ve su cadena; el público ve conteo y
   rango agregado.
6. **El acuerdo es un dominio separado del match (Fase 2).** `marketplace_agreement` congela
   términos y montos, calcula comisión en puntos base y depósito con `DepositPolicy`, y se paga con
   el checkout existente (`platform_fee_minor`). No se le llama "escrow". La liquidación a terceros
   empieza manual y verificada; Stripe Connect llega por gate.
7. **La reputación nace del acuerdo completado (Fase 3)**: el acuerdo `completed` inserta
   `directory_interaction(kind='marketplace_agreement')`, que es la única vía para una reseña
   verificada.
8. **Todo detrás de gates del registro de features** (`marketplace.offers`,
   `marketplace.agreements`, `marketplace.reputation`, `marketplace.matching`); fusionar código
   no activa pagos.
9. **Compatibilidad.** Columnas con `DEFAULT`, backfill idempotente, `CHECK` de estado
   reemplazado con rollback que mapea estados nuevos a antiguos, contrato OpenAPI aditivo
   (`proposedAmountMinor` sigue aceptándose junto a `amountMinor`).

## Alternativas rechazadas

- Tabla `marketplace_request` separada de `classified`: duplicaría búsqueda, moderación, alertas,
  favoritos, derivados de artista y el DM contextual.
- Negociar en el chat: el precio final quedaría ambiguo y no habría dato para acuerdo, comisión ni
  métricas.
- Subasta inversa automática (gana el menor precio): el autor elige por precio, reputación y
  términos; el sistema solo ordena.
- Índice único por (anuncio, oferente) activo: fallaría sobre datos históricos con varias
  postulaciones; se valida en el handler y se diagnostica en la migración.
- Cobrar la comisión al comprador por defecto: el take rate del lado de la oferta es la norma del
  sector (inDrive, SoundBetter) y evita que el precio visible difiera del acordado.

## Consecuencias

- Las pantallas actuales de "postulaciones" se convierten en "ofertas" con comparación y
  negociación; la terminología cambia en i18n manteniendo alias.
- `Policy.hs`, `formal-model.yaml` y `PolicySpec` ganan invariantes I18–I20.
- La Fase 2 requiere ADR 0133 para acuerdo, comisión y liquidación, con revisión legal de la
  política de cancelación por país.
