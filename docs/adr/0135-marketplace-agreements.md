# ADR 0135: Acuerdos del marketplace (Fase 2, parte A)

Estado: aceptado para implementación. Fecha: 2026-11-12. Extiende ADR 0134 y ADR 0105.

## Contexto

La Fase 1 (ADR 0134) termina cuando una oferta se acepta: hay precio pactado y un cupo consumido,
pero nada que represente el compromiso, su comisión ni su cierre. El dueño del producto definió el
12 de noviembre de 2026 que TDF cobra 10 % de comisión sobre el monto pactado, que el depósito de
confirmación es 30 % y que la Fase 2 se entrega en dos partes: primero el acuerdo, después los
pagos.

## Decisión

1. **Un acuerdo por oferta aceptada.** Con el gate `marketplace.agreements` activo, aceptar una
   oferta que tiene monto positivo y moneda crea, en la misma transacción, una fila
   `marketplace_agreement`. Ofertas sin monto (intercambio, no remunerado) no crean acuerdo.
2. **Partes según la intención del anuncio.** En un anuncio `seeking` el autor es el comprador
   (busca y paga) y el oferente el proveedor; en uno `offering`, al revés.
3. **Valores congelados.** El acuerdo guarda monto, moneda, términos (los del anuncio combinados
   con los de la oferta), comisión y reparto depósito/saldo calculados con la tarifa vigente
   (`marketplace_agreement_pricing`, por categoría o regla por defecto, editable sin desplegar). Un
   trigger rechaza cualquier cambio posterior de esos valores. `agreementAmounts` es la única
   fórmula: la comisión redondea hacia abajo, el depósito al medio hacia arriba, y
   depósito + saldo = total siempre.
4. **Sin cobro en la parte A.** El acuerdo nace `confirmed` con `settlement_mode='off_platform'`:
   el pago se coordina entre las partes y TDF no cobra depósito ni comisión todavía. Los estados
   `pending_deposit`, `balance_due` y `expired` existen en el modelo para la parte B (checkout),
   pero ninguna solicitud de un participante puede entrar en ellos y un `CHECK` impide usarlos
   con liquidación fuera de la plataforma.
5. **Quién mueve qué.** El proveedor marca el inicio; solo el comprador confirma que se completó
   (el proveedor no se autocertifica, y un estado de pago nunca implica servicio prestado);
   cualquiera de los dos cancela o abre disputa mientras el acuerdo está vivo; solo un
   administrador resuelve una disputa. La relación vive en `TDF.Directory.Policy`, en
   `marketplace_agreement_transition_allowed` (trigger) y en `formal-model.yaml#agreement`, y el
   gate estático las compara.
6. **Cierre con reputación.** Completar un acuerdo registra una `directory_interaction` verificada
   de tipo `marketplace_agreement`; con ella ambas partes pueden dejarse reseña con las reglas de
   reseñas existentes (I12).
7. **Cancelar deshace el compromiso.** Cancelar el acuerdo retira la oferta aceptada y libera su
   cupo (reabre el anuncio cubierto si no venció); retirar la oferta aceptada cancela el acuerdo
   vivo. Un acuerdo completado, en disputa o con saldo pendiente no se deshace retirando la oferta.
8. **Concurrencia.** Mismo orden de bloqueo que ADR 0134, extendido: anuncio → oferta → acuerdo.
   Cada decisión va atada a la versión mostrada (`expectedVersion`).
9. **Transparencia.** Ambas partes ven las mismas cifras: total, depósito, saldo, comisión de TDF y
   neto del proveedor.

## Consecuencias

- Contrato: `GET /directory/agreements`, `GET /directory/agreements/{agreementId}`,
  `PATCH /directory/agreements/{agreementId}/status`; `ClassifiedApplication.agreementId`.
- Migración `2026-11-12_marketplace_agreements` (aditiva, con rollback): tablas de acuerdo y
  tarifas, relación y trigger de transiciones, ampliación de las allowlists de interacción,
  notificación y analítica, y gate (apagado en producción).
- UI web: pestaña «Acuerdos» en `/mis-clasificados` y enlace desde la oferta aceptada.
- Parte B (ADR posterior): checkout del depósito con `domain_order_kind='marketplace_agreement'` y
  `platform_fee_minor = commission_minor`, liquidación, reembolsos según la política de
  cancelación congelada, vencimiento del depósito y resolución de disputas en el panel de
  operaciones.

## Alternativas descartadas

- **Marcar la oferta como `converted` en lugar de crear una entidad.** No deja dónde congelar
  comisión, depósito ni política de cancelación, ni dónde colgar el cobro.
- **Calcular comisión y depósito al cobrar.** Un cambio de tarifa entre la aceptación y el cobro
  alteraría un precio ya pactado.
- **Dejar que el proveedor confirme la finalización.** Habilitaría reseñas verificadas sin que el
  comprador reconozca el servicio.
