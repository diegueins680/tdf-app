# ADR 0136: Pagos de acuerdos del marketplace (Fase 2, parte B)

Estado: propuesto; diseño aprobado por el dueño del producto el 2026-11-12, implementación en curso.
Extiende ADR 0135. Se apoya en ADR 0115 (ruteo neutral de proveedores) y ADR 0126 (gates de
capacidad del checkout completo); no los modifica.

## Contexto

La parte A (ADR 0135) crea el acuerdo con comisión (10 %) y depósito (30 %) congelados, pero nace
`confirmed` y `off_platform`: TDF no cobra nada. El dueño decidió el 12 de noviembre de 2026:

1. Cobrar con Stripe, PayPhone y DataWeb (Datafast), según la moneda, el país y la elección del
   usuario.
2. El proveedor del servicio recibe su dinero de forma automática, no por transferencia manual.
3. DataWeb queda integrado pero cerrado hasta que Datafast contrate y se verifique un split.
4. El reparto ocurre en cada cobro: del depósito TDF toma su comisión y el resto va al proveedor;
   el saldo va íntegro al proveedor.

Hechos verificados antes de diseñar (fuentes al final):

- ADR 0115 ya exige, para todo flujo `marketplace`, evidencia verificada de `connected_accounts`,
  `split_settlement` y `seller_payouts`, y rechaza que TDF retenga saldos del vendedor para pagarlos
  a mano. La decisión 2 del dueño coincide con esa regla; no hay que cambiarla.
- Stripe: el repo ya crea cuentas Express y cargos destino (propinas a artistas, PAY-TIP-001), pero
  Stripe no tiene perfil en el ruteo canónico y el checkout Stripe del marketplace está deshabilitado
  «hasta usar el runtime canónico». Stripe no ofrece cuentas conectadas autoservicio en Ecuador.
- PayPhone: documenta «split de pagos» (`transferTo`, arreglo JSON cifrado con AES-256-CBC y la
  `codingPassword` de la tienda). Requiere autorización de PayPhone por tienda (3–5 días hábiles) y
  que cada receptor sea usuario PayPhone (identificación tipo 3: RUC/CI; tipo 4: teléfono). El split
  inmediato no se revierte automáticamente.
- Datafast/DataWeb: no documenta split ni pagos a terceros.

## Decisión

1. **El cobro usa el checkout canónico.** Cada cobro de un acuerdo es una fila
   `marketplace_agreement_payment` (`kind` = `deposit` | `balance`) con su
   `commerce_checkout_session` de `domain_type='marketplace_agreement'` y
   `domain_order_id` = id del cobro. `productFlowForDomain` lo mapea a `FlowMarketplace`, de modo
   que el ruteo existente solo ofrece un proveedor cuando su cuenta, contrato, credenciales y las
   tres capacidades de marketplace están verificadas para ese método y entorno. Ningún módulo del
   marketplace llama a un proveedor directamente.
2. **Reparto congelado por cobro.** Al crear el cobro se guarda `platform_fee_minor`,
   `provider_amount_minor` y `processing_fee_minor`:
   - depósito: `platform_fee = min(commission_minor, deposit_minor)`;
   - saldo: `platform_fee = commission_minor − lo que los cobros pagados ya recaudaron para la
     plataforma` (0 con la tarifa por defecto; más que el resto nominal cuando la comisión del
     procesador se comió parte de un depósito menor que la comisión);
   - `provider_amount = monto − platform_fee − processing_fee`, nunca negativo.
   `processing_fee` sale de `marketplace_payment_fee_rule` (por proveedor: bps + fijo + quién la
   asume). Por defecto la asume el proveedor del servicio, porque en el cobro del saldo TDF no
   recibe nada de lo que descontarla. La regla es editable sin desplegar.
3. **Cuenta de cobro del proveedor.** `marketplace_payout_account` (perfil, proveedor, referencia,
   estado). PayPhone: el proveedor declara su identificación (tipo 3) o teléfono (tipo 4). Stripe:
   cuenta Express con onboarding alojado (reutiliza `Services.Stripe`). Sin una cuenta lista para
   algún proveedor ofrecible en la moneda del acuerdo, el acuerdo nace como en la parte A
   (`off_platform`, `confirmed`); no se bloquea la aceptación.
4. **Estados.** Con el gate `marketplace.agreement_payments` activo y cuenta de cobro lista, el
   acuerdo nace `pending_deposit` + `platform`, con `deposit_due_at` según la tarifa.
   - Depósito verificado → `confirmed`.
   - Depósito sin pagar al vencer → `expired`: retira la oferta y libera el cupo (misma rutina
     que cancelar).
   - El proveedor inicia (`in_progress`). El comprador confirma la entrega: con saldo pendiente
     pasa a `balance_due` y se crea el cobro del saldo; el saldo verificado lo lleva a
     `completed` y registra la interacción verificada. Sin saldo, pasa directo a `completed`.
   - Ningún participante ni administrador puede marcar un cobro como pagado: solo la
     reconciliación con consulta autenticada al proveedor (trigger
     `marketplace_agreement_payment_require_verified_payment`, mismo patrón que reservas y
     entradas).
5. **Proveedores.**
   - **PayPhone:** el adaptador añade `transferTo` cifrado cuando el checkout trae reparto. Queda
     cerrado hasta que PayPhone autorice el split para la tienda y se registre la evidencia de
     capacidad; se exige la modalidad «al día siguiente» para conservar el reverso del primer día.
   - **Stripe:** adaptador canónico con Checkout Session alojada y cargo destino
     (`transfer_data.destination`, `application_fee_amount`); se entrega en un segundo PR (B2) junto
     con reembolsos y panel de disputas, porque incorpora Stripe al runtime canónico.
   - **DataWeb:** el dominio queda conectado al ruteo, y por tanto cerrado: no hay capacidad de
     split verificable. Se abre registrando evidencia cuando Datafast la contrate, sin código nuevo
     en el marketplace.
6. **Cancelación con dinero cobrado.** En B1, un acuerdo `platform` con depósito pagado no se
   cancela por los participantes, ni directamente ni retirando la oferta aceptada: abre disputa y
   un administrador resuelve. Tampoco se cancela ni se retira mientras un intento de pago pueda
   cobrar todavía. El reembolso
   automático según la política congelada (`cancellation_policy`) llega en B2, cuando exista la
   reversión del reparto por proveedor.
7. **Orden de bloqueo.** anuncio → oferta → acuerdo → cobro. La reconciliación bloquea checkout e
   intento primero (regla existente) y después el cobro y el acuerdo; ningún camino del marketplace
   toma un bloqueo de checkout mientras sostiene el acuerdo, salvo la creación del checkout, que es
   una inserción.

## Alternativas descartadas

- Cobrar todo a la cuenta de TDF y transferir a mano: lo prohíbe ADR 0115 (custodia).
- Retener hasta completar (escrow): solo Stripe lo permite; dejaría fuera a PayPhone.
- Llamar a Stripe desde el módulo del marketplace como en las propinas: repetiría el camino que el
  repo deshabilitó para el marketplace.
- Bloquear la aceptación si el proveedor no tiene cuenta de cobro: frena la negociación por un
  trámite externo; el acuerdo fuera de plataforma sigue siendo válido.

## Consecuencias

- Nada se cobra hasta que cada proveedor esté contratado y verificado; el gate
  `marketplace.agreement_payments` es FALSE en producción.
- El proveedor recibe su parte del depósito antes de prestar el servicio. Si el acuerdo se cae,
  recuperar esa parte depende de la reversión del proveedor de pagos (B2) o de la disputa.
- La comisión de procesamiento reduce lo que recibe el proveedor del servicio salvo que se cambie
  la regla; la interfaz debe mostrar el neto antes de aceptar.
- Entregas: B1 (este diseño menos Stripe y reembolsos) y B2 (Stripe canónico, reembolsos, panel de
  disputas).

## Fuentes (consultadas 2026-11-12)

- PayPhone, Implementación de split: https://docs.payphone.app/split-de-pagos
- Stripe, Cross-border payouts: https://docs.stripe.com/connect/cross-border-payouts
- Datafast, guía de integración: https://developers.datafast.com.ec/index.aspx
