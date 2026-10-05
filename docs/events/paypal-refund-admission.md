# PayPal external refund admission fence

A signed `PAYMENT.CAPTURE.REFUNDED` event represents a **refund** resource. Its
`id` is not the original capture ID and its shape need not include capture-only
payee or order fields. Retain the bounded upstream link during inbox evidence
minimization and require the configured PayPal environment, exact provider host,
GET method and an unambiguous capture identifier. No response link is fetched.
Bind that capture to the existing merchant, checkout and payment attempt before
recording a reconciliation exception. Reversals bind their capture resource ID.
Currency and positive amount must fit the bound capture.

For event tickets, recording the external change locks the event and order in
the same order used by scanning. Admission rejects an unresolved refund/reversal
exception only when provider, environment, merchant, capture and internal order
all match the purchased checkout. A stale legacy paid/issued row cannot override
this fence. Existing reconciliation resolution remains an authorized operation;
unknown, ignored and other-merchant events do not establish this binding.

This is an admission suspension pending reconciliation. It does **not** invent
which ticket a partial external refund represents, restock inventory, mark a
refund completed, issue a credit note or implement the missing PayPal refund
approval path for ticket orders. Those financial transitions remain separate
release requirements; the event must not be activated on this safeguard alone.

Regression coverage: actual PostgreSQL inventory/admission/transfer harness,
including a bound refund and reversal versus another merchant, no admission
audit on denial, the last one/two places under eight buyers, and one redemption
under eight scans. Parser tests cover real refund-shaped resources after inbox
minimization, foreign environment/host and malformed capture URLs. Full provider
purchase/refund is a separate sandbox check, never inferred from these tests.

Sources: [PayPal payment/refund API](https://developer.paypal.com/api/payments/v2)
and [official webhook events](https://developer.paypal.com/api/rest/webhooks/event-names/).
