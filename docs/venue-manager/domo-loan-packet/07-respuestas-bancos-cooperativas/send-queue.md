# Domo lender send queue

Fecha: 2026-04-18

## Delivery status

- 2026-04-18: Banco Pichincha public WhatsApp outreach was attempted through OpenClaw after adding the public WhatsApp number to the account allowlist. Delivery was not confirmed; the gateway HTTP service and WhatsApp listener came up, but gateway RPC timed out before returning a message id.
- Next practical action: send `email-pichincha.eml` with `../00-resumen/domo-public-lender-packet.zip` from the real TDF sender mailbox, or retry WhatsApp after OpenClaw gateway RPC is healthy.
- Produbanco remains ready for manual phone/email intake because the public PYME page lists phone support but not a secure document-upload channel.

## Ready to send first

1. Banco Pichincha
2. Produbanco

Official starting points:

- Banco Pichincha PYME credits: https://www.pichincha.com/detalle-catalogo/pymes-creditos
- Banco Pichincha productivo empresas: https://www.pichincha.com/portal/principal/empresas/creditos/productivo
- Produbanco activos fijos PYME: https://www.produbanco.com.ec/pymes/activos-fijos/
- Produbanco capital de trabajo PYME: https://www.produbanco.com.ec/pymes/capital-de-trabajo/
- Produbanco phone listed on its PYME page: 02 400 9000, option 5.

Attach the public-safe packet:

- `../00-resumen/domo-lender-one-pager.pdf`
- `../00-resumen/domo-del-pululahua-financial-plan.pdf`
- `../00-resumen/domo-36-month-pro-forma.pdf`

Or attach the ZIP:

- `../00-resumen/domo-public-lender-packet.zip`

## Before sending

- Add real sender name, phone, and email in the mail client.
- Confirm whether the recipient is a bank executive, branch email, web form, or WhatsApp business channel.
- Do not send RUC, cedula, bank statements, tax filings, title documents, or property documents until the lender confirms a secure intake channel.
- Record the sent date and contact in `lender-outreach-tracker.csv`.

## Send order

| Order | Institution | Draft | Status |
| ---: | --- | --- | --- |
| 1 | Banco Pichincha | `email-pichincha.eml` | WhatsApp not delivered; email ready |
| 2 | Produbanco | `email-produbanco.eml` | Ready to send |
| 3 | Banco Guayaquil | `lender-outreach-drafts.md` | Drafted |
| 4 | Banco Internacional | `lender-outreach-drafts.md` | Drafted |
| 5 | Jardin Azuayo | `lender-outreach-drafts.md` | Drafted |
| 6 | JEP | `lender-outreach-drafts.md` | Drafted |
| 7 | Cooprogreso | `lender-outreach-drafts.md` | Drafted |
| 8 | Cooperativa 29 de Octubre | `lender-outreach-drafts.md` | Drafted |
