# Domo lender send queue

Fecha: 2026-04-19

## Delivery status

- 2026-04-18: Banco Pichincha public WhatsApp outreach was delivered through OpenClaw to `+593962992999`.
- Confirmed delivery metadata: `messageId=3EB0BC2D72A97C56DDFE0D`, `runId=711e732a-820a-4fac-825b-114ea23320c9`, `toJid=593962992999@s.whatsapp.net`.
- Pichincha replied with an automated assistant that says it cannot understand specific text or additional images, then asked to choose an identification type to authenticate.
- OpenClaw auto-replied twice before the bank number was removed from the WhatsApp allowlist. Pichincha's bot rejected the free text and repeated the identification menu, so the thread should not be used for the loan packet.
- Next practical action: follow up through Pichincha PYME phone or branch and send `email-pichincha.eml` with `../00-resumen/domo-public-lender-packet.zip` only through a secure lender-confirmed channel. Do not send cedula, RUC, tax filings, title documents, bank statements, or other sensitive documents in the bot chat.
- Produbanco remains ready for manual phone/email intake because the public PYME page lists phone support but not a secure document-upload channel.
- 2026-04-19: The public-safe lender ZIP was rebuilt with a cover letter, bank submission index, corrected USD 100,000 use-of-funds allocation, 36-month pro forma, use-of-funds schedule, and lender comparison table.

## Ready to send first

1. Banco Pichincha
2. Produbanco

Official starting points:

- Banco Pichincha PYME credits: https://www.pichincha.com/detalle-catalogo/pymes-creditos
- Banco Pichincha productivo empresas: https://www.pichincha.com/portal/principal/empresas/creditos/productivo
- Banco Pichincha banca telefonica: `(02) 2999 999`
- Produbanco activos fijos PYME: https://www.produbanco.com.ec/pymes/activos-fijos/
- Produbanco capital de trabajo PYME: https://www.produbanco.com.ec/pymes/capital-de-trabajo/
- Produbanco phone listed on its PYME page: 02 400 9000, option 5.

Phone/branch script:

- `phone-intake-script.md`

Attach the public-safe packet:

- `../00-resumen/domo-submission-cover-letter.pdf`
- `../00-resumen/domo-bank-submission-index.pdf`
- `../00-resumen/domo-lender-one-pager.pdf`
- `../00-resumen/domo-del-pululahua-financial-plan.pdf`
- `../00-resumen/domo-36-month-pro-forma.pdf`
- `../04-proformas-uso-de-fondos/domo-use-of-funds-schedule.pdf`

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
| 1 | Banco Pichincha | `email-pichincha.eml` | WhatsApp bot loop; secure intake needed |
| 2 | Produbanco | `email-produbanco.eml` | Ready to send |
| 3 | Banco Guayaquil | `lender-outreach-drafts.md` | Drafted |
| 4 | Banco Internacional | `lender-outreach-drafts.md` | Drafted |
| 5 | Jardin Azuayo | `lender-outreach-drafts.md` | Drafted |
| 6 | JEP | `lender-outreach-drafts.md` | Drafted |
| 7 | Cooprogreso | `lender-outreach-drafts.md` | Drafted |
| 8 | Cooperativa 29 de Octubre | `lender-outreach-drafts.md` | Drafted |
