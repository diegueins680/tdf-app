# Banco Pichincha WhatsApp response

Date: 2026-04-18

Outbound:

- Channel: WhatsApp via OpenClaw
- To: `+593962992999`
- Message ID: `3EB0BC2D72A97C56DDFE0D`
- Run ID: `711e732a-820a-4fac-825b-114ea23320c9`
- JID: `593962992999@s.whatsapp.net`

Inbound summary:

- Banco Pichincha's public WhatsApp responded with an automated assistant.
- The assistant says it will not ask for confidential information such as passwords, that the user must be the account or card holder, and that it does not understand specific questions or additional text/images.
- The follow-up message asks the user to choose an identification type to authenticate.
- OpenClaw sent two automated replies after the bot response. Pichincha's bot rejected the free text and repeated the menu.

Decision:

- Do not continue the business-loan packet intake inside this bot chat.
- Use Pichincha's PYME phone or branch channel to obtain a named advisor or secure upload/email destination.
- Send the public-safe packet first. Send personal/company identifiers, tax filings, title documents, bank statements, or permits only through a secure lender-confirmed channel.
- The Pichincha WhatsApp number was removed from OpenClaw's WhatsApp allowlist after this exchange to prevent further automatic replies.
